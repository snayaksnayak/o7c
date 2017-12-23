//Oberon7 code generator
//Based on Niklaus Wirth's implementation
//Compile using gcc
//$ gcc -Wall obs.c obt.c obg.c obp.c obc.c

//This code is distributed under the GPL License.
//For more info, check: http://www.gnu.org/copyleft/gpl.html

//15 June 2016: Srinivas Nayak: This file created
//24 June 2016: Srinivas Nayak: Coding started
//10 July 2016: Srinivas Nayak: Code updated with NW's version of 04 July 2016

//Code generator for Oberon7 compiler for x86 processor.
//Procedural interface to Parser; result in array "code".
//Procedure Close writes code-files.

//logical shift left = arithmetic shift left
//logical shift right # arithmetic shift right
//arithmetic shift right inserts sign bit from left side
//other three shifts inserts 0

//C shift operator << on unsigned type is logical shift left
//C shift operator << on signed type is arithmetic shift left
//that means << is indifferent on unsigned and signed type
//C shift operator >> on unsigned type is logical shift right
//C shift operator >> on signed type is arithmetic shift right

//Oberon MUL is equivalent to C logical shift left
//Oberon DIV is equivalent to C arithmetic shift right
//Oberon MOD is equivalent to C bitwise AND

#include "obc.h"
//maximum size of generated code
#define maxCode 8000
//maximum size of string literals
#define maxStrx 2400
//maximum size of type descriptors
#define maxTD 120

int WordSize = 4,
    StkOrg0 = -64, VarOrg0 = 0, //for RISC-0 only
    MT = 12, SB = 13, SP = 14, LNK = 15, //dedicated registers
    C24 = 0x1000000, //24 bit addresses for code
    Reg = 10, RegI = 11, Cond = 12,  //item modes, class values are from 0 to 8

//frequently used opcodes

	//these two sets mode
    U = 0x2000, V = 0x1000,

    //bitwise operations
    Mov = 0, Lsl = 1, Asr = 2, Ror= 3,
    //logical operations
    And = 4, Ann = 5, Ior = 6, Xor = 7,
    //integer arithmetic operations
    Add = 8, Sub = 9, Cmp = 9, Mul = 10, Div = 11,
    //floating point arithmetic operations
    Fad = 12, Fsb = 13, Fml = 14, Fdv = 15,

    //load and store operations
    Ldr = 8, Str = 10,

    //branching operations
    BR = 0, BLR = 1, BC = 2, BL = 3,

    //condition codes
    MI = 0, PL = 8, //to denote positive or negative number
    EQ = 1, NE = 9, //only six relational operators exist in language
    //CS, CC //so these are not used
    //VS, VC
    //LS, HI
    LT = 5, GE = 13,
    LE = 6, GT = 14;

int version;  //0 = RISC-0, 1 = RISC-5
int entry;   //module entry point
int RH;  //available registers R[0] ... R[H-1]
int curSB;  //current static base in SB
int frame;  //frame offset changed in SaveRegs and RestoreRegs
int fixorgP, fixorgD, fixorgT;   //origins of lists of locations to be fixed up by loader
int check;  //emit run-time checks

int relmap[6];  //condition codes for relations
int code[maxCode]; //contains generated code
int pc; //program counter
int data[maxTD];//contains type descriptors
int tdx; //global type descriptor counter
int varsize; //data index; holds whole size of total variables declared in a module, which gets its value from dc of OBP
char _str[maxStrx]; //coniains all string literals
int strx; //global string length counter

//emit Format0 instructions
void Put0(int op, int a, int b, int c)
{
    //emit format-0 instruction
    code[pc] = ((a*0x10 + b) * 0x10 + op) * 0x10000 + c;
    pc++;
    printf("\npos = %d", pos());
    printf("\nPut0: op=%d a=%d b=%d c=%d", op, a, b, c);
    printf("\n%#x\n", code[pc-1]);
}

//emit Format1 instructions when im is 16bit, i.e. -0x10000 <= im < 0x10000
void Put1(int op, int a, int b, int im)
{
    if( im < 0 )
    {
        op = op + V;
    }
    code[pc] = (((a+0x40) * 0x10 + b) * 0x10 + op) * 0x10000 + (im & 0xFFFF);
    pc++;
    printf("\npos = %d", pos());
    printf("\nPut1: op=%d a=%d b=%d im=%d", op, a, b, im);
    printf("\n%#x\n", code[pc-1]);
}

//emit Format1 instructions when im is 32bit
void Put1a(int op, int a, int b, int im)
{
    //if im is 16bit, i.e. -0x10000H <= im < 0x10000
    if( (im >= -0x10000) && (im < 0x10000) )
    {
        Put1(op, a, b, im);
    }
    else //if im is 32bit
    {
        Put1(Mov+U, RH, 0, im >> 16); //MSBs of RH = MSBs of im
        if( (im & 0xFFFF) != 0 ) //if there is any LSB
        {
            Put1(Ior, RH, RH, im & 0xFFFF); //RH = RH | LSBs of im
        }
        Put0(op, a, b, RH); //Ra = Rb op RH
    }
}

//emit load/store Format2 instruction
void Put2(int op, int a, int b, int off)
{
    code[pc] = ((op * 0x10 + a) * 0x10 + b) * 0x100000 + (off & 0xFFFFF);
    pc++;
    printf("\npos = %d", pos());
    printf("\nPut2: op=%d a=%d b=%d off=%d", op, a, b, off);
    printf("\n%#x\n", code[pc-1]);
}

//emit branch instruction
void Put3(int op, int cond, int off)
{
    code[pc] = ((op+12) * 0x10 + cond) * 0x1000000 + (off & 0xFFFFFF);
    pc++;
    printf("\npos = %d", pos());
    printf("\nPut3: op=%d cond=%d off=%d", op, cond, off);
    printf("\n%#x\n", code[pc-1]);
}

//increament register stack top.
//R0-R11 is treated as a register stack
//R12-R15 are special purpose registers
//MT = 12, SB = 13, SP = 14, LNK = 15
void incR()
{
    if( RH < MT-1 )
    {
        RH++;
    }
    else
    {
        Mark("register stack overflow");
    }
}

//used in StatSequence() after consuming one statement
void CheckRegs()
{
    if( RH != 0 )
    {
        Mark("Reg Stack");
        RH = 0;
    }
    if( pc >= maxCode - 40 )
    {
        Mark("program too long");
    }
}

//makes a Cond type Item
void SetCC(Item *x, int n)
{
    x->mode = Cond;
    x->a = 0;
    x->b = 0;
    x->r = n;
}

//Module Table address is at MT
void Trap(int cond, int num)
{
    Put3(BLR, cond, pos()*0x100 + num*0x10 + MT);
}

//handling of forward reference, fixups of branch addresses and constant tables

//MI becomes PL, EQ becomes NE
int negated(int cond)
{
    if( cond < 8 )
    {
        cond = cond+8;
    }
    else
    {
        cond = cond-8;
    }
    return cond;
}

//invalidate Static Base, which is base address of module variables
void invalSB()
{
    curSB = 1;
}

//fix offset at 'code[at]' with 'with'
void fix(int at, int with)
{
    code[at] = ((code[at] >> 24) * C24) + (with & 0xFFFFFF);
}

void FixLink(int L)
{
    int L1;
    invalSB();
    while( L != 0 ) //in some cases we need loop
    {
        L1 = code[L] & 0x3FFFF; //why? only 18bits being extracted? RAM size 1MB, addressable by 20bits; branch instruction offset is in words, so 2 bits less.
        fix(L, pc-L-1);
        L = L1;
    }
}

void FixLinkWith(int L0, int dst)
{
    int L1;

    while( L0 != 0 )
    {
        L1 = code[L0] & 0xFFFFFF;
        code[L0] = ((code[L0] >> 24) * C24) + ((dst - L0 - 1) & 0xFFFFFF);
        L0 = L1;
    }
}

int merged(int L0, int L1)
{
    int L2, L3;

    if( L0 != 0 )
    {
        L3 = L0;
        do
        {
            L2 = L3;
            L3 = code[L2] & 0x3FFFF;
        }
        while(!( L3 == 0 ));
        code[L2] = code[L2] + L1;
        L1 = L0;
    }
    return L1;
}

// loading of operands and addresses into registers

void GetSB(int base)
{
    if( (version != 0) && ((base != curSB) || (base != 0)) )
    {
        Put2(Ldr, SB, -base, pc-fixorgD);
        fixorgD = pc-1;
        curSB = base;
    }
}

//segfault
void NilCheck()
{
    if( check )
    {
        Trap(EQ, 4);
    }
}

void load(Item* x)
{
    int op;

    if( x->type->size == 1 )
    {
        op = Ldr+1; //load byte
    }
    else
    {
        op = Ldr; //load word
    }

    if( x->mode != Reg )
    {
        if( x->mode == Const )
        {
            if( x->type->form == Proc )
            {
                if( x->r > 0 )
                {
                    Mark("not allowed");
                }
                else if( x->r == 0 )
                {
                    Put3(BL, 7, 0);
                    Put1a(Sub, RH, LNK, pc*4 - x->a);
                }
                else
                {
                    GetSB(x->r);
                    Put1(Add, RH, SB, x->a + 0x100);//mark as progbase-relative
                }
            }
            else if( (x->a <= 0x0FFFF) && (x->a >= -0x10000) )
            {
                Put1(Mov, RH, 0, x->a);
            }
            else
            {
                Put1(Mov+U, RH, 0, (x->a >> 16) & 0xFFFF);
                if( (x->a & 0xFFFF) != 0 )
                {
                    Put1(Ior, RH, RH, x->a & 0xFFFF);
                }
            }
            x->r = RH;
            incR();
        }
        else if( x->mode == Var )
        {
            if( x->r > 0 )//local
            {
                Put2(op, RH, SP, x->a + frame);
            }
            else
            {
                GetSB(x->r);
                Put2(op, RH, SB, x->a);
            }
            x->r = RH;
            incR();
        }
        else if( x->mode == Par )
        {
            Put2(Ldr, RH, SP, x->a + frame);
            Put2(op, RH, RH, x->b);
            x->r = RH;
            incR();
        }
        else if( x->mode == RegI )
        {
            Put2(op, x->r, x->r, x->a);
        }
        else if( x->mode == Cond )
        {
            Put3(BC, negated(x->r), 2);
            FixLink(x->b);
            Put1(Mov, RH, 0, 1);
            Put3(BC, 7, 1);
            FixLink(x->a);
            Put1(Mov, RH, 0, 0);
            x->r = RH;
            incR();
        }

        x->mode = Reg;
    }
}

void loadAdr(Item* x)
{
    if( x->mode == Var )
    {
        if( x->r > 0 )//local
        {
            Put1a(Add, RH, SP, x->a + frame);
        }
        else
        {
            GetSB(x->r);
            Put1a(Add, RH, SB, x->a);
        }
        x->r = RH;
        incR();
    }
    else if( x->mode == Par )
    {
        Put2(Ldr, RH, SP, x->a + frame);
        if( x->b != 0 )
        {
            Put1a(Add, RH, RH, x->b);
        }
        x->r = RH;
        incR();
    }
    else if( x->mode == RegI )
    {
        if( x->a != 0 )
        {
            Put1a(Add, x->r, x->r, x->a);
        }
    }
    else
    {
        Mark("address error");
    }
    x->mode = Reg;
}

void loadCond(Item* x)
{
    if( x->type->form == Bool )
    {
        if( x->mode == Const )
        {
            x->r = 15 - x->a*8;
        }
        else
        {
            load(x);
            if( (code[pc-1] >> 30) != -2 )
            {
                Put1(Cmp, x->r, x->r, 0);
            }
            x->r = NE;
            RH--;
        }
        x->mode = Cond;
        x->a = 0;
        x->b = 0;
    }
    else
    {
        Mark("not Boolean?");
    }
}

void loadTypTagAdr(Type T)
{
    Item x;
    x.mode = Var;
    x.a = T->len;
    x.r = -T->mno;
    loadAdr(&x);
}

void loadStringAdr(Item* x)
{
    GetSB(0);
    Put1a(Add, RH, SB, varsize+x->a);
    x->mode = Reg;
    x->r = RH;
    incR();
}

// Items: Conversion from literals or from Objects on the Heap to Items on the Stack
// makes item for INT, CHAR, NIL, FALS, TRU symbols/literals
void MakeConstItem(Item* x, Type typ, int val)
{
    x->mode = Const;
    x->type = typ;
    x->a = val;
}

//makes item for REAL symbols/literals
void MakeRealItem(Item* x, float val)
{
    union
    {
        float f;
        int i;
    } u;
    u.f = val;

    x->mode = Const;
    x->type = realType;
    x->a = u.i; //equivalent to: x->a = val
				//type casting from float to int will truncate the value
				//so union trick is used to put a float value unchanged into int variable
}

//makes item for constant string literals
//copies string from ORS-buffer str[] to ORG-string array _str[]
void MakeStringItem(Item* x, int len)
{
    int i;
    x->mode = Const;
    x->type = strType;
    x->a = strx; //address of this string constant
    x->b = len; //length of this string constant as given by ORS, which covers '\0'
    i = 0;
    if( strx + len + 4 < maxStrx )
    {
        while( len > 0 ) //copy the string constant and the trailing '\0' as well
        {
            _str[strx] = str[i];
            strx++;
            i++;
            len--;
        }
        while( strx % 4 != 0 ) //make strx 4 byte aligned
        {
            _str[strx] = '\0';
            strx++;
        }
    }
    else
    {
        Mark("too many strings");
    }
}

void MakeItem(Item* x, Object y, int curlev)
{
    x->mode = y->class;
    x->type = y->type;
    x->a = y->val;
    x->rdo = y->rdo;
    if( y->class == Par )
    {
        x->b = 0;
    }
    else if( y->class == Typ )
    {
        x->a = y->type->len;
        x->r = -y->lev;
    }
    else if( (y->class == Const) && (y->type->form == String) )
    {
        x->b = y->lev; //lev abused for string length
    }
    else
    {
        x->r = y->lev;
    }

    if( (y->lev > 0) && (y->lev != curlev) && (y->class != Const) )
    {
        Mark("level error, not accessible");
    }
}

// Code generation for Selectors, Variables, Constants

void Field(Item* x, Object y) // x := x.y
{
    if( x->mode == Var )
    {
        if( x->r >= 0 )
        {
            x->a = x->a + y->val;
        }
        else
        {
            loadAdr(x);
            x->mode = RegI;
            x->a = y->val;
        }
    }
    else if( x->mode == RegI )
    {
        x->a = x->a + y->val;
    }
    else if( x->mode == Par )
    {
        x->b = x->b + y->val;
    }
}

void Index(Item* x, Item* y) //x := x[y]
{
    int s, lim;
    s = x->type->base->size;
    lim = x->type->len;
    if( (y->mode == Const) && (lim >= 0) )
    {
        if( (y->a < 0) || (y->a >= lim) )
        {
            Mark("bad index");
        }
        if( x->mode == Var || x->mode == RegI )
        {
            x->a = y->a * s + x->a;
        }
        else if( x->mode == Par )
        {
            x->b = y->a * s + x->b;
        }
    }
    else
    {
        load(y);
        if( check )//check array bounds
        {
            if( lim >= 0 )
            {
                Put1a(Cmp, RH, y->r, lim);
            }
            else //open array
            {
                if( x->mode == Var || x->mode == Par )
                {
                    Put2(Ldr, RH, SP, x->a+4+frame);
                    Put0(Cmp, RH, y->r, RH);
                }
                else
                {
                    Mark("error in Index");
                }
            }
            Trap(10, 1); //BCC
        }
        if( s == 4 )
        {
            Put1(Lsl, y->r, y->r, 2);
        }
        else if( s > 1 )
        {
            Put1a(Mul, y->r, y->r, s);
        }
        if( x->mode == Var )
        {
            if( x->r > 0 )
            {
                Put0(Add, y->r, SP, y->r);
                x->a = x->a + frame;
            }
            else
            {
                GetSB(x->r);
                if( x->r == 0 )
                {
                    Put0(Add, y->r, SB, y->r);
                }
                else
                {
                    Put1a(Add, RH, SB, x->a);
                    Put0(Add, y->r, RH, y->r);
                    x->a = 0;
                }
            }
            x->r = y->r;
            x->mode = RegI;
        }
        else if( x->mode == Par )
        {
            Put2(Ldr, RH, SP, x->a + frame);
            Put0(Add, y->r, RH, y->r);
            x->mode = RegI;
            x->r = y->r;
            x->a = x->b;
        }
        else if( x->mode == RegI )
        {
            Put0(Add, x->r, x->r, y->r);
            RH--;
        }
    }
}

void DeRef(Item* x)
{
    if( x->mode == Var )
    {
        if( x->r > 0 ) //local
        {
            Put2(Ldr, RH, SP, x->a + frame);
        }
        else
        {
            GetSB(x->r);
            Put2(Ldr, RH, SB, x->a);
        }
        NilCheck();
        x->r = RH;
        incR();
    }
    else if( x->mode == Par )
    {
        Put2(Ldr, RH, SP, x->a + frame);
        Put2(Ldr, RH, RH, x->b);
        NilCheck();
        x->r = RH;
        incR();
    }
    else if( x->mode == RegI )
    {
        Put2(Ldr, x->r, x->r, x->a);
        NilCheck();
    }
    else if( x->mode != Reg )
    {
        Mark("bad mode in DeRef");
    }
    x->mode = RegI;
    x->a = 0;
    x->b = 0;
}


void Q(Type T, int *dcw)
{
    //one entry of type descriptor extension table
    if( T->base != 0 )
    {
        Q(T->base, dcw);
        data[*dcw] = (T->mno*0x1000 + T->len) * 0x1000 + *dcw - fixorgT;
        fixorgT = *dcw;
        (*dcw)++;
    }
}

void FindPtrFlds(Type typ, int off, int *dcw)
{
    Object fld;
    int i, s;
    if( (typ->form == Pointer) || (typ->form == NilTyp) )
    {
        data[*dcw] = off;
        (*dcw)++;
    }
    else if( typ->form == Record )
    {
        fld = typ->dsc;
        while( fld != 0 )
        {
            FindPtrFlds(fld->type, fld->val + off, dcw);
            fld = fld->next;
        }
    }
    else if( typ->form == Array )
    {
        s = typ->base->size;
        for(i=0; i<=typ->len-1; i++)
        {
            FindPtrFlds(typ->base, i*s + off, dcw);
        }
    }
}

void BuildTD(Type T, int* dc)
{
    int dcw, k, s;
	//set len
    T->len = *dc;
	//s needs to be 32, 64, 128, 256 byte chunks
    //s assumes 8 byte overhead for type extension and garbage collector
    s = T->size;
    if( s <= 24 )
    {
        s = 32;
    }
    else if( s <= 56 )
    {
        s = 64;
    }
    else if( s <= 120 )
    {
        s = 128;
    }
    else
    {
        s = (s+263) / 256 * 256; //two 256 byte chunks may be required if s=249 + 8 byte overhead
    }

    dcw = *dc / 4; //convert size for heap allocation, dcw = word address
    data[dcw] = s; //len used as address
    dcw++;

    k = T->nofpar; //extension level!
    if( k > 3 )
    {
        Mark("ext level too large");
    }
    else
    {
        Q(T, &dcw);
        while( k < 3 )
        {
            data[dcw] = -1;
            dcw++;
            k++;
        }
    }
    FindPtrFlds(T, 0, &dcw);
    data[dcw] = -1;
    dcw++;
    tdx = dcw;
    *dc = dcw*4;

    if( tdx >= maxTD )
    {
        Mark("too many record types");
        tdx = 0;
    }
}

void _TypeTest(Item* x, Type T, int varpar, int isguard)
{
    int pc0;
    //fetch tag into RH
    if( varpar )
    {
        Put2(Ldr, RH, SP, x->a+4+frame);
    }
    else
    {
        load(x);
        pc0 = pc;
        Put3(BC, EQ, 0); //NIL belongs to every pointer type
        Put2(Ldr, RH, x->r, -8);
    }
    Put2(Ldr, RH, RH, T->nofpar*4);
    incR();
    loadTypTagAdr(T); //tag of T
    Put0(Cmp, RH-1, RH-1, RH-2);
    RH = RH - 2;
    if( !varpar )
    {
        fix(pc0, pc - pc0 - 1);
    }
    if( isguard )
    {
        if( check )
        {
            Trap(NE, 2);
        }
    }
    else
    {
        SetCC(x, EQ);
        if( !varpar )
        {
            RH--;
        }
    }
}

//Code generation for Boolean operators

void Not(Item* x)   //x := ~x
{
    int t;

    if( x->mode != Cond )
    {
        loadCond(x);
    }
    x->r = negated(x->r);
    t = x->a;
    x->a = x->b;
    x->b = t;
}

void And1(Item* x)   //x := x &
{
    if( x->mode != Cond )
    {
        loadCond(x);
    }
    Put3(BC, negated(x->r), x->a);
    x->a = pc-1;
    FixLink(x->b);
    x->b = 0;
}

void And2(Item* x, Item* y)
{
    if( y->mode != Cond )
    {
        loadCond(y);
    }
    x->a = merged(y->a, x->a);
    x->b = y->b;
    x->r = y->r;
}

void Or1(Item* x)   //x := x OR
{
    if( x->mode != Cond )
    {
        loadCond(x);
    }
    Put3(BC, x->r, x->b);
    x->b = pc-1;
    FixLink(x->a);
    x->a = 0;
}

void Or2(Item* x, Item* y)
{
    if( y->mode != Cond )
    {
        loadCond(y);
    }
    x->a = y->a;
    x->b = merged(y->b, x->b);
    x->r = y->r;
}

// Code generation for arithmetic operators

void Neg(Item* x)   // x := -x
{
    if( x->type->form == Int )
    {
        if( x->mode == Const )
        {
            x->a = -x->a;
        }
        else
        {
            load(x);
            Put1(Mov, RH, 0, 0);
            Put0(Sub, x->r, RH, x->r);
        }
    }
    else if( x->type->form == Real )
    {
        if( x->mode == Const )
        {
            x->a = x->a + 0x7FFFFFFF + 1;
        }
        else
        {
            load(x);
            Put1(Mov, RH, 0, 0);
            Put0(Fsb, x->r, RH, x->r);
        }
    }
    else //form = Set
    {
        if( x->mode == Const )
        {
            x->a = -x->a-1;
        }
        else
        {
            load(x);
            Put1(Xor, x->r, x->r, -1);
        }
    }
}

void AddOp(int op, Item* x, Item *y)   // x := x +- y
{
    if( op == PLUS )
    {
        if( (x->mode == Const) && (y->mode == Const) )
        {
            x->a = x->a + y->a;
        }
        else if( y->mode == Const )
        {
            load(x);
            if( y->a != 0 )
            {
                Put1a(Add, x->r, x->r, y->a);
            }
        }
        else
        {
            load(x);
            load(y);
            Put0(Add, RH-2, x->r, y->r);
            RH--;
            x->r = RH-1;
        }
    }
    else //op == MINUS
    {
        if( (x->mode == Const) && (y->mode == Const) )
        {
            x->a = x->a - y->a;
        }
        else if( y->mode == Const )
        {
            load(x);
            if( y->a != 0 )
            {
                Put1a(Sub, x->r, x->r, y->a);
            }
        }
        else
        {
            load(x);
            load(y);
            Put0(Sub, RH-2, x->r, y->r);
            RH--;
            x->r = RH-1;
        }
    }
}

int _log2(int m, int* e)
{
    *e = 0;
    while( m%2==0 )
    {
        m = m / 2;
        (*e)++;
    }
    return m;
}

void MulOp(Item* x, Item* y)   //x := x * y
{
    int e;

    if( (x->mode == Const) && (y->mode == Const) )
    {
        x->a = x->a * y->a;
    }
    else if( (y->mode == Const) && (y->a >= 2) && (_log2(y->a, &e) == 1) )
    {
        load(x);
        Put1(Lsl, x->r, x->r, e);
    }
    else if( y->mode == Const )
    {
        load(x);
        Put1a(Mul, x->r, x->r, y->a);
    }
    else if( (x->mode == Const) && (x->a >= 2) && (_log2(x->a, &e) == 1) )
    {
        load(y);
        Put1(Lsl, y->r, y->r, e);
        x->mode = Reg;
        x->r = y->r;
    }
    else if( x->mode == Const )
    {
        load(y);
        Put1a(Mul, y->r, y->r, x->a);
        x->mode = Reg;
        x->r = y->r;
    }
    else
    {
        load(x);
        load(y);
        Put0(Mul, RH-2, x->r, y->r);
        RH--;
        x->r = RH-1;
    }
}

void DivOp(int op, Item* x, Item* y)   // x := x op y
{
    int e;

    if( op == DIV )
    {
        if( (x->mode == Const) && (y->mode == Const) )
        {
            if( y->a > 0 )
            {
                x->a = x->a / y->a;
            }
            else
            {
                Mark("bad divisor");
            }
        }
        else if( (y->mode == Const) && (y->a >= 2) && (_log2(y->a, &e) == 1) )
        {
            load(x);
            Put1(Asr, x->r, x->r, e);
        }
        else if( y->mode == Const )
        {
            if( y->a > 0 )
            {
                load(x);
                Put1a(Div, x->r, x->r, y->a);
            }
            else
            {
                Mark("bad divisor");
            }
        }
        else
        {
            load(y);
            if( check )
            {
                Trap(LE, 6);
            }
            load(x);
            Put0(Div, RH-2, x->r, y->r);
            RH--;
            x->r = RH-1;
        }
    }
    else //op == MOD
    {
        if( (x->mode == Const) && (y->mode == Const) )
        {
            if( y->a > 0 )
            {
                x->a = x->a % y->a;
            }
            else
            {
                Mark("bad modulus");
            }
        }
        else if( (y->mode == Const) && (y->a >= 2) && (_log2(y->a, &e) == 1) )
        {
            load(x);
            if( e <= 16 )
            {
                Put1(And, x->r, x->r, y->a-1);
            }
            else
            {
                Put1(Lsl, x->r, x->r, 32-e);
                Put1(Ror, x->r, x->r, 32-e);
            }
        }
        else if( y->mode == Const )
        {
            if( y->a > 0 )
            {
                load(x);
                Put1a(Div, x->r, x->r, y->a);
                Put0(Mov+U, x->r, 0, 0);
            }
            else
            {
                Mark("bad modulus");
            }
        }
        else
        {
            load(y);
            if( check )
            {
                Trap(LE, 6);
            }
            load(x);
            Put0(Div, RH-2, x->r, y->r);
            Put0(Mov+U, RH-2, 0, 0);
            RH--;
            x->r = RH-1;
        }
    }
}

// Code generation for REAL operators

void RealOp(int op, Item* x, Item* y)   // x := x op y
{
    load(x);
    load(y);
    if( op == PLUS )
    {
        Put0(Fad, RH-2, x->r, y->r);
    }
    else if( op == MINUS )
    {
        Put0(Fsb, RH-2, x->r, y->r);
    }
    else if( op == MUL )
    {
        Put0(Fml, RH-2, x->r, y->r);
    }
    else if( op == RDIV )
    {
        Put0(Fdv, RH-2, x->r, y->r);
    }
    RH--;
    x->r = RH-1;
}

// Code generation for set operators

void Singleton(Item* x)  // x := {x}
{
    if( x->mode == Const )
    {
        x->a = (1 << x->a); //correct?
    }
    else
    {
        load(x);
        Put1(Mov, RH, 0, 1);
        Put0(Lsl, x->r, RH, x->r);
    }
}

void _Set(Item* x, Item* y)   // x := {x .. y}
{
    if( (x->mode == Const) && ( y->mode == Const) )
    {
        if( x->a <= y->a )
        {
            x->a = (2 << y->a) - (1 << x->a);
        }
        else
        {
            x->a = 0;
        }
    }
    else
    {
        if( (x->mode == Const) && (x->a <= 16) )
        {
            x->a = ((unsigned int)-1 << x->a);
        }
        else
        {
            load(x);
            Put1(Mov, RH, 0, -1);
            Put0(Lsl, x->r, RH, x->r);
        }
        if( (y->mode == Const) && (y->a < 16) )
        {
            Put1(Mov, RH, 0, ((unsigned int)-2 << y->a));
            y->mode = Reg;
            y->r = RH;
            incR();
        }
        else
        {
            load(y);
            Put1(Mov, RH, 0, -2);
            Put0(Lsl, y->r, RH, y->r);
        }
        if( x->mode == Const )
        {
            if( x->a != 0 )
            {
                Put1(Xor, y->r, y->r, -1);
                Put1a(And, RH-1, y->r, x->a);
            }
            x->mode = Reg;
            x->r = RH-1;
        }
        else
        {
            RH--;
            Put0(Ann, RH-1, x->r, y->r);
        }
    }
}

void In(Item* x, Item* y)  // x := x IN y
{
    load(y);
    if( x->mode == Const )
    {
        Put1(Ror, y->r, y->r, (x->a + 1) & 0x1F);
        RH--;
    }
    else
    {
        load(x);
        Put1(Add, x->r, x->r, 1);
        Put0(Ror, y->r, y->r, x->r);
        RH = RH - 2;
    }
    SetCC(x, MI);
}

void SetOp(int op, Item* x, Item* y)   // x := x op y
{
    int xset, yset; //x->type->form == Set

    if( (x->mode == Const) && (y->mode == Const) )
    {
        xset = x->a;
        yset = y->a; //correct?
        if( op == PLUS )
        {
            xset = xset | yset;
        }
        else if( op == MINUS )
        {
            xset = xset & (~yset);
        }
        else if( op == MUL )
        {
            xset = xset & yset;
        }
        else if( op == RDIV )
        {
            xset = xset ^ yset;
        }
        x->a = xset;//correct?
    }
    else if( y->mode == Const )
    {
        load(x);
        if( op == PLUS )
        {
            Put1a(Ior, x->r, x->r, y->a);
        }
        else if( op == MINUS )
        {
            Put1a(Ann, x->r, x->r, y->a);
        }
        else if( op == MUL )
        {
            Put1a(And, x->r, x->r, y->a);
        }
        else if( op == RDIV )
        {
            Put1a(Xor, x->r, x->r, y->a);
        } ;
    }
    else
    {
        load(x);
        load(y);
        if( op == PLUS )
        {
            Put0(Ior, RH-2, x->r, y->r);
        }
        else if( op == MINUS )
        {
            Put0(Ann, RH-2, x->r, y->r);
        }
        else if( op == MUL )
        {
            Put0(And, RH-2, x->r, y->r);
        }
        else if( op == RDIV )
        {
            Put0(Xor, RH-2, x->r, y->r);
        } ;
        RH--;
        x->r = RH-1;
    }
}

// Code generation for relations

void IntRelation(int op, Item* x, Item* y)   // x := x < y
{
    if( (y->mode == Const) && (y->type->form != Proc) )
    {
        load(x);
        if( (y->a != 0) || !(op == EQL || op == NEQ) || ((code[pc-1] >> 30) != -2) )
        {
            Put1a(Cmp, x->r, x->r, y->a);
        }
        RH--;
    }
    else
    {
        if( (x->mode == Cond) || (y->mode == Cond) )
        {
            Mark("not implemented");
        }
        load(x);
        load(y);
        Put0(Cmp, x->r, x->r, y->r);
        RH = RH - 2;
    }
    SetCC(x, relmap[op - EQL]);
}

void RealRelation(int op, Item* x, Item* y )   // x := x < y
{
    load(x);
    if( (y->mode == Const) && (y->a == 0) )
    {
        RH--;
    }
    else
    {
        load(y);
        Put0(Fsb, x->r, x->r, y->r);
        RH = RH - 2;
    }
    SetCC(x, relmap[op - EQL]);
}

void StringRelation(int op, Item* x, Item* y)   // x := x < y
{
    //x, y are char arrays or strings
    if( x->type->form == String )
    {
        loadStringAdr(x);
    }
    else
    {
        loadAdr(x);
    }

    if( y->type->form == String )
    {
        loadStringAdr(y);
    }
    else
    {
        loadAdr(y);
    }

    Put2(Ldr+1, RH, x->r, 0); //load byte
    Put1(Add, x->r, x->r, 1);
    Put2(Ldr+1, RH+1, y->r, 0); //load byte
    Put1(Add, y->r, y->r, 1);
    Put0(Cmp, RH+2, RH, RH+1);
    Put3(BC, NE, 2);
    Put1(Cmp, RH+2, RH, 0);
    Put3(BC, NE, -8);
    RH = RH - 2;
    SetCC(x, relmap[op - EQL]);
}

// Code generation of Assignments

void StrToChar(Item* x )
{
    x->type = charType;
    strx = strx - 4;
    x->a = _str[x->a];
}

void Store(Item* x, Item* y) // x := y
{
    int op;
    load(y);
    if( x->type->size == 1 )
    {
        op = Str+1;
    }
    else
    {
        op = Str;
    }
    if( x->mode == Var )
    {
        if( x->r > 0 )//local
        {
            Put2(op, y->r, SP, x->a + frame);
        }
        else
        {
            GetSB(x->r);
            Put2(op, y->r, SB, x->a);
        }
    }
    else if( x->mode == Par )
    {
        Put2(Ldr, RH, SP, x->a + frame);
        Put2(op, y->r, RH, x->b);
    }
    else if( x->mode == RegI )
    {
        Put2(op, y->r, x->r, x->a);
        RH--;
    }
    else
    {
        Mark("bad mode in Store");
    }
    RH--;
}

void StoreStruct(Item* x, Item* y) // x := y, frame = 0
{
    int s, pc0;
    if( y->type->size != 0 )
    {
        loadAdr(x);
        loadAdr(y);
        if( (x->type->form == Array) && (x->type->len > 0) )
        {
            if( y->type->len >= 0 )
            {
                if( x->type->size >= y->type->size )
                {
                    Put1a(Mov, RH, 0, (y->type->size+3) / 4);
                }
                else
                {
                    Mark("different length/size, not implemented");
                }
            }
            else//y is open array
            {
                Put2(Ldr, RH, SP, y->a+4);
                s = y->type->base->size;//element size
                pc0 = pc;
                Put3(BC, EQ, 0);
                if( s == 1 )
                {
                    Put1(Add, RH, RH, 3);
                    Put1(Asr, RH, RH, 2);
                }
                else if( s != 4 )
                {
                    Put1a(Mul, RH, RH, s / 4);
                }
                if( check )
                {
                    Put1a(Mov, RH+1, 0, (x->type->size+3) / 4);
                    Put0(Cmp, RH+1, RH, RH+1);
                    Trap(GT, 3);
                }
                fix(pc0, pc + 5 - pc0);
            }
        }
        else if( x->type->form == Record )
        {
            Put1a(Mov, RH, 0, x->type->size / 4);
        }
        else
        {
            Mark("inadmissible assignment");
        }
        Put2(Ldr, RH+1, y->r, 0);
        Put1(Add, y->r, y->r, 4);
        Put2(Str, RH+1, x->r, 0);
        Put1(Add, x->r, x->r, 4);
        Put1(Sub, RH, RH, 1);
        Put3(BC, NE, -6);
    }
    RH = 0;
}

void CopyString(Item*x, Item* y)  //x := y
{
    int len;

    loadAdr(x);
    len = x->type->len;
    if( len >= 0 )
    {
        if( len < y->b )
        {
            Mark("string too long");
        }
    }
    else if( check )
    {
        Put2(Ldr, RH, SP, x->a+4);//open array len, frame = 0hk
        Put1(Cmp, RH, RH, y->b);
        Trap(LT, 3);
    }
    loadStringAdr(y);
    Put2(Ldr, RH, y->r, 0);
    Put1(Add, y->r, y->r, 4);
    Put2(Str, RH, x->r, 0);
    Put1(Add, x->r, x->r, 4);
    Put1(Asr, RH, RH, 24);
    Put3(BC, NE, -6);
    RH = 0;

}

// Code generation for parameters

void OpenArrayParam(Item* x)
{
    loadAdr(x);
    if( x->type->len >= 0 )
    {
        Put1a(Mov, RH, 0, x->type->len);
    }
    else
    {
        Put2(Ldr, RH, SP, x->a+4+frame);
    }
    incR();
}

void VarParam(Item* x, Type ftype)
{
    int xmd;
    xmd = x->mode;
    loadAdr(x);
    if( (ftype->form == Array) && (ftype->len < 0) )//open array
    {
        if( x->type->len >= 0 )
        {
            Put1a(Mov, RH, 0, x->type->len);
        }
        else
        {
            Put2(Ldr, RH, SP, x->a+4+frame);
        }
        incR();
    }
    else if( ftype->form == Record )
    {
        if( xmd == Par )
        {
            Put2(Ldr, RH, SP, x->a+4+frame);
            incR();
        }
        else
        {
            loadTypTagAdr(x->type);
        }
    }
}

void ValueParam(Item* x)
{
    load(x);
}

void StringParam(Item* x)
{
    loadStringAdr(x);
    Put1(Mov, RH, 0, x->b);
    incR();//len
}

// For Statements

void For0(Item* x, Item* y)
{
    load(y);
}

void For1(Item* x, Item* y, Item* z, Item* w, int* L)
{
    if( z->mode == Const )
    {
        Put1a(Cmp, RH, y->r, z->a);
    }
    else
    {
        load(z);
        Put0(Cmp, RH-1, y->r, z->r);
        RH--;
    }
    *L = pc;
    if( w->a > 0 )
    {
        Put3(BC, GT, 0);
    }
    else if( w->a < 0 )
    {
        Put3(BC, LT, 0);
    }
    else
    {
        Mark("zero increment");
        Put3(BC, MI, 0);
    }
    Store(x, y);
}

void For2(Item* x, Item* y, Item* w)
{
    load(x);
    RH--;
    Put1a(Add, x->r, x->r, w->a);
}

// Branches, procedure calls, procedure prolog and epilog

int Here()
{
    invalSB();
    return pc;
}

void FJump(int *L)
{
    Put3(BC, 7, *L); //write the code for branch instruction
    *L = pc-1; //get the address of this just written branching code in L
}

void CFJump(Item* x)
{
    if( x->mode != Cond )
    {
        loadCond(x);
    }
    Put3(BC, negated(x->r), x->a);
    FixLink(x->b);
    x->a = pc-1;
}

void BJump(int L)
{
    Put3(BC, 7, L-pc-1);
}

void CBJump(Item* x, int L)
{
    if( x->mode != Cond )
    {
        loadCond(x);
    }
    Put3(BC, negated(x->r), L-pc-1);
    FixLink(x->b);
    FixLinkWith(x->a, L);
}

void Fixup(Item* x)
{
    FixLink(x->a);
}

void SaveRegs(int r)
{
    int r0;

    //r > 0
    r0 = 0;
    Put1(Sub, SP, SP, r*4);
    frame = frame + 4*r;
    do
    {
        Put2(Str, r0, SP, (r-r0-1)*4);
        r0++;
    }
    while(!( r0 == r ));
}

void RestoreRegs(int r) //R[0 .. r-1]
{
    int r0;

    //r > 0
    r0 = r;
    do
    {
        r0--;
        Put2(Ldr, r0, SP, (r-r0-1)*4);
    }
    while(!( r0 == 0 ));
    Put1(Add, SP, SP, r*4);
    frame = frame - 4*r;
}

void PrepCall(Item* x, int* r)
{
    //x->type->form == Proc
    if( x->mode > Par )
    {
        load(x);
    }
    *r = RH;
    if( RH > 0 )
    {
        SaveRegs(RH);
        RH = 0;
    }
}

void Call(Item* x, int r)
{
    //x->type->form == Proc
    if( x->mode == Const )
    {
        if( x->r >= 0 )
        {
            Put3(BL, 7, (x->a / 4)-pc-1);
        }
        else//imported
        {
            if( pc - fixorgP < 0x1000 )
            {
                Put3(BL, 7, ((-x->r) * 0x100 + x->a) * 0x1000 + pc-fixorgP);
                fixorgP = pc-1;
            }
            else
            {
                Mark("fixup impossible");
            }
        }
    }
    else
    {
        if( x->mode <= Par )
        {
            load(x);
            RH--;
        }
        else
        {
            Put2(Ldr, RH, SP, 0);
            Put1(Add, SP, SP, 4);
            r--;
            frame = frame - 4;
        }
        if( check )
        {
            Trap(EQ, 5);
        }
        Put3(BLR, 7, RH);
    }
    if( x->type->base->form == NoTyp )//procedure
    {
        RH = 0;
    }
    else//function
    {
        if( r > 0 )
        {
            Put0(Mov, r, 0, 0);
            RestoreRegs(r);
        }
        x->mode = Reg;
        x->r = r;
        RH = r+1;
    }
    invalSB();
}

//procedure prolog
void Enter(int parblksize, int locblksize, int internal)
{
    int a, r;
    invalSB();
    frame = 0;

    if( !internal ) //normal procedure
    {
        a = 4;
        r = 0;
        Put1(Sub, SP, SP, locblksize); //allocate space for parameters and local variables, locblksize includes parblksize
        Put2(Str, LNK, SP, 0); //put link reg value on stack, where now SP points
        while( a < parblksize ) //we know that parameters to this function have come through registers
        {
            Put2(Str, r, SP, a); //store register values R0, R1 etc. in this order to stack locations SP+4, SP+8 respectively
            r++;
            a = a + 4;
        }
    }
    else //interrupt procedure
    {
        Put1(Sub, SP, SP, 12);
        Put2(Str, 0, SP, 0);
        Put2(Str, 1, SP, 4);
        Put2(Str, SB, SP, 8);
        //R0, R1, SB saved on stack
    }
}

//procedure epilog
void Return(int form, Item* x, int size, int internal)
{
    if( form != NoTyp )
    {
        load(x); //why? load result to which register?
    }
    if( !internal ) //normal procedure
    {
        Put2(Ldr, LNK, SP, 0);
        Put1(Add, SP, SP, size);
        Put3(BR, 7, LNK);
    }
    else //interrupt procedure return, restore SB, R1, R0
    {
        Put2(Ldr, SB, SP, 8);
        Put2(Ldr, 1, SP, 4);
        Put2(Ldr, 0, SP, 0);
        Put1(Add, SP, SP, 12);
        Put3(BR, 7, 0x10); //why? jump to address in R16?
    }
    RH = 0;
}

// In-line code procedures

void Increment(int upordown, Item* x, Item* y)
{
    int op, zr, v;

    //frame = 0
    if( upordown == 0 )
    {
        op = Add;
    }
    else
    {
        op = Sub;
    }

    if( x->type == byteType )
    {
        v = 1;
    }
    else
    {
        v = 0;
    }

    if( y->type->form == NoTyp )
    {
        y->mode = Const;
        y->a = 1;
    }

    if( (x->mode == Var) && (x->r > 0) )
    {
        zr = RH;
        Put2(Ldr+v, zr, SP, x->a); //if v=0, load word, if v=1, load byte
        incR();
        if( y->mode == Const )
        {
            Put1a(op, zr, zr, y->a);
        }
        else
        {
            load(y);
            Put0(op, zr, zr, y->r);
            RH--;
        }
        Put2(Str+v, zr, SP, x->a); //if v=0, store word, if v=1, store byte
        RH--;
    }
    else
    {
        loadAdr(x);
        zr = RH;
        Put2(Ldr+v, RH, x->r, 0); //if v=0, load word, if v=1, load byte
        incR();
        if( y->mode == Const )
        {
            Put1a(op, zr, zr, y->a);
        }
        else
        {
            load(y);
            Put0(op, zr, zr, y->r);
            RH--;
        }
        Put2(Str+v, zr, x->r, 0); //if v=0, store word, if v=1, store byte
        RH = RH - 2;
    }
}

void Include(int inorex, Item* x, Item* y)
{
    int op, zr;
    loadAdr(x);
    zr = RH;
    Put2(Ldr, RH, x->r, 0);
    incR();
    if( inorex == 0 )
    {
        op = Ior;
    }
    else
    {
        op = Ann;
    }
    if( y->mode == Const )
    {
        Put1a(op, zr, zr, (1 << y->a));
    }
    else
    {
        load(y);
        Put1(Mov, RH, 0, 1);
        Put0(Lsl, y->r, RH, y->r);
        Put0(op, zr, zr, y->r);
        RH--;
    }
    Put2(Str, zr, x->r, 0);
    RH = RH - 2;
}

void Assert(Item* x)
{
    int cond;

    if( x->mode != Cond )
    {
        loadCond(x);
    }
    if( x->a == 0 )
    {
        cond = negated(x->r);
    }
    else
    {
        Put3(BC, x->r, x->b);
        FixLink(x->a);
        x->b = pc-1;
        cond = 7;
    }
    Trap(cond, 7);
    FixLink(x->b);
}

void New(Item* x)
{
    loadAdr(x);
    loadTypTagAdr(x->type->base);
    Trap(7, 0);
    RH = 0;
    invalSB();
}

void Pack(Item* x, Item* y)
{
    Item z;
    z = *x;
    load(x);
    load(y);
    Put1(Lsl, y->r, y->r, 23);
    Put0(Add, x->r, x->r, y->r);
    RH--;
    Store(&z, x);
}

void Unpk(Item* x, Item* y)
{
    Item z, e0;
    z = *x;
    load(x);
    e0.mode = Reg;
    e0.r = RH;
    e0.type = intType;
    Put1(Asr, RH, x->r, 23);
    Put1(Sub, RH, RH, 127);
    Store(y, &e0);
    incR();
    Put1(Lsl, RH, RH, 23);
    Put0(Sub, x->r, x->r, RH);
    Store(&z, x);
}

void Led(Item* x)
{
    load(x);
    Put1(Mov, RH, 0, -60);
    Put2(Str, x->r, RH, 0);
    RH--;
}

void _Get(Item* x, Item* y)
{
    load(x);
    x->type = y->type;
    x->mode = RegI;
    x->a = 0;
    Store(y, x);
}

void Put(Item* x, Item* y)
{
    load(x);
    x->type = y->type;
    x->mode = RegI;
    x->a = 0;
    Store(x, y);
}

void Copy(Item* x, Item* y , Item* z)
{
    load(x);
    load(y);
    if( z->mode == Const )
    {
        if( z->a > 0 )
        {
            load(z);
        }
        else
        {
            Mark("bad count");
        }
    }
    else
    {
        load(z);
        if( check )
        {
            Trap(LT, 3);
        }
        Put3(BC, EQ, 6);
    }
    Put2(Ldr, RH, x->r, 0);
    Put1(Add, x->r, x->r, 4);
    Put2(Str, RH, y->r, 0);
    Put1(Add, y->r, y->r, 4);
    Put1(Sub, z->r, z->r, 1);
    Put3(BC, NE, -6);
    RH = RH - 3;
}

void LDPSR(Item* x)
{
    //x->mode == Const
    Put3(0, 15, x->a + 0x20);
}

void LDREG(Item* x, Item* y)
{
    if( y->mode == Const )
    {
        Put1a(Mov, x->a, 0, y->a);
    }
    else
    {
        load(y);
        Put0(Mov, x->a, 0, y->r);
        RH--;
    }
}


//In-line code functions

//absolute value of a Real or Int, no other
void Abs(Item* x)
{
    if( x->mode == Const )
    {
        x->a = x->a>=0 ? x->a : -x->a; //abs
    }
    else
    {
        load(x);
        if( x->type->form == Real )
        {
            Put1(Lsl, x->r, x->r, 1);
            Put1(Ror, x->r, x->r, 1);
        }
        else // Int
        {
            Put1(Cmp, x->r, x->r, 0);
            Put3(BC, GE, 2);
            Put1(Mov, RH, 0, 0);
            Put0(Sub, x->r, RH, x->r);
        }
    }
}

void Odd(Item* x)
{
    load(x); //bring to register (even if it is a literal)
    Put1(And, x->r, x->r, 1);
    SetCC(x, NE);
    RH--;
}

void Floor(Item* x)
{
    load(x);
    Put1(Mov+U, RH, 0, 0x4B00);
    Put0(Fad+V, x->r, x->r, RH);
}

void Float(Item* x)
{
    load(x);
    Put1(Mov+U, RH, 0, 0x4B00);
    Put0(Fad+U, x->r, x->r, RH);
}

void Ord(Item* x)
{
    if( x->mode == Var || x->mode == Par || x->mode == RegI || x->mode == Cond )
    {
        load(x);
    }
}

void Len(Item* x)
{
    if( x->type->len >= 0 )
    {
        if( x->mode == RegI )
        {
            RH--;
        }
        x->mode = Const;
        x->a = x->type->len;
    }
    else//open array
    {
        Put2(Ldr, RH, SP, x->a + 4 + frame);
        x->mode = Reg;
        x->r = RH;
        incR();
    }
}

void Shift(int fct, Item* x, Item* y)
{
    int op;
    load(x);
    if( fct == 0 )
    {
        op = Lsl;
    }
    else if( fct == 1 )
    {
        op = Asr;
    }
    else
    {
        op = Ror;
    }
    if( y->mode == Const )
    {
        Put1(op, x->r, x->r, y->a & 0x1F);
    }
    else
    {
        load(y);
        Put0(op, RH-2, x->r, y->r);
        RH--;
        x->r = RH-1;
    }
}

void ADC(Item* x, Item* y)
{
    load(x);
    load(y);
    Put0(Add+0x2000, x->r, x->r, y->r);
    RH--;
}

void SBC(Item* x, Item* y)
{
    load(x);
    load(y);
    Put0(Sub+0x2000, x->r, x->r, y->r);
    RH--;
}

void UML(Item* x, Item* y)
{
    load(x);
    load(y);
    Put0(Mul+0x2000, x->r, x->r, y->r);
    RH--;
}

void Bit(Item* x, Item* y)
{
    load(x);
    Put2(Ldr, x->r, x->r, 0);
    if( y->mode == Const )
    {
        Put1(Ror, x->r, x->r, y->a+1);
        RH--;
    }
    else
    {
        load(y);
        Put1(Add, y->r, y->r, 1);
        Put0(Ror, x->r, x->r, y->r);
        RH = RH - 2;
    }
    SetCC(x, MI);
}

void Register(Item* x)
{
    //x->mode == Const
    Put0(Mov, RH, 0, x->a & 0xF);
    x->mode = Reg;
    x->r = RH;
    incR();
}

void H(Item* x)
{
    //x->mode == Const
    Put0(Mov + U + (x->a & 0x1) * V, RH, 0, 0);
    x->mode = Reg;
    x->r = RH;
    incR();
}

void Adr(Item* x)
{
    if( x->mode == Var || x->mode == Par || x->mode == RegI )
    {
        loadAdr(x);
    }
    else if( (x->mode == Const) && (x->type->form == Proc) )
    {
        load(x);
    }
    else if( (x->mode == Const) && (x->type->form == String) )
    {
        loadStringAdr(x);
    }
    else
    {
        Mark("not addressable");
    }
}

void Condition(Item* x)
{
    //x->mode == Const
    SetCC(x, x->a);
}

//initialize ORG
void Open(int v)
{
    pc = 0;
    tdx = 0;
    strx = 0;
    RH = 0;
    fixorgP = 0;
    fixorgD = 0;
    fixorgT = 0;
    check = (v != 0); //check=1 for risc v=1, not for v=0
    version = v;
    if( v == 0 )
    {
        pc = 1;
        do
        {
            code[pc] = 0;
            pc++;
        }
        while(!( pc == 8 ));
    }
}

//updates whole size of total variables declared in a module,
//which is collected from dc of OBP
void SetDataSize(int dc)
{
    varsize = dc;
}

void Header()
{
    entry = pc*4; //for RISC-5, pc=0, entry=0

    if( version == 0 ) //RISC-0
    {
        code[0] = 0x0E7000000-1 + pc;
        Put1a(Mov, SB, 0, VarOrg0);
        Put1a(Mov, SP, 0, StkOrg0);
    }
    else //RISC-5
    {
        Put1(Sub, SP, SP, 4); //make space for link reg value
        Put2(Str, LNK, SP, 0); //store link reg value there
        invalSB();
    }
}

int NofPtrs(Type typ)
{
    Object fld;
    int n;

    if( (typ->form == Pointer) || (typ->form == NilTyp) )
    {
        n = 1;
    }
    else if( typ->form == Record )
    {
        fld = typ->dsc;
        n = 0;
        while( fld != 0 )
        {
            n = NofPtrs(fld->type) + n;
            fld = fld->next;
        }
    }
    else if( typ->form == Array )
    {
        n = NofPtrs(typ->base) * typ->len;
    }
    else
    {
        n = 0;
    }
    return n;
}

void WriteByte(FILE *R, int x)
{
    fputc(x, R);
}

void WriteInt(FILE *R, int x)
{
    fwrite(&x, sizeof(int), 1, R);
}

void FindPtrs(FILE* R, Type typ, int adr)
{
    Object fld;
    int i, s;

    if( (typ->form == Pointer) || (typ->form == NilTyp) )
    {
        WriteInt(R, adr);
    }
    else if( typ->form == Record )
    {
        fld = typ->dsc;
        while( fld != 0 )
        {
            FindPtrs(R, fld->type, fld->val + adr);
            fld = fld->next;
        }
    }
    else if( typ->form == Array )
    {
        s = typ->base->size;
        for (i = 0; i <= typ->len-1; i++)
        {
            FindPtrs(R, typ->base, i*s + adr);
        }
    }
}

void Close(char* modid, int key, int nofent)
{
    Object obj;
    int i, comsize, nofimps, nofptrs, size;
    char name[ID_LEN];
    FILE* R;

    //exit code
    if( version == 0 )
    {
        Put1(Mov, 0, 0, 0);
        Put3(BR, 7, 0);//RISC-0
    }
    else
    {
        Put2(Ldr, LNK, SP, 0); //pop to link reg
        Put1(Add, SP, SP, 4); //reduce stack
        Put3(BR, 7, LNK); //jump to link reg
    }

    //compute total size of object code to be written to *.rsc file
    obj = topScope->next; //jump over Head
    nofimps = 0;
    comsize = 4;
    nofptrs = 0;
    while( obj != 0 )
    {
        if( (obj->class == Mod) && (obj->dsc != System) )
        {
            nofimps++; //count imports excluding SYSTEM
        }
        //these are conditions for being a command procedure
        else if( (obj->exno != 0) && (obj->class == Const) && (obj->type->form == Proc)
                 && (obj->type->nofpar == 0) && (obj->type->base == noType) )
        {
            i = 0;//count commands
            while( obj->name[i] != 0x0 )
            {
                i++;
            }
            i = (i+4) / 4 * 4; //keep space for '\0'; also make 4byte aligned
            comsize = comsize + i+4; //allocate one more 4byte per command name
        }
        else if( obj->class == Var )
        {
            nofptrs = nofptrs + NofPtrs(obj->type); //count pointers
        }
        obj = obj->next;
    }
    size = varsize + strx + comsize + (pc + nofimps + nofent + nofptrs + 1)*4; //varsize includes type descriptors; pc denotes code length

    //create *.rsc file
    MakeFileName(name, modid, ".rsc");
    R = fopen(name, "wb");
    if(R == 0)
    {
        printf("can't create object file\n");
        exit(0);
    }

	//write header
    WriteString(R, modid); //first write module name with '\0'
    WriteInt(R, key); //4byte for key
    WriteByte(R, version); //1byte for version
    WriteInt(R, size); //4byte for size

	//write imported module names excluding SYSTEM
    obj = topScope->next;
    while( (obj != 0) && (obj->class == Mod) )
    {
        if( obj->dsc != System )
        {
            WriteString(R, ((Module)obj)->orgname); //original module name with '\0'
            WriteInt(R, obj->val); //its val in 4byte
        }
        obj = obj->next;
    }

	//terminator 1byte
    Write(R, 0x0);

	//type descriptor table size
    WriteInt(R, tdx*4); //4byte for type descriptor table size i.e. tdx*4

	//write type descriptors
    i = 0;
    while( i < tdx )
    {
        WriteInt(R, data[i]); //4byte for one type descriptor
        i++;
    }

	//4byte for data size
    WriteInt(R, varsize - tdx*4); //varsize includes type descriptors

	//4byte to write length of all string constants contained in ORG _str[]
    WriteInt(R, strx);

    //now write those strings from _str[]
    for (i = 0; i <= strx-1; i++)
    {
        Write(R, _str[i]);
    }

	//4byte to write code length
    WriteInt(R, pc);

    //write the code
    for(i = 0; i <= pc-1; i++)
    {
        WriteInt(R, code[i]); //code[] is of type int
    }

	//write command names and its val
    obj = topScope->next;
    while( obj != 0 )
    {
        if( (obj->exno != 0) && (obj->class == Const) && (obj->type->form == Proc) &&
                (obj->type->nofpar == 0) && (obj->type->base == noType) )
        {
            WriteString(R, obj->name); //command name with '\0'
            WriteInt(R, obj->val); //its val in 4byte
        }
        obj = obj->next;
    }

    //terminator 1byte
    Write(R, 0x0);

	//4byte for number of entry points
    WriteInt(R, nofent);

	//4byte for entry
    WriteInt(R, entry);

	//entries
    obj = topScope->next;
    while( obj != 0 )
    {
        if( obj->exno != 0 )
        {
            if( ((obj->class == Const) && (obj->type->form == Proc)) || (obj->class == Var) )
            {
                WriteInt(R, obj->val);
            }
            else if( obj->class == Typ )
            {
                if( obj->type->form == Record )
                {
                    WriteInt(R, obj->type->len & 0xFFFF);
                }
                else if( (obj->type->form == Pointer) && ((obj->type->base->typobj == 0) || (obj->type->base->typobj->exno == 0)) )
                {
                    WriteInt(R, obj->type->base->len & 0xFFFF);
                }
            }
        }
        obj = obj->next;
    }

	//pointer variables
    obj = topScope->next;
    while( obj != 0 )
    {
        if( obj->class == Var )
        {
            FindPtrs(R, obj->type, obj->val);
        }
        obj = obj->next;
    }

    WriteInt(R, -1);
    WriteInt(R, fixorgP);
    WriteInt(R, fixorgD);
    WriteInt(R, fixorgT);
    WriteInt(R, entry);

    //1byte for O; this is capital O (ascii 79), not zero
    Write(R, 'O');

    fclose(R);
}

void initObg()
{
	//relmap[] holds RISC-5 opcode for relational oprations for convenience
    relmap[0] = 1; //EQ
    relmap[1] = 9; //NE
    relmap[2] = 5; //LT
    relmap[3] = 6; //LE
    relmap[4] = 14; //GT
    relmap[5] = 13; //GE
}
