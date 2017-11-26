//Oberon7 parser
//Based on Niklaus Wirth's implementation
//Compile using gcc
//$ gcc -Wall obs.c obt.c obg.c obp.c obc.c

//This code is distributed under the GPL License.
//For more info, check: http://www.gnu.org/copyleft/gpl.html

//15 June 2016: Srinivas Nayak: This file created
//21 June 2016: Srinivas Nayak: Coding started
//10 July 2016: Srinivas Nayak: Code updated with NW's version of 04 July 2016

//Parser of Oberon-RISC compiler. Uses Scanner to obtain symbols (tokens),
//Symbol table for definition of data structures and for handling import and export and
//Generator to produce binary code. Parser performs type checking and data allocation.
//Parser is tarGet-independent, except for part of the handling of allocations.

#include "obc.h"

//forward declaration
typedef struct PtrBaseDesc PtrBaseDesc, *PtrBase;

//list of names of pointer base types
typedef struct PtrBaseDesc
{
    char name[ID_LEN];
    Type type;
    PtrBase next;
} PtrBaseDesc, *PtrBase;

int sym; //last symbol/token read, holds return value of scanner Get()
int dc;    //data counter
int level, exno, version;
int newSF;  //compiler option: create/overwrite new symbol file?

char modid[ID_LEN]; //holds currentnly compiling module name
PtrBase pbsList;   //list of names of pointer base types
Object dummy=0;

//some forward declarations
void Declarations(int *varsize);
void expression(Item *x);
void _Type(Type *type);
void FormalType(Type *typ, int dim);

void initObp()
{
    //create a dummy ObjDesc
    NEW((void **)&dummy, sizeof(ObjDesc));
    dummy->class = Var;
    dummy->type = intType;
}

//consume a token if found, else print error message
void Check(int s, char *msg)
{
    if( sym == s )
    {
        Get(&sym);
    }
    else
    {
        Mark(msg);
    }
}

//consumes export mark '*'
void CheckExport(int *expo)
{
    if( sym == MUL )
    {
        *expo = TRUE;
        Get(&sym);
        if( level != 0 )
        {
            Mark("remove asterisk");
        }
    }
    else
    {
        *expo = FALSE;
    }
}

void qualident(Object *obj)
{
    //qualident = [ident "."] ident.
    *obj = thisObj();
    Get(&sym);
    if( *obj == NIL )
    {
        Mark("undef");
        *obj = dummy;
    }
    if( (sym == PERIOD) && ((*obj)->class == Mod) )
    {
        Get(&sym);
        if( sym == IDENT )
        {
            *obj = thisimport(*obj);
            Get(&sym);
            if( *obj == NIL )
            {
                Mark("undef");
                *obj = dummy;
            }
        }
        else
        {
            Mark("identifier expected");
            *obj = dummy;
        }
    }
}

void CheckBool(Item *x)
{
    if( x->type->form != Bool )
    {
        Mark("not Boolean");
        x->type = boolType;
    }
}

void CheckInt(Item *x)
{
    if( x->type->form != Int )
    {
        Mark("not Integer");
        x->type = intType;
    }
}

void CheckReal(Item *x)
{
    if( x->type->form != Real )
    {
        Mark("not Real");
        x->type = realType;
    }
}

void CheckSet(Item *x)
{
    if( x->type->form != Set )
    {
        Mark("not Set");
        x->type = setType;
    }
}

void CheckSetVal(Item *x)
{
    if( x->type->form != Int )
    {
        Mark("not Int");
        x->type = setType;
    }
    else if( x->mode == Const )
    {
        if( (x->a < 0) || (x->a >= 32) )
        {
            Mark("invalid set");
        }
    }
}

void CheckConst(Item *x)
{
    if( x->mode != Const )
    {
        Mark("not a constant");
        x->mode = Const;
    }
}

void CheckReadOnly(Item *x)
{
    if( x->rdo )
    {
        Mark("read-only");
    }
}


int IsExtension(Type t0, Type t1)
{
    //t1 is an extension of t0
    return (t0 == t1) || ((t1 != NIL) && IsExtension(t0, t1->base)) ;
}


//expressions
void TypeTest(Item *x, Type T, int guard)
{
    Type xt;
    xt = x->type;
    if( (T->form == Pointer || T->form == Record) && (T->form == xt->form) )
    {
        while( (xt != T) && (xt != NIL) )
        {
            xt = xt->base;
        }
        if( xt != T )
        {
            xt = x->type;
            if( xt->form == Pointer )
            {
                if( IsExtension(xt->base, T->base) )
                {
                    _TypeTest(x, T->base, FALSE, guard);
                    x->type = T;
                }
                else
                {
                    Mark("not an extension");
                }
            }
            else if( (xt->form == Record) && (x->mode == Par) )
            {
                if( IsExtension(xt, T) )
                {
                    _TypeTest(x, T, TRUE, guard);
                    x->type = T;
                }
                else
                {
                    Mark("not an extension");
                }
            }
            else
            {
                Mark("incompatible types");
            }
        }
        else if( !guard )
        {
            MakeConstItem(x, boolType, 1);
        }
    }
    else
    {
        Mark("type mismatch");
    }
    if( !guard )
    {
        x->type = boolType;
    }
}

void selector(Item *x)
{
    Item y;
    Object obj;

    //selector = "." ident | "[" ExpList "]" | "^" | "(" qualident ")".
    while( (sym == LBRAK) || (sym == PERIOD) || (sym == ARROW)
            || ((sym == LPAREN) && (x->type->form == Record || x->type->form == Pointer)) )
    {

        //ExpList = expression {"," expression}.
        if( sym == LBRAK )
        {
            do
            {
                Get(&sym);
                expression(&y);
                if( x->type->form == Array )
                {
                    CheckInt(&y);
                    Index(x, &y);
                    x->type = x->type->base;
                }
                else
                {
                    Mark("not an array");
                }
            }
            while(!( sym != COMMA ));
            Check(RBRAK, "no ]");
        }
        else if( sym == PERIOD )
        {
            Get(&sym);
            if( sym == IDENT )
            {
                if( x->type->form == Pointer )
                {
                    DeRef(x);
                    x->type = x->type->base;
                }
                if( x->type->form == Record )
                {
                    obj = thisfield(x->type);
                    Get(&sym);
                    if( obj != NIL )
                    {
                        Field(x, obj);
                        x->type = obj->type;
                    }
                    else
                    {
                        Mark("undef");
                    }
                }
                else
                {
                    Mark("not a record");
                }
            }
            else
            {
                Mark("ident?");
            }
        }
        else if( sym == ARROW )
        {
            Get(&sym);
            if( x->type->form == Pointer )
            {
                DeRef(x);
                x->type = x->type->base;
            }
            else
            {
                Mark("not a pointer");
            }
        }
        else if( (sym == LPAREN) && (x->type->form == Record || x->type->form == Pointer) ) //type guard
        {
            Get(&sym);
            if( sym == IDENT )
            {
                qualident(&obj);
                if( obj->class == Typ )
                {
                    TypeTest(x, obj->type, TRUE);
                }
                else
                {
                    Mark("guard type expected");
                }
            }
            else
            {
                Mark("not an identifier");
            }
            Check(RPAREN, " ) missing");
        }

    }
}


int CompTypes(Type t0, Type t1, int varpar);
int EqualSignatures(Type t0, Type t1)
{
    Object p0, p1;
    int com;
    com = TRUE;
    if( (t0->base == t1->base) && (t0->nofpar == t1->nofpar) )
    {
        p0 = t0->dsc;
        p1 = t1->dsc;
        while( p0 != NIL)
        {
            if( (p0->class == p1->class) && (p0->rdo == p1->rdo) &&
                    ((p0->type == p1->type) ||
                     ((p0->type->form == Array) && (p1->type->form == Array) && (p0->type->len == p1->type->len) && (p0->type->base == p1->type->base)) ||
                     ((p0->type->form == Proc) && (p1->type->form == Proc) && EqualSignatures(p0->type, p1->type)))
              )
            {
                p0 = p0->next;
                p1 = p1->next;
            }
            else
            {
                p0 = NIL;
                com = FALSE;
            }
        }
    }
    else
    {
        com = FALSE;
    }

    return com;
}


int CompTypes(Type t0, Type t1, int varpar)
{
    //check for assignment compatibility
    return (t0 == t1)
           || ((t0->form == Array) && (t1->form == Array) && (t0->base == t1->base) && (t0->len == t1->len))
           || ((t0->form == Record) && (t1->form == Record) && IsExtension(t0, t1))
           || (!varpar &&
               (((t0->form == Pointer) && (t1->form == Pointer) && IsExtension(t0->base, t1->base))
                || ((t0->form == Proc) && (t1->form == Proc) && EqualSignatures(t0, t1))
                || ((t0->form == Pointer || t0->form == Proc) && (t1->form == NilTyp)))) ;
}


void Parameter(Object par)
{
    Item x;
    int varpar;
    expression(&x);
    if( par != NIL)
    {
        varpar = (par->class == Par);
        if( CompTypes(par->type, x.type, varpar) )
        {
            if( !varpar )
            {
                ValueParam(&x);
            }
            else // par->class = Par it was commented in ORP.Mod.txt, why?
            {
                if( !par->rdo )
                {
                    CheckReadOnly(&x);
                }
                VarParam(&x, par->type);
            }
        }
        else if( (x.type->form == Array) && (par->type->form == Array) &&
                 (x.type->base == par->type->base) && (par->type->len < 0) )
        {
            if( !par->rdo )
            {
                CheckReadOnly(&x);
            }
            OpenArrayParam(&x);
        }
        else if( (x.type->form == String) && varpar && par->rdo && (par->type->form == Array) &&
                 (par->type->base->form == Char) && (par->type->len < 0) )
        {
            StringParam(&x);
        }
        else if( !varpar && (par->type->form == Int) && (x.type->form == Int) )
        {
            ValueParam(&x);
        }
        else if( (x.type->form == String) && (x.b == 2) && (par->class == Var) && (par->type->form == Char) )
        {
            StrToChar(&x);
            ValueParam(&x);
        }
        else if( (par->type->form == Array) && (par->type->base == byteType) &&
                 (par->type->len > 0) && (par->type->size == x.type->size) )
        {
            VarParam(&x, par->type);
        }
        else
        {
            Mark("incompatible parameters");
        }
    }
}



void ParamList(Item *x)
{
    int n;
    Object par;
    par = x->type->dsc;
    n = 0;


    //ActualParameters = "(" [ExpList] ")" .
    //ExpList = expression {"," expression}.
    if( sym != RPAREN )
    {
        Parameter(par);
        n = 1;
        while( sym <= COMMA )
        {
            Check(sym, "comma?");
            if( par != NIL )
            {
                par = par->next;
            }
            n++;
            Parameter(par);
        }
        Check(RPAREN, ") missing");
    }
    else
    {
        Get(&sym);
    }
    if( n < x->type->nofpar )
    {
        Mark("too few params");
    }
    else if( n > x->type->nofpar )
    {
        Mark("too many params");
    }
}

void StandFunc(Item *x, int fct, Type restyp)
{
    Item y;
    int n, npar;
    Check(LPAREN, "no (");
    npar = fct % 10;
    fct = fct / 10;
    expression(x);
    n = 1;
    while( sym == COMMA )
    {
        Get(&sym);
        expression(&y);
        n++;
    }
    Check(RPAREN, "no )");
    if( n == npar )
    {
        if( fct == 0 ) //ABS
        {
            if( x->type->form == Int || x->type->form == Real )
            {
                Abs(x);
                restyp = x->type;
            }
            else
            {
                Mark("bad type");
            }
        }
        else if( fct == 1 ) //ODD
        {
            CheckInt(x);
            Odd(x);
        }
        else if( fct == 2 ) //FLOOR
        {
            CheckReal(x);
            Floor(x);
        }
        else if( fct == 3 ) //FLT
        {
            CheckInt(x);
            Float(x);
        }
        else if( fct == 4 ) //ORD
        {
            if( x->type->form <= Proc )
            {
                Ord(x);
            }
            else if( (x->type->form == String) && (x->b == 2) )
            {
                StrToChar(x);
            }
            else
            {
                Mark("bad type");
            }
        }
        else if( fct == 5 ) //CHR
        {
            CheckInt(x);
            Ord(x);
        }
        else if( fct == 6 ) //LEN
        {
            if( x->type->form == Array )
            {
                Len(x);
            }
            else
            {
                Mark("not an array");
            }
        }
        else if( fct == 7 || fct == 8 || fct == 9 ) //LSL, ASR, ROR
        {
            CheckInt(&y);
            if( x->type->form == Int || x->type->form == Set )
            {
                Shift(fct-7, x, &y);
                restyp = x->type;
            }
            else
            {
                Mark("bad type");
            }
        }
        else if( fct == 11 ) //ADC
        {
            ADC(x, &y);
        }
        else if( fct == 12 ) //SBC
        {
            SBC(x, &y);
        }
        else if( fct == 13 ) //UML
        {
            UML(x, &y);
        }
        else if( fct == 14 ) //BIT
        {
            CheckInt(x);
            CheckInt(&y);
            Bit(x, &y);
        }
        else if( fct == 15 ) //REG
        {
            CheckConst(x);
            CheckInt(x);
            Register(x);
        }
        else if( fct == 16 ) //VAL
        {
            if( (x->mode == Typ) && (x->type->size <= y.type->size) )
            {
                restyp = x->type;
                *x = y;
            }
            else
            {
                Mark("casting not allowed");
            }
        }
        else if( fct == 17 ) //ADR
        {
            Adr(x);
        }
        else if( fct == 18 ) //SIZE
        {
            if( x->mode == Typ )
            {
                MakeConstItem(x, intType, x->type->size);
            }
            else
            {
                Mark("must be a type");
            }
        }
        else if( fct == 19 ) //COND
        {
            CheckConst(x);
            CheckInt(x);
            Condition(x);
        }
        else if( fct == 20 ) //H
        {
            CheckConst(x);
            CheckInt(x);
            H(x);
        }
        x->type = restyp;
    }
    else
    {
        Mark("wrong nof params");
    }
}

void element(Item *x)
{
    Item y;

    //element = expression [".." expression].
    expression(x);
    CheckSetVal(x);
    if( sym == UPTO )
    {
        Get(&sym);
        expression(&y);
        CheckSetVal(&y);
        _Set(x, &y);
    }
    else
    {
        Singleton(x);
    }
    x->type = setType;
}

void set(Item *x)
{
    Item y;

    //set = "{" [element {"," element}] "}".
    if(

        sym >= IF

    )
    {
        if( sym != RBRACE )
        {
            Mark(" } missing");
        }
        MakeConstItem(x, setType, 0); //empty set
    }
    else
    {
        element(x);
        while( (sym < RPAREN) || (sym > RBRACE) )
        {
            if( sym == COMMA )
            {
                Get(&sym);
            }
            else if( sym != RBRACE )
            {
                Mark("missing comma");
            }
            element(&y);
            SetOp(PLUS, x, &y);
        }
    }
}

void factor(Item *x)
{
    Object obj=0;
    int rx;

    //sync
    if( (sym < CHAR) || (sym > IDENT) )
    {
        Mark("expression expected");
        do
        {
            Get(&sym);
        }
        while(!( (sym >= CHAR) && (sym <= IDENT) ));
    }


    //factor = number | string | NIL | TRUE | FALSE | set | designator [ActualParameters] | "(" expression ")" | "~" factor.
    //designator = qualident {selector}.
    if( sym == IDENT )
    {
        qualident(&obj);
        if( obj->class == SFunc )
        {
            StandFunc(x, obj->val, obj->type);
        }
        else
        {
            MakeItem(x, obj, level);
            selector(x);
            if( sym == LPAREN )
            {
                Get(&sym);
                if( (x->type->form == Proc) && (x->type->base->form != NoTyp) )
                {
                    PrepCall(x, &rx);
                    ParamList(x);
                    Call(x, rx);
                    x->type = x->type->base;
                }
                else
                {
                    Mark("not a function");
                    ParamList(x);
                }
            }
        }
    }
    else if( sym == INT )
    {
        MakeConstItem(x, intType, ival);
        Get(&sym);
    }
    else if( sym == REAL )
    {
        MakeRealItem(x, rval);
        Get(&sym);
    }
    else if( sym == CHAR )
    {
        MakeConstItem(x, charType, ival);
        Get(&sym);
    }
    else if( sym == NILL )
    {
        Get(&sym);
        MakeConstItem(x, nilType, 0);
    }
    else if( sym == STRING )
    {
        MakeStringItem(x, slen);
        Get(&sym);
    }
    else if( sym == LPAREN )
    {
        Get(&sym);
        expression(x);
        Check(RPAREN, "no )");
    }
    else if( sym == LBRACE )
    {
        Get(&sym);
        set(x);
        Check(RBRACE, "no }");
    }
    else if( sym == NOT )
    {
        Get(&sym);
        factor(x);
        CheckBool(x);
        Not(x);
    }
    else if( sym == FALS )
    {
        Get(&sym);
        MakeConstItem(x, boolType, 0);
    }
    else if( sym == TRU )
    {
        Get(&sym);
        MakeConstItem(x, boolType, 1);
    }
    else
    {
        Mark("not a factor");
        MakeConstItem(x, intType, 0);
    }

    (void)rx;
}


void term(Item *x)
{
    Item y;
    int op, f;

    //term = factor {MulOperator factor}.
    factor(x);
    f = x->type->form;

    //MulOperator = "*" | "/" | DIV | MOD | "&".
    while( (sym >= MUL) && (sym <= AND) )
    {
        op = sym;
        Get(&sym);
        if( op == MUL )
        {
            if( f == Int )
            {
                factor(&y);
                CheckInt(&y);
                MulOp(x, &y);
            }
            else if( f == Real )
            {
                factor(&y);
                CheckReal(&y);
                RealOp(op, x, &y);
            }
            else if( f == Set )
            {
                factor(&y);
                CheckSet(&y);
                SetOp(op, x, &y);
            }
            else
            {
                Mark("bad type");
            }
        }
        else if( (op == DIV) || (op == MOD) )
        {
            CheckInt(x);
            factor(&y);
            CheckInt(&y);
            DivOp(op, x, &y);
        }
        else if( op == RDIV )
        {
            if( f == Real )
            {
                factor(&y);
                CheckReal(&y);
                RealOp(op, x, &y);
            }
            else if( f == Set )
            {
                factor(&y);
                CheckSet(&y);
                SetOp(op, x, &y);
            }
            else
            {
                Mark("bad type");
            }

        }
        //if op == AND
        else
        {
            CheckBool(x);
            And1(x);
            factor(&y);
            CheckBool(&y);
            And2(x, &y);
        }
    }
}

void SimpleExpression(Item *x)
{
    Item y;
    int op;

    //SimpleExpression = ["+" | "-"] term {AddOperator term}.
    if( sym == MINUS )
    {
        Get(&sym);
        term(x);
        if( x->type->form == Int || x->type->form == Real || x->type->form == Set )
        {
            Neg(x);
        }
        else
        {
            CheckInt(x);
        }
    }
    else if( sym == PLUS )
    {
        Get(&sym);
        term(x);
    }
    else
    {
        term(x);
    }

    //AddOperator = "+" | "-" | OR.
    while( (sym >= PLUS) && (sym <= OR) )
    {
        op = sym;
        Get(&sym);
        if( op == OR )
        {
            Or1(x);
            CheckBool(x);
            term(&y);
            CheckBool(&y);
            Or2(x, &y);
        }
        else if( x->type->form == Int )
        {
            term(&y);
            CheckInt(&y);
            AddOp(op, x, &y);
        }
        else if( x->type->form == Real )
        {
            term(&y);
            CheckReal(&y);
            RealOp(op, x, &y);
        }
        else
        {
            CheckSet(x);
            term(&y);
            CheckSet(&y);
            SetOp(op, x, &y);
        }
    }
}


void expression(Item *x)
{
    Item y;
    Object obj=0;
    int rel, xf, yf;

    //expression = SimpleExpression [relation SimpleExpression].
    SimpleExpression(x);
    //relation = "=" | "#" | "<" | "<=" | ">" | ">=" | IN | IS.
    if( (sym >= EQL) && (sym <= GEQ) ) //EQL = 9, NEQ = 10, LSR = 11, LEQ = 12, GTR = 13, GEQ = 14
    {
        rel = sym; //remember the relational operator
        Get(&sym);
        SimpleExpression(&y);
        xf = x->type->form;
        yf = y.type->form;
        if( x->type == y.type )
        {
            if( (xf == Char || xf == Int) )
            {
                IntRelation(rel, x, &y);
            }
            else if( xf == Real )
            {
                RealRelation(rel, x, &y);
            }
            else if( (xf == Set || xf == Pointer || xf == Proc || xf == NilTyp || xf == Bool) )
            {
                if( rel <= NEQ )
                {
                    IntRelation(rel, x, &y);
                }
                else
                {
                    Mark("only = or #");
                }
            }
            else if( ((xf == Array) && (x->type->base->form == Char)) || (xf == String) )
            {
                StringRelation(rel, x, &y);
            }
            else
            {
                Mark("illegal comparison");
            }
        }
        else if( ((xf == Pointer || xf == Proc) && (yf == NilTyp))
                 || ((yf == Pointer || yf == Proc) && (xf == NilTyp)) )
        {
            if( rel <= NEQ )
            {
                IntRelation(rel, x, &y);
            }
            else
            {
                Mark("only = or #");
            }
        }
        else if( (xf == Pointer) && (yf == Pointer) &&
                 (IsExtension(x->type->base, y.type->base) || IsExtension(y.type->base, x->type->base)) )
        {
            if( rel <= NEQ )
            {
                IntRelation(rel, x, &y);
            }
            else
            {
                Mark("only = or #");
            }
        }
        else if( (((xf == Array) && (x->type->base->form == Char) &&
                   ((yf == String) || ((yf == Array) && (y.type->base->form == Char))))
                  || ((yf == Array) && (y.type->base->form == Char) && (xf == String))) )
        {
            StringRelation(rel, x, &y);
        }
        else if( (xf == Char) && (yf == String) && (y.b == 2) )
        {
            StrToChar(&y);
            IntRelation(rel, x, &y);
        }
        else if( (yf == Char) && (xf == String) && (x->b == 2) )
        {
            StrToChar(x);
            IntRelation(rel, x, &y);
        }
        else if( (xf == Int) && (yf == Int) )
        {
            IntRelation(rel, x, &y);  //BYTE
        }
        else
        {
            Mark("illegal comparison");
        }
        x->type = boolType;
    }
    else if( sym == IN )
    {
        Get(&sym);
        CheckInt(x);
        SimpleExpression(&y);
        CheckSet(&y);
        In(x, &y);
        x->type = boolType;
    }
    else if( sym == IS )
    {
        Get(&sym);
        qualident(&obj);
        TypeTest(x, obj->type, FALSE);
        x->type = boolType;
    }
}


//statements
void StandProc(int pno)
{
    int nap, npar; //nof actual/formal parameters
    Item x, y, z;
    Check(LPAREN, "no (");
    npar = pno % 10;
    pno = pno / 10;
    expression(&x);
    nap = 1;
    if( sym == COMMA )
    {
        Get(&sym);
        expression(&y);
        nap = 2;
        z.type = noType;
        while( sym == COMMA )
        {
            Get(&sym);
            expression(&z);
            nap++;
        }
    }
    else
    {
        y.type = noType;
    }
    Check(RPAREN, "no )");
    if( (npar == nap) || (pno == 0 || pno == 1) )
    {
        if( pno == 0 || pno == 1 ) //INC, DEC
        {
            CheckInt(&x);
            CheckReadOnly(&x);
            if( y.type != noType )
            {
                CheckInt(&y);
            }
            Increment(pno, &x, &y);
        }
        else if( pno == 2 || pno == 3 ) //INCL, EXCL
        {
            CheckSet(&x);
            CheckReadOnly(&x);
            CheckInt(&y);
            Include(pno-2, &x, &y);
        }
        else if( pno == 4 )
        {
            CheckBool(&x);
            Assert(&x);
        }
        else if( pno == 5 ) //NEW
        {
            CheckReadOnly(&x);
            if( (x.type->form == Pointer) && (x.type->base->form == Record) )
            {
                New(&x);
            }
            else
            {
                Mark("not a pointer to record");
            }
        }
        else if( pno == 6 )
        {
            CheckReal(&x);
            CheckInt(&y);
            CheckReadOnly(&x);
            Pack(&x, &y);
        }
        else if( pno == 7 )
        {
            CheckReal(&x);
            CheckInt(&y);
            CheckReadOnly(&x);
            Unpk(&x, &y);
        }
        else if( pno == 8 )
        {
            if( x.type->form <= Set )
            {
                Led(&x);
            }
            else
            {
                Mark("bad type");
            }
        }
        else if( pno == 10 )
        {
            CheckInt(&x);
            _Get(&x, &y);
        }
        else if( pno == 11 )
        {
            CheckInt(&x);
            Put(&x, &y);
        }
        else if( pno == 12 )
        {
            CheckInt(&x);
            CheckInt(&y);
            CheckInt(&z);
            Copy(&x, &y, &z);
        }
        else if( pno == 13 )
        {
            CheckConst(&x);
            CheckInt(&x);
            LDPSR(&x);
        }
        else if( pno == 14 )
        {
            CheckInt(&x);
            LDREG(&x, &y);
        }
    }
    else
    {
        Mark("wrong nof parameters");
    }
}

void StatSequence();

void TypeCase(Object obj, Item *x)
{
    Object typobj=0;
    if( sym == IDENT )
    {
        qualident(&typobj);
        MakeItem(x, obj, level);
        if( typobj->class != Typ )
        {
            Mark("not a type");
        }
        TypeTest(x, typobj->type, FALSE);
        obj->type = typobj->type;
        CFJump(x);
        Check(COLON, ": expected");
        StatSequence();
    }
    else
    {
        CFJump(x);
        Mark("type id expected");
    }
}


void SkipCase()
{
    while( sym != COLON )
    {
        Get(&sym);
    }
    Get(&sym);
    StatSequence();
}

void StatSequence()
{
    Object obj;
    Type orgtype; //original type of case var
    Item x, y, z, w;
    int L0, L1, rx;

    //StatSequence
    do
    {
        obj = NIL;

        //sync
        if( !((sym == IDENT)

                || ((sym >= IF) && (sym <= FOR))

                || (sym >= SEMICOLON)) )
        {
            Mark("statement expected");
            do
            {
                Get(&sym);
            }
            while(!( (sym == IDENT)

                     || (sym >= IF) ));

        }





//StatementSequence = statement {";" statement}.
//statement = [assignment | ProcedureCall | IfStatement | CaseStatement | WhileStatement | RepeatStatement | ForStatement].
//assignment = designator ":=" expression.
//designator = qualident {selector}.
//selector = "." ident | "[" ExpList "]" | "^" | "(" qualident ")".
        if( sym == IDENT )
        {
            qualident(&obj);
            MakeItem(&x, obj, level);
            if( x.mode == SProc )
            {
                StandProc(obj->val);
            }
            else
            {
                selector(&x);
                //assignment = designator ":=" expression.
                if( sym == BECOMES ) //assignment
                {
                    Get(&sym);
                    CheckReadOnly(&x);
                    expression(&y);

                    if( CompTypes(x.type, y.type, FALSE) )
                    {
                        if( (x.type->form <= Pointer) || (x.type->form == Proc) )
                        {
                            Store(&x, &y);
                        }
                        else
                        {
                            StoreStruct(&x, &y);
                        }
                    }
                    else if( (x.type->form == Array) && (y.type->form == Array) && (x.type->base == y.type->base) && (y.type->len < 0) )
                    {
                        StoreStruct(&x, &y);
                    }
                    else if( (x.type->form == Array) && (x.type->base->form == Char) && (y.type->form == String) )
                    {
                        CopyString(&x, &y);
                    }
                    else if( (x.type->form == Int) && (y.type->form == Int) )//BYTE
                    {
                        Store(&x, &y);
                    }
                    else if( (x.type->form == Char) && (y.type->form == String) && (y.b == 2) )
                    {
                        StrToChar(&y);
                        Store(&x, &y);
                    }
                    else
                    {
                        Mark("illegal assignment");
                    }

                }
                else if( sym == EQL )
                {
                    Mark("should be :=");
                    Get(&sym);
                    expression(&y);
                }
                else if( sym == LPAREN )//procedure call
                {
                    Get(&sym);
                    if( (x.type->form == Proc) && (x.type->base->form == NoTyp) )
                    {
                        PrepCall(&x, &rx);
                        ParamList(&x);
                        Call(&x, rx);
                    }
                    else
                    {
                        Mark("not a procedure");
                        ParamList(&x);
                    }
                }
                else if( x.type->form == Proc )//procedure call without parameters
                {
                    if( x.type->nofpar > 0 )
                    {
                        Mark("missing parameters");
                    }
                    if( x.type->base->form == NoTyp )
                    {
                        PrepCall(&x, &rx);
                        Call(&x, rx);
                    }
                    else
                    {
                        Mark("not a procedure");
                    }
                }
                else if( x.mode == Typ )
                {
                    Mark("illegal assignment");
                }
                else
                {
                    Mark("not a procedure");
                }
            }

        }
//IfStatement = IF expression THEN StatementSequence {ELSIF expression THEN StatementSequence} [ELSE StatementSequence] END.
        else if(

            sym == IF

        )
        {
            Get(&sym);
            expression(&x);
            CheckBool(&x);
            CFJump(&x);

            Check(THEN, "no THEN");

            StatSequence();
            L0 = 0;
            while(

                sym == ELSIF

            )
            {
                Get(&sym);
                FJump(&L0);
                Fixup(&x);
                expression(&x);
                CheckBool(&x);
                CFJump(&x);

                Check(THEN, "no THEN");

                StatSequence();
            }
            if(

                sym == ELSE

            )
            {
                Get(&sym);
                FJump(&L0);
                Fixup(&x);
                StatSequence();
            }
            else
            {
                Fixup(&x);
            }
            FixLink(L0);

            Check(END, "no END");



        }
//WhileStatement = WHILE expression DO StatementSequence {ELSIF expression DO StatementSequence} END.
        else if(

            sym == WHILE

        )
        {
            Get(&sym);
            L0 = Here();
            expression(&x);
            CheckBool(&x);
            CFJump(&x);

            Check(DO, "no DO");

            StatSequence();
            BJump(L0);
            while(

                sym == ELSIF

            )
            {
                Get(&sym);
                Fixup(&x);
                expression(&x);
                CheckBool(&x);
                CFJump(&x);

                Check(DO, "no DO");

                StatSequence();
                BJump(L0);
            }
            Fixup(&x);

            Check(END, "no END");



        }
//RepeatStatement = REPEAT StatementSequence UNTIL expression.
        else if(

            sym == REPEAT

        )
        {
            Get(&sym);
            L0 = Here();
            StatSequence();
            if(

                sym == UNTIL

            )
            {
                Get(&sym);
                expression(&x);
                CheckBool(&x);
                CBJump(&x, L0);
            }
            else
            {
                Mark("missing UNTIL");
            }


        }
//ForStatement = FOR ident ":=" expression TO expression [BY ConstExpression] DO StatementSequence END.
        else if( sym == FOR )
        {
            Get(&sym);
            if( sym == IDENT )
            {
                qualident(&obj);
                MakeItem(&x, obj, level);
                CheckInt(&x);
                CheckReadOnly(&x);
                if( sym == BECOMES )
                {
                    Get(&sym);
                    expression(&y);
                    CheckInt(&y);
                    For0(&x, &y);
                    L0 = Here();
                    Check(TO, "no TO");
                    expression(&z);
                    CheckInt(&z);
                    obj->rdo = 1;
                    if( sym == BY )
                    {
                        Get(&sym);
                        expression(&w);
                        CheckConst(&w);
                        CheckInt(&w);
                    }
                    else
                    {
                        MakeConstItem(&w, intType, 1);
                    }

                    Check(DO, "no DO");

                    For1(&x, &y, &z, &w, &L1);
                    StatSequence();

                    Check(END, "no END");

                    For2(&x, &y, &w);
                    BJump(L0);
                    FixLink(L1);
                    obj->rdo = FALSE;
                }
                else
                {
                    Mark(":= expected");
                }
            }
            else
            {
                Mark("identifier expected");
            }






        }
//CaseStatement = CASE expression OF case {"|" case} END.
//case = [CaseLabelList ":" StatementSequence].
//CaseLabelList = LabelRange {"," LabelRange}.
//LabelRange = label [".." label].
//label = integer | string | qualident.

        else if( sym == CASE )
        {
            Get(&sym);
            if( sym == IDENT )
            {
                qualident(&obj);
                orgtype = obj->type;
                if( (orgtype->form == Pointer) || ((orgtype->form == Record) && (obj->class == Par)) )
                {
                    Check(OF, "OF expected");
                    TypeCase(obj, &x);
                    L0 = 0;
                    while( sym == BAR )
                    {
                        Get(&sym);
                        FJump(&L0);
                        Fixup(&x);
                        obj->type = orgtype;
                        TypeCase(obj, &x);
                    }
                    Fixup(&x);
                    FixLink(L0);
                    obj->type = orgtype;
                }
                else
                {
                    Mark("numeric case not implemented");
                    Check(OF, "OF expected");
                    SkipCase();
                    while( sym == BAR )
                    {
                        SkipCase();
                    }
                }
            }
            else
            {
                Mark("ident expected");
            }

            Check(END, "no END");

        }

        CheckRegs();
        if( sym == SEMICOLON ) //why? which semicolon?
        {
            Get(&sym);
        }
        else if( sym < SEMICOLON )
        {
            Mark("missing semicolon?");
        }

    }
    while(!( sym > SEMICOLON ));

    (void)rx;
    (void)L0;
    (void)L1;
}



//Types and declarations
void IdentList(int class, Object *first)
{
    Object obj=0;

//IdentList = identdef {"," identdef}.
//identdef = ident ["*"].
    if( sym == IDENT )
    {
        NewObj(first, id, class);
        Get(&sym);
        CheckExport(&(*first)->expo);
        while( sym == COMMA )
        {
            Get(&sym);
            if( sym == IDENT )
            {
                NewObj(&obj, id, class);
                Get(&sym);
                CheckExport(&obj->expo);
            }
            else
            {
                Mark("ident?");
            }
        }
        //VariableDeclaration = IdentList ":" type.
        if( sym == COLON )
        {
            Get(&sym);
        }
        else
        {
            Mark(":?");
        }
    }
    else
    {
        *first = NIL;
    }
}

void ArrayType(Type *type)
{
    Item x;
    Type typ=0;
    int len;
    NEW((void **)&typ, sizeof(TypeDesc));
    typ->form = NoTyp;
    //ArrayType = ARRAY length {"," length} OF type.
    expression(&x);
    if( (x.mode == Const) && (x.type->form == Int) && (x.a >= 0) )
    {
        len = x.a;
    }
    else
    {
        len = 1;
        Mark("not a valid length");
    }
    if( sym == OF )
    {
        Get(&sym);
        _Type(&typ->base);
        if( (typ->base->form == Array) && (typ->base->len < 0) )
        {
            Mark("dyn array not allowed");
        }
    }
    else if( sym == COMMA )
    {
        Get(&sym);
        ArrayType(&typ->base);//multidimentional array
    }
    else
    {
        Mark("missing OF");
        typ->base = intType;
    }
    typ->size = (len * typ->base->size + 3) / 4 * 4;
    typ->form = Array;
    typ->len = len;
    *type = typ;
}


void RecordType(Type *type)
{
    Object obj=0, obj0=0, new=0, bot=0, base=0;
    Type typ=0, tp=0;
    int offset, off, n;

    NEW((void **)&typ, sizeof(TypeDesc));
    typ->form = NoTyp;
    typ->base = NIL;
    typ->mno = -level;
    typ->nofpar = 0;
    offset = 0;
    bot = NIL;
    //RecordType = RECORD ["(" BaseType ")"] [FieldListSequence] END.
    if( sym == LPAREN )
    {
        Get(&sym);//record extension
        if( level != 0 )
        {
            Mark("extension of local types not implemented");
        }
        //BaseType = qualident.
        if( sym == IDENT )
        {
            qualident(&base);
            if( base->class == Typ )
            {
                if( base->type->form == Record )
                {
                    typ->base = base->type;
                }
                else
                {
                    typ->base = intType;
                    Mark("invalid extension");
                }
                typ->nofpar = typ->base->nofpar + 1;//"nofpar" here abused for extension level
                bot = typ->base->dsc;
                offset = typ->base->size;
            }
            else
            {
                Mark("type expected");
            }
        }
        else
        {
            Mark("ident expected");
        }
        Check(RPAREN, "no )");
    }



    //FieldListSequence = FieldList {";" FieldList}.
    //FieldList = IdentList ":" type.
    //IdentList = identdef {"," identdef}.
    //identdef = ident ["*"].
    while( sym == IDENT )//fields
    {
        n = 0;
        obj = bot;
        while( sym == IDENT )
        {
            obj0 = obj;
            while( (obj0 != NIL) && (strcmp(obj0->name, id) != 0) )
            {
                obj0 = obj0->next;
            }
            if( obj0 != NIL)
            {
                Mark("mult def");
            }
            NEW((void **)&new, sizeof(ObjDesc));
            CopyId(new->name);
            new->class = Fld;
            new->next = obj;
            obj = new;
            n++;
            Get(&sym);
            CheckExport(&new->expo);
            if( (sym != COMMA) && (sym != COLON) )
            {
                Mark("comma expected");
            }
            else if( sym == COMMA )
            {
                Get(&sym);
            }
        }
        Check(COLON, "colon expected");
        _Type(&tp);
        if( (tp->form == Array) && (tp->len < 0) )
        {
            Mark("dyn array not allowed");
        }
        if( tp->size > 1 )
        {
            offset = (offset+3) / 4 * 4;
        }
        offset = offset + n * tp->size;
        off = offset;
        obj0 = obj;
        while( obj0 != bot )
        {
            obj0->type = tp;
            obj0->lev = 0;
            off = off - tp->size;
            obj0->val = off;
            obj0 = obj0->next;
        }
        bot = obj;

        if( sym == SEMICOLON )
        {
            Get(&sym);

        }
        else if( sym != END )
        {

            Mark(" ; or END");
        }
    }
    typ->form = Record;
    typ->dsc = bot;
    typ->size = (offset + 3) / 4 * 4;
    *type = typ;
}

void FPSection(int *adr, int *nofpar)
{
    Object obj=0, first=0;
    Type tp=0;
    int parsize=0;
    int cl;
    int rdo;

    //FPSection = [VAR] ident {"," ident} ":" FormalType.
    //FormalType = {ARRAY OF} qualident.

    if( sym == VAR )
    {
        Get(&sym);
        cl = Par;
    }
    else
    {
        cl = Var;
    }

    //IdentList = identdef {"," identdef}.
    //identdef = ident ["*"].

    IdentList(cl, &first); //why? FPSection doesn't expect a IdentList...
    FormalType(&tp, 0);
    rdo = FALSE;
    if( (cl == Var) && (tp->form >= Array) )
    {
        cl = Par;
        rdo = TRUE;
    }
    if( ((tp->form == Array) && (tp->len < 0)) || (tp->form == Record) )
    {
        parsize = 2*WordSize; //open array or record, needs second word for length or type tag
    }
    else
    {
        parsize = WordSize;
    }
    obj = first;
    while( obj != NIL )
    {
        (*nofpar)++;
        obj->class = cl;
        obj->type = tp;
        obj->rdo = rdo;
        obj->lev = level;
        obj->val = *adr;
        *adr = *adr + parsize;
        obj = obj->next;
    }
    if( *adr >= 52 )
    {
        Mark("too many parameters");
    }
}


void ProcedureType(Type ptype, int *parblksize)
{
    Object obj=0;
    int size;
    int nofpar;

    ptype->base = noType;
    size = *parblksize;
    nofpar = 0;
    ptype->nofpar = 0;
    ptype->dsc = NIL;

    //ProcedureType = PROCEDURE [FormalParameters].
    //FormalParameters = "(" [FPSection {";" FPSection}] ")" [":" qualident].

    if( sym == LPAREN )
    {
        Get(&sym);
        if( sym == RPAREN )
        {
            Get(&sym);
        }
        else
        {
            FPSection(&size, &nofpar);
            while( sym == SEMICOLON )
            {
                Get(&sym);
                FPSection(&size, &nofpar);
            }
            Check(RPAREN, "no )");
        }
        ptype->nofpar = nofpar;
        *parblksize = size;
        if( sym == COLON )//function
        {
            Get(&sym);
            if( sym == IDENT )
            {
                qualident(&obj);
                ptype->base = obj->type;
                if( !((obj->class == Typ) && (obj->type->form == Byte
                                              || obj->type->form == Bool
                                              || obj->type->form == Char
                                              || obj->type->form == Int
                                              || obj->type->form == Real
                                              || obj->type->form == Set
                                              || obj->type->form == Pointer
                                              || obj->type->form == Proc)) )
                {
                    Mark("illegal function type");
                }
            }
            else
            {
                Mark("type identifier expected");
            }
        }
    }


    //why? if no lparen found, then nofpar of the procedure type should be 0.
    //where it is done in oberon code?

}

void FormalType(Type *typ, int dim)
{
    Object obj=0;
    int dmy;

    //FormalType = {ARRAY OF} qualident | ProcedureType.
    if( sym == IDENT )
    {
        qualident(&obj);
        if( obj->class == Typ )
        {
            *typ = obj->type;
        }
        else
        {
            Mark("not a type");
            *typ = intType;
        }
    }
    else if( sym == ARRAY )
    {
        Get(&sym);
        Check(OF, "OF ?");
        if( dim >= 1 )
        {
            Mark("multi-dimensional open arrays not implemented");
        }
        NEW((void **)typ, sizeof(TypeDesc));
        (*typ)->form = Array;
        (*typ)->len = -1;
        (*typ)->size = 2*WordSize;
        FormalType(&(*typ)->base, dim+1);
    }
    else if( sym == PROCEDURE )
    {
        Get(&sym);
        OpenScope();
        NEW((void **)typ, sizeof(TypeDesc));
        (*typ)->form = Proc;
        (*typ)->size = WordSize;
        dmy = 0;
        ProcedureType(*typ, &dmy);
        (*typ)->dsc = topScope->next;
        CloseScope();
    }
    else
    {
        Mark("identifier expected");
        *typ = noType;
    }
}


void CheckRecLevel(int lev)
{
    if( lev != 0 )
    {
        Mark("ptr base must be global");
    }
}

void _Type(Type *type)
{
    int dmy;
    Object obj=0;
    PtrBase ptbase=0;

    *type = intType;

    //sync
    if( (sym != IDENT) && (sym < ARRAY) )
    {
        Mark("not a type");
        do
        {
            Get(&sym);
        }
        while(!( (sym == IDENT) || (sym >= ARRAY) ));
    }


    //type = qualident | ArrayType | RecordType | PointerType | ProcedureType.
    //qualident = [ident "."] ident.

    if( sym == IDENT )
    {
        qualident(&obj);
        if( obj->class == Typ )
        {
            if( (obj->type != NIL) && (obj->type->form != NoTyp) )
            {
                *type = obj->type;
            }
        }
        else
        {
            Mark("not a type or undefined");
        }

    }
    //ArrayType = ARRAY length {"," length} OF type.
    else if( sym == ARRAY )
    {
        Get(&sym);
        ArrayType(type);

    }
    //RecordType = RECORD ["(" BaseType ")"] [FieldListSequence] END.
    else if( sym == RECORD )
    {
        Get(&sym);
        RecordType(type);

        Check(END, "no END");


    }
    //PointerType = POINTER TO type.
    else if( sym == POINTER )
    {
        Get(&sym);
        Check(TO, "no TO");
        NEW((void **)type, sizeof(TypeDesc));
        (*type)->form = Pointer;
        (*type)->size = WordSize;
        (*type)->base = intType;

        if( sym == IDENT )
        {
            obj = thisObj();
            if( obj != NIL )
            {
                if( (obj->class == Typ) && (obj->type->form == Record || obj->type->form == NoTyp) )
                {
                    CheckRecLevel(obj->lev);
                    (*type)->base = obj->type;
                }
                else if( obj->class == Mod )
                {
                    Mark("external base type not implemented");
                }
                else
                {
                    Mark("no valid base type");
                }
            }
            else
            {
                CheckRecLevel(level);//enter into list of forward references to be fixed in Declarations
                NEW((void **)&ptbase, sizeof(PtrBaseDesc));
                CopyId(ptbase->name);
                ptbase->type = *type;
                ptbase->next = pbsList;
                pbsList = ptbase;
            }
            Get(&sym);
        }
        else
        {
            _Type(&(*type)->base);
            if( (*type)->base->form != Record )
            {
                Mark("must point to record");
            }
            CheckRecLevel(level);
        }

    }
    //ProcedureType = PROCEDURE [FormalParameters].
    else if( sym == PROCEDURE )
    {
        Get(&sym);
        OpenScope();
        NEW((void **)type, sizeof(TypeDesc));
        (*type)->form = Proc;
        (*type)->size = WordSize;
        dmy = 0;
        ProcedureType(*type, &dmy);
        (*type)->dsc = topScope->next;
        CloseScope();
    }
    else
    {
        Mark("illegal type");
    }

    (void)dmy;
}


//consumes ProcedureDeclaration
void ProcedureDecl()
{
    Object proc=0;
    Type type=0;
    char procid[ID_LEN];
    Item x;
    int locblksize, parblksize, L;
    int internal;

    internal = FALSE;
    //DeclarationSequence = [CONST {ConstDeclaration ";"}] [TYPE {TypeDeclaration ";"}] [VAR {VariableDeclaration ";"}] {ProcedureDeclaration ";"}.
    //ProcedureDeclaration = ProcedureHeading ";" ProcedureBody ident.
    //ProcedureHeading = PROCEDURE identdef [FormalParameters].

    Get(&sym);
    if( sym == MUL )
    {
        Get(&sym);
        internal = TRUE; //why?
    }

    //PROCEDURE already consumed in Declarations()
    //identdef = ident ["*"].
    if( sym == IDENT )
    {
        CopyId(procid);
        Get(&sym);
        NewObj(&proc, id, Const);
        parblksize = 4;
        NEW((void **)&type, sizeof(TypeDesc));
        type->form = Proc;
        type->size = WordSize;
        proc->type = type;
        CheckExport(&proc->expo);
        if( proc->expo )
        {
            proc->exno = exno;
            exno++;
        }
        OpenScope();
        level++;
        proc->val = -1;
        type->base = noType;




//FormalParameters = "(" [FPSection {";" FPSection}] ")" [":" qualident].
//FPSection = [VAR] ident {"," ident} ":" FormalType.
//FormalType = {ARRAY OF} qualident.
        ProcedureType(type, &parblksize);  //formal parameter list
//ProcedureDeclaration = ProcedureHeading ";" ProcedureBody ident.
        Check(SEMICOLON, "no ;"); //above semicolon
        locblksize = parblksize;


        //ProcedureBody = DeclarationSequence [BEGIN StatementSequence] [RETURN expression] END.
        Declarations(&locblksize); //above DeclarationSequence

        proc->val = Here() * 4;
        proc->type->dsc = topScope->next;

        //if PROCEDURE found, do recursive call
        if( sym == PROCEDURE )
        {
            L = 0;
            FJump(&L);
            do
            {
                ProcedureDecl();
                Check(SEMICOLON, "no ;");//why? which semicolon?
            }
            while(!( sym != PROCEDURE ));
            FixLink(L);
            proc->val = Here() * 4;
            proc->type->dsc = topScope->next;
        }

        Enter(parblksize, locblksize, internal);

        //ProcedureBody = DeclarationSequence [BEGIN StatementSequence] [RETURN expression] END.
        if( sym == BEGIN )
        {
            Get(&sym);
            StatSequence();
        }

        if( sym == RETURN )
        {

            Get(&sym);
            expression(&x);
            if( type->base == noType )
            {
                Mark("this is not a function");
            }
            else if( !CompTypes(type->base, x.type, FALSE) )
            {
                Mark("wrong result type");
            }
        }
        else if( type->base->form != NoTyp )
        {
            Mark("function without result");
            type->base = noType;
        }
        Return(type->base->form, &x, locblksize, internal);
        CloseScope();
        level--;

        Check(END, "no END");

        if( sym == IDENT )
        {
            if( strcmp(id, procid) != 0 )
            {
                Mark("no match");
            }
            Get(&sym);
        }
        else
        {
            Mark("no proc id");
        }
    }
    internal = FALSE;

    (void)L;
    (void)internal;
    (void)x;

}

void Declarations(int *varsize)
{
    Object obj=0, first=0;
    Item x;
    Type tp=0;
    PtrBase ptbase=0;
    int expo;
    char id[ID_LEN];

    pbsList = NIL;

    //sync
    if( (sym < CONST) && (sym != END) && (sym != RETURN) )
    {
        Mark("declaration?");
        do
        {
            Get(&sym);
        }
        while(!( (sym >= CONST) || (sym == END) || (sym == RETURN) ));
    }

    //DeclarationSequence = [CONST {ConstDeclaration ";"}] [TYPE {TypeDeclaration ";"}] [VAR {VariableDeclaration ";"}] {ProcedureDeclaration ";"}.
    if( sym == CONST )
    {
        Get(&sym);

        //ConstDeclaration = identdef "=" ConstExpression.
        //ConstExpression = expression.
        //identdef = ident ["*"].

        while( sym == IDENT )
        {
            CopyId(id);
            Get(&sym);
            CheckExport(&expo);//consumes "*"
            if( sym == EQL )
            {
                Get(&sym);
            }
            else
            {
                Mark("= ?");
            }
            expression(&x);
            if( (x.type->form == String) && (x.b == 2) )
            {
                StrToChar(&x);
            }
            NewObj(&obj, id, Const);
            obj->expo = expo;
            if( x.mode == Const )
            {
                obj->val = x.a;
                obj->lev = x.b;
                obj->type = x.type;
            }
            else
            {
                Mark("expression not constant");
                obj->type = intType;
            }
            Check(SEMICOLON, "; missing"); //consumes ";"
        }
    }

    if( sym == TYPE )
    {
        Get(&sym);
        //TypeDeclaration = identdef "=" type.
        while( sym == IDENT )
        {
            CopyId(id);
            Get(&sym);
            CheckExport(&expo);//consumes "*"
            if( sym == EQL )
            {
                Get(&sym);
            }
            else
            {
                Mark("=?");
            }
            _Type(&tp);

            NewObj(&obj, id, Typ);
            obj->type = tp;
            obj->expo = expo;
            obj->lev = level;
            if( tp->typobj == NIL )
            {
                tp->typobj = obj;
            }

            if( expo && (obj->type->form == Record) )
            {
                obj->exno = exno;
                exno++;
            }
            else
            {
                obj->exno = 0;
            }
            if( tp->form == Record )
            {
                ptbase = pbsList;//check whether this is base of a pointer type; search and fixup
                while( ptbase != NIL )
                {
                    if( strcmp(obj->name, ptbase->name) == 0 )
                    {
                        ptbase->type->base = obj->type;
                    }
                    ptbase = ptbase->next;
                }
                if( level == 0 )
                {
                    BuildTD(tp, &dc);
                }//type descriptor; len used as its address
            }

            Check(SEMICOLON, "; missing"); //consumes ";"

        }
    }

    if( sym == VAR )
    {
        Get(&sym);
        //VariableDeclaration = IdentList ":" type.
        while( sym == IDENT )
        {
            IdentList(Var, &first);
            _Type(&tp);
            obj = first;
            while( obj != NIL )
            {
                obj->type = tp;
                obj->lev = level;
                if( tp->size > 1 )
                {
                    *varsize = (*varsize + 3) / 4 * 4;
                }
                obj->val = *varsize;
                *varsize = *varsize + obj->type->size;
                if( obj->expo )
                {
                    obj->exno = exno;
                    exno++;
                }
                obj = obj->next;
            }
            Check(SEMICOLON, "; missing");//consumes ";"
        }
    }


    *varsize = (*varsize + 3) / 4 * 4;
    ptbase = pbsList;
    while( ptbase != NIL )
    {
        if( ptbase->type->base->form == Int )
        {
            Mark("undefined pointer base of");
        }
        ptbase = ptbase->next;
    }

    if( (sym >= CONST) && (sym <= VAR) )
    {
        Mark("declaration in bad order");
    }
}

//consume the entire MODULE corrently being compiled
void _Module()
{
    int key;
    char impid[ID_LEN];
    char impid1[ID_LEN];

    Get(&sym); //This is the first Get()
    //now onwards, whenever a desired token is found, do a Get()!

    //module = MODULE ident ";" [ImportList] DeclarationSequence [BEGIN StatementSequence] END ident "." .
    if( sym == MODULE )
    {
        Get(&sym);
        if( sym == MUL )
        {
            version = 0;
            Get(&sym);
        }
        else
        {
            version = 1;
        }

        //initialize symbol table
        InitSymbolTable();
        //creates a new scope
        OpenScope();

        if( sym == IDENT )
        {
            CopyId(modid); //copy module name currently being compiled
            Get(&sym);
            printf("compiling %s\n", modid);
        }
        else
        {
            Mark("identifier expected");
        }
        Check(SEMICOLON, "no ;"); //consume semicolon

        level = 0;
        dc = 0;
        exno = 1;
        key = 0;

        //ImportList = IMPORT import {"," import} ";".
        if( sym == IMPORT )
        {
            Get(&sym);

            while( sym == IDENT )
            {
                CopyId(impid); //copy original name of imported module
                Get(&sym);

                //import = ident [":=" ident].
                if( sym == BECOMES )
                {
                    Get(&sym);
                    if( sym == IDENT )
                    {
                        CopyId(impid1); //copy alias name of imported module
                        Get(&sym);
                    }
                    else
                    {
                        Mark("id expected");
                    }
                }
                else
                {
                    strcpy(impid1, impid); //if impid1 is not given
                }

                //import module
                Import(impid, impid1);

                if( sym == COMMA )
                {
                    Get(&sym);
                }
                else if( sym == IDENT )
                {
                    Mark("comma missing");
                }
            }
            Check(SEMICOLON, "no ;");
        }

        Open(version);

        //consumes DeclarationSequence except ProcedureDeclaration
        Declarations(&dc);

        SetDataSize((dc + 3) / 4 * 4); //aligning to 4 bytes

        while( sym == PROCEDURE )
        {
            ProcedureDecl(); //consumes all ProcedureDeclaration
            Check(SEMICOLON, "no ;"); //consumes semicolon appearing at the end of "... END Procname;"
        }

        Header();

        //module = MODULE ident ";" [ImportList] DeclarationSequence [BEGIN StatementSequence] END ident "." .
        if( sym == BEGIN )
        {
            Get(&sym);
            StatSequence();//consumes StatementSequence
        }

        Check(END, "no END"); //consumes END

        if( sym == IDENT )//consumes ident
        {
            if( strcmp(id, modid) != 0 )
            {
                Mark("no match");
            }
            Get(&sym);
        }
        else
        {
            Mark("identifier missing");
        }

        if( sym != PERIOD ) //consumes "." applearing at the end of "... END Modname."
        {
            Mark("period missing");
        }

		//whole module is parsed,
		//now proceed to create symbol file
        if( (errcnt == 0) && (version != 0) )
        {
			//create symbol file for compiled module
            Export(modid, &newSF, &key);
            if( newSF )
            {
                printf("new symbol file created\n");
            }
        }

        if( errcnt == 0 )
        {
			//create executable file; *.rsc for RISC-5
            Close(modid, key, exno);
        }
        else
        {
            printf("compilation FAILED\n");
        }

		//close module scope
        CloseScope();
        pbsList = NIL;

    }
    else
    {
        Mark("must start with MODULE");
    }
}

//set compiler options
void Option()
{
    newSF = TRUE; //create/overwrite new symbol file
}

void Compile()
{
    Option(); //set compiler options
    initScanner(f, 0);
    _Module();
}
