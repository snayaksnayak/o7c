//common header file

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TRUE 1
#define FALSE 0
#define NIL 0

//****interface to scanner****

#define ID_LEN 32 //maximum identifier length
extern FILE* f; //source file (.Mod)

extern char id[]; //If scanner Get(sym) returns IDENT, then identifier name (which is a string) is found here
extern int ival; //If scanner Get(sym) returns INT or CHAR, then value of that integer or character literal is found here
extern float rval; //If scanner Get(sym) returns REAL, then value of that real literal is found here
extern char str[]; //If scanner Get(sym) returns STRING, then value of that string literal is found here
extern int slen; //and its length is found here

extern int errcnt; //scanner keeps count of errors reported
extern int k; //index for keytab[]
//keytab[] contains keyword number and keyword name.
//kwx[] contains indexes of keytab[]
//if kwx[3]=5 and kwx[4]=9, it means,
//4 letter keywords are listed from keytab[5] to keytab[8]
//that means to find n letter keywords, search
//for (k=kwx[n-1]; k<kwx[n]; k++){ if keytab[k] matches or not...}
extern int kwx[];

//we associate each token with a number
//these cnstants represent token number
extern int MUL, RDIV, DIV, MOD, AND,
       PLUS, MINUS, OR, EQL, NEQ,
       LSR, LEQ, GTR, GEQ, IN,
       IS, ARROW, PERIOD,          CHAR,
       INT, REAL, FALS, TRU, NILL,
       STRING, NOT, LPAREN, LBRAK, LBRACE,
       IDENT, IF, WHILE,            REPEAT,
       CASE, FOR,                        COMMA,
       COLON, BECOMES, UPTO, RPAREN, RBRAK,
       RBRACE, THEN, OF, DO, TO,
       BY, SEMICOLON, END, BAR, ELSE,
       ELSIF, UNTIL, RETURN,           ARRAY,
       RECORD, POINTER, CONST, TYPE, VAR,
       PROCEDURE, BEGIN, IMPORT, MODULE, EOT;

void initObs();
void initScanner(FILE* f, int pos);
void enter_kw(int sym, char* name);
void Get(int *sym);
void Mark(char * msg);
void CopyId(char *ident);
int pos();
void WriteString(FILE *R, char *buf);
void MakeFileName(char *FName, char *name, char *ext);
void Write(FILE *R, int x);

//****interface to symbol table****

extern int versionkey;
//we associate each program element with a number
//these constants represent program element number
extern int Head, Const, Var, Par, Fld, Typ,
       SProc, SFunc, Mod;

//we associate each predefined datatype with a number
//these constants represent predefined datatype number
extern int Byte, Bool, Char, Int, Real, Set,
       Pointer, NilTyp, NoTyp, Proc,
       String, Array, Record;

//forward declaration
typedef struct ObjDesc ObjDesc, *Object;
typedef struct ModDesc ModDesc, *Module;
typedef struct TypeDesc TypeDesc, *Type;

//each program element is described by this structure
//that means in symbol table, we have such descriptions
//for each program elements; aka objects
typedef struct ObjDesc
{
    int class; //class=Const, class=Var, class=Par, class=Typ etc...
    int lev;
    int exno; //export number; each exported program element is assigned a number
    int expo; //boolean; denotes if exported with '*' mark
    int rdo; //boolean; denotes if read-only
    Object next;
    Object dsc;
    Type type;
    char name[ID_LEN]; //identifier name
    int val; //this is not the value of identifier as seen to the programmer
    //Object classes and the meaning of "val":
    //    class    val
    //    ------------
    //    Var      address
    //    Par      address
    //    Const    value
    //    Fld      offset
    //    Typ      type descriptor (TypeDesc) address
    //    SProc    inline code number
    //    SFunc    inline code number
    //    Mod      key
} ObjDesc, *Object;

//derived from ObjDesc
typedef struct ModDesc
{
    ObjDesc objdesc;
    char orgname[ID_LEN]; //while importing a module, we may assign an alias to it.
    //in that case, this field holds the original module name.
} ModDesc, *Module;

typedef struct TypeDesc
{
    int form; //form=Bool, form=Char, form=Int, form=Set, form=Pointer,
    //form=Proc, form=String, form=Array, form=Record etc.
    int ref;  //ref is only used while importing/exporting
    //for ref=Byte, form=Int. It says if variable
    //is of type Byte, it is internally represented as an Int.
    //Hence, type BYTE is compatible with type INTEGER, and vice-versa.
    //For a predefined data type, ref is that data type, form is its internal representation
    int mno;
    int nofpar;  //for procedures, extension level for records
    int len;  //for arrays, len < 0 => open array; for records: adr of descriptor
    Object dsc;
    Object typobj;
    Type base;
    //Type forms and the meaning of "dsc" and "base":
    //    form     dsc      base
    //    ------------------------
    //    Pointer  -        type of dereferenced object
    //    Proc     params   result type
    //    Array    -        type of elements
    //    Record   fields   extension
    //    others   -        0
    int size;  //in bytes; always multiple of 4, except for Byte, Bool and Char
} TypeDesc, *Type;

extern Object topScope, System;
extern Type byteType, boolType, charType, intType, realType, setType, nilType, noType, strType;

void initObt();
void InitSymbolTable();
void NEW(void **p, int size);
void NewObj(Object *obj, char *id, int class);
Object thisObj();
Object thisimport(Object mod);
Object thisfield(Type rec);
void OpenScope();
void CloseScope();
void MakeFileName(char *FName, char *name, char *ext);
void Import(char *modid, char *modid1);
void Export(char* modid, int *newSF, int *key);

//****interface to parser****

void initObp();
void Compile();

//****interface to code generator****

typedef struct Item
{
    int mode;
    Type type;
    int a, b, r;
    int rdo; //read only
} Item;

extern int WordSize;
extern int pc;

void initObg();
void MakeConstItem(Item *x, Type typ, int val);
void MakeRealItem(Item *x, float val);
void MakeStringItem(Item *x, int len); //copies string from ORS-buffer to ORG-string array
void MakeItem(Item *x, Object y, int curlev);
void Open(int v);
void SetDataSize(int dc);
void Header();
void StrToChar(Item *x);
void BuildTD(Type T, int *dc);
int Here();
void FixLink(int L);
void FJump(int *L);
void CheckRegs();
void Field(Item* x, Object y);
void DeRef(Item* x);
void Index(Item* x, Item* y);
void _TypeTest(Item* x, Type T, int varpar, int isguard);
void Not(Item* x);
void And1(Item* x);
void And2(Item* x, Item* y);
void Or1(Item* x);
void Or2(Item* x, Item* y);
void Neg(Item* x);
void AddOp(int op, Item* x, Item *y);
void MulOp(Item* x, Item* y);
void DivOp(int op, Item* x, Item* y);
void RealOp(int op, Item* x, Item* y);
void Singleton(Item* x);
void _Set(Item* x, Item* y);
void In(Item* x, Item* y);
void SetOp(int op, Item* x, Item* y);
void IntRelation(int op, Item* x, Item* y);
void RealRelation(int op, Item* x, Item* y );
void StringRelation(int op, Item* x, Item* y);
void StrToChar(Item* x );
void Store(Item* x, Item* y);
void StoreStruct(Item* x, Item* y);
void CopyString(Item*x, Item* y);
void VarParam(Item* x, Type ftype);
void ValueParam(Item* x);
void OpenArrayParam(Item* x);
void StringParam(Item* x);
void For0(Item* x, Item* y);
void For1(Item* x, Item* y, Item* z, Item* w, int* L);
void For2(Item* x, Item* y, Item* w);
void CFJump(Item* x);
void BJump(int L);
void CBJump(Item* x, int L);
void Fixup(Item* x);
void PrepCall(Item* x, int* r);
void Call(Item* x, int r);
void Enter(int parblksize, int locblksize, int internal);
void Return(int form, Item* x, int size, int internal);
void Increment(int upordown, Item* x, Item* y);
void Include(int inorex, Item* x, Item* y);
void Assert(Item* x);
void New(Item* x);
void Pack(Item* x, Item* y);
void Unpk(Item* x, Item* y);
void Led(Item* x);
void _Get(Item* x, Item* y);
void Put(Item* x, Item* y);
void Copy(Item* x, Item* y , Item* z);
void LDPSR(Item* x);
void LDREG(Item* x, Item* y);
void Abs(Item* x);
void Odd(Item* x);
void Floor(Item* x);
void Float(Item* x);
void Ord(Item* x);
void Len(Item* x);
void Shift(int fct, Item* x, Item* y);
void ADC(Item* x, Item* y);
void SBC(Item* x, Item* y);
void UML(Item* x, Item* y);
void Bit(Item* x, Item* y);
void Register(Item* x);
void H(Item* x);
void Adr(Item* x);
void Condition(Item* x);
void Close(char* modid, int key, int nofent);





