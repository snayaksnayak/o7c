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
//we associate each type of objects with a number
//these constants represent types of objects
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

//each variable or type is described by this structure in symbol table
//collectively variables, types, procedures, constants are refered as objects
typedef struct ObjDesc
{
    int class; //class=Const, class=Var, class=Par, class=Typ etc...
    int lev; //import order number of a module = modules's level
    int exno; //export number; each exported variable is assigned a number
    int expo; //boolean; denotes if exported with '*' mark
    int rdo; //boolean; denotes if read-only
    Object next; //all variables, types, constants, procedures, parameters, fields are connected through this
    Object dsc; //module object's dsc holds its variables and types
    Type type; //type of variables
    char name[ID_LEN]; //identifier name
    int val; //this is not the value of identifier as seen to the programmer
	//Object classes and the meaning of "val":
	//    class    val
	//    ----------
	//    Var      address
	//    Par      address
	//    Const    value
	//    Fld      offset
	//    Typ      type descriptor (TypeDesc) address? seems 0 instead
	//    SProc    built-in procedure number
	//    SFunc    built-in function number
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
    int ref; //ref is used while importing/exporting
    //this is used as index to typtab[] to get TypeDesc.
    //each type(primary/secondary) is assigned ref from a gloabally incrementing counter
    //for ref=Byte, we have form=Int. It says if variable
    //is of type Byte, it is internally represented as an Int.
    //Hence, type BYTE is compatible with type INTEGER, and vice-versa.
    int mno; //import order number of a module = modules's level
    int nofpar; //number of params for procedures, extension level for records
    int len; //number of elements for arrays; len < 0 => open array; addr of descriptor for records
    Object dsc; //Module object uses this to list all its symbols
    Object typobj; //ObjDesc
    Type base;
    //Type forms and the meaning of "dsc" and "base":
    //    form     dsc      base
    //    ------------------------
    //    Pointer  -        type of dereferenced object
    //    Proc     params   result type
    //    Array    -        type of elements
    //    Record   fields   extension
    //    others   -        0
    int size; //in bytes; always multiple of 4, except for Byte, Bool and Char
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
//Item forms and meaning of fields:
//mode    r      a       b
//--------------------------------
//Const   -     value (proc adr)  (immediate value)
//Var     base   off     -               (direct adr)
//Par      -     off0     off1         (indirect adr)
//Reg    regno
//RegI   regno   off     -
//Cond  cond   Fchain  Tchain  *)

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


//Oberon7 syntax in EBNF:
//-----------------------
//[] means optional, i.e. 0 or once
//{} means 0 or more
//-----------------------
//ActualParameters = "(" [ExpList] ")".
//AddOperator = "+" | "-" | OR.
//ArrayType = ARRAY length {"," length} OF type.
//assignment = designator ":=" expression.
//BaseType = qualident.
//case = [CaseLabelList ":" StatementSequence].
//CaseLabelList = LabelRange {"," LabelRange}.
//CaseStatement = CASE expression OF case {"|" case} END.
//ConstDeclaration = identdef "=" ConstExpression.
//ConstExpression = expression.
//DeclarationSequence = [CONST {ConstDeclaration ";"}] [TYPE {TypeDeclaration ";"}] [VAR {VariableDeclaration ";"}] {ProcedureDeclaration ";"}.
//designator = qualident {selector}.
//digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9".
//element = expression [".." expression].
//ExpList = expression {"," expression}.
//expression = SimpleExpression [relation SimpleExpression].
//factor = number | string | NIL | TRUE | FALSE | set | designator [ActualParameters] | "(" expression ")" | "~" factor.
//FieldList = IdentList ":" type.
//FieldListSequence = FieldList {";" FieldList}.
//FormalParameters = "(" [FPSection {";" FPSection}] ")" [":" qualident].
//FormalType = {ARRAY OF} qualident | ProcedureType.
//ForStatement = FOR ident ":=" expression TO expression [BY ConstExpression] DO StatementSequence END.
//FPSection = [VAR] ident {"," ident} ":" FormalType.
//hexDigit = digit | "A" | "B" | "C" | "D" | "E" | "F".
//identdef = ident ["*"].
//ident = letter {letter | digit}.
//IdentList = identdef {"," identdef}.
//IfStatement = IF expression THEN StatementSequence {ELSIF expression THEN StatementSequence} [ELSE StatementSequence] END.
//import = ident [":=" ident].
//ImportList = IMPORT import {"," import} ";".
//integer = digit {digit} | digit {hexDigit} "H".
//label = integer | string | qualident.
//LabelRange = label [".." label].
//length = ConstExpression.
//letter = "A" | "B" | ... | "Z" | "a" | "b" | ... | "z".
//module = MODULE ident ";" [ImportList] DeclarationSequence [BEGIN StatementSequence] END ident "." .
//MulOperator = "*" | "/" | DIV | MOD | "&".
//number = integer | real.
//PointerType = POINTER TO type.
//ProcedureBody = DeclarationSequence [BEGIN StatementSequence] [RETURN expression] END.
//ProcedureCall = designator [ActualParameters].
//ProcedureDeclaration = ProcedureHeading ";" ProcedureBody ident.
//ProcedureHeading = PROCEDURE identdef [FormalParameters].
//ProcedureType = PROCEDURE [FormalParameters].
//qualident = [ident "."] ident.
//real = digit {digit} "." {digit} [ScaleFactor].
//RecordType = RECORD ["(" BaseType ")"] [FieldListSequence] END.
//relation = "=" | "#" | "<" | "<=" | ">" | ">=" | IN | IS.
//RepeatStatement = REPEAT StatementSequence UNTIL expression.
//ScaleFactor = "E" ["+" | "-"] digit {digit}.
//selector = "." ident | "[" ExpList "]" | "^" | "(" qualident ")".
//set = "{" [element {"," element}] "}".
//SimpleExpression = ["+" | "-"] term {AddOperator term}.
//statement = [assignment | ProcedureCall | IfStatement | CaseStatement | WhileStatement | RepeatStatement | ForStatement].
//StatementSequence = statement {";" statement}.
//string = """ {character} """ | digit {hexDigit} "X".
//term = factor {MulOperator factor}.
//TypeDeclaration = identdef "=" type.
//type = qualident | ArrayType | RecordType | PointerType | ProcedureType.
//VariableDeclaration = IdentList ":" type.
//WhileStatement = WHILE expression DO StatementSequence {ELSIF expression DO StatementSequence} END.
//-----------------------

//Recursive Parsing Strategy:
//---------------------------
//							  K -> Pr(K)
//							  ----------
//							"x"	-> IF sym == "x" THEN next ELSE error END
//						  (exp) -> Pr(exp)
//						  [exp] -> IF sym IN first(exp) THEN Pr(exp) END
//						  {exp} -> WHILE sym IN first(exp) DO Pr(exp) END
//			 fac0 fac1 ... facN -> Pr(fac0); Pr(fac1); ... Pr(facN);
//	term0 | term1 | ... | termN	-> CASE sym OF
//									  first(term0): Pr(term0)
//									| first(term1): Pr(term1)
//									...
//									| first(termN): Pr(termN)
//								   END
//---------------------------


//Symbol file syntax:
//-------------------
//SymFile = null key name versionkey {object}.
//object = (CON name type (value | exno) //exno is for procedures!
//        | TYP name type [{fix} 0]
//        | VAR name type exno).
//type =      -ref | ref (PTR btype
//                      | ARR btype len size
//                      | PRO btype {param} 0).
//                      | REC btype exno nofpar size {field} 0
//param =                           (VAR | PAR) rdo type.
//field =                                             FLD name type offset.
//btype = type
//-------------------

