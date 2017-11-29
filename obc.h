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
extern int slen; //and its length is found here, slen includes '\0' character, so 1 bigger than actual string length

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
//The 'class' of ObjDesc is renamed as 'mode' in Item.
//So they both hold similar values.

//Item modes and meaning of fields:
//mode    r      a       b
//--------------------------------
//Const   -      value   (proc addr)  (immediate value)
//Var     base   off     -            (direct addr)
//Par     -      off0    off1         (indirect addr)
//Fld     -      -       -
//Typ     -      -       -
//--------------------------------
//Reg     regno  -       -
//RegI    regno  off     -
//Cond    cond   Tjump   Fjump        (T=True, F=False)

//Note the similarity of the two types Item and Object.
//Both describe objects, but whereas
//Objects represent declared, named objects,
//whose visibility reaches beyond
//the construct of their declaration,
//Items describe objects which are
//always strictly bound to their syntactic construct.
//Therefore, it is strongly recommended
//not to allocate Items dynamically (in a heap),
//but rather to declare them as local parameters and variables.

//Item helps in doing 1.Type compatibility check,
//2.compiletime expression evaluation
//3.code generation while parsing.

//Item mode reflect the target computer's architecture,
//in particular its addressing modes.
//The more addressing modes a computer offers,
//the more item modes are needed to represent them.


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
//[] means 0 or once; i.e. optional
//{} means 0 or more; i.e. optional but repetatable
//-----------------------
//ActualParameters = "(" [ExpList] ")".
		//ex. myproc(4,6)
//AddOperator = "+" | "-" | OR.
		//ex. + | - | OR
//ArrayType = ARRAY length {"," length} OF type.
		//ex. TYPE k: ARRAY 3+1,4 OF INTEGER;
//assignment = designator ":=" expression.
		//ex. a := 8+9;
//BaseType = qualident.
		//ex. TYPE mytype = POINTER TO mymodule.myrecord;
//case = [CaseLabelList ":" StatementSequence].
		//ex. CASE myvar OF 1,3:Out.String("hi");Out.String("bye")END
//CaseLabelList = LabelRange {"," LabelRange}.
		//ex. CASE myvar OF 1,3..5:Out.String("hi");Out.String("bye") | 6:Out.String("bye");Out.String("hi") END
//CaseStatement = CASE expression OF case {"|" case} END.
		//ex. CASE myvar OF 1:Out.String("hi");Out.String("bye") | 2:Out.String("bye");Out.String("hi") END
//ConstDeclaration = identdef "=" ConstExpression.
		//ex. CONST PI* = 3.142;
//ConstExpression = expression.
		//ex. 5+7
//DeclarationSequence = [CONST {ConstDeclaration ";"}] [TYPE {TypeDeclaration ";"}] [VAR {VariableDeclaration ";"}] {ProcedureDeclaration ";"}.
		//ex. CONST PI* = 3.142; TYPE T* = INTEGER; VAR x: INTEGER; PROCEDURE myproc(x:INTEGER):REAL; VAR y:INTEGER; BEGIN y:=1; RETURN x+y END myproc;
		//note that there is no semicolon at the end of RETURN
//designator = qualident {selector}.
		//ex. myvar | mymodule.myvar | mymodule.myvar[i]
		//see selector for more exmples
//digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9".
		//digits 0 to 9
//element = expression [".." expression].
		//ex. see second element of the set {1, 2..5, n+1..2*k}
//ExpList = expression {"," expression}.
		//ex. multiple expressions separated by comma as in 5,n+2,9
//expression = SimpleExpression [relation SimpleExpression].
		//ex. -6 < k
//factor = number | string | NIL | TRUE | FALSE | set | designator [ActualParameters] | "(" expression ")" | "~" factor.
		//ex. 3 | "hi" | {1, 2} | myvar | myproc(9,6) | (6*7) | ~mybool
//FieldList = IdentList ":" type.
		//ex. x,y,z:INTEGER
//FieldListSequence = FieldList {";" FieldList}.
		//ex. x,y:INTEGER;z:REAL
//FormalParameters = "(" [FPSection {";" FPSection}] ")" [":" qualident].
		//ex. PROCEDURE myproc(VAR x,y:INTEGER; z:CHAR):REAL;
//FormalType = {ARRAY OF} qualident | ProcedureType.
		//ex. PROCEDURE myproc(VAR x,y:INTEGER):REAL;
		//ex. PROCEDURE myproc(x,y: ARRAY OF module.type):REAL;
		//ex. PROCEDURE myproc(VAR x,y: PROCEDURE):REAL;
		//ex. PROCEDURE myproc(x,y: PROCEDURE(a,b:INTEGER):INTEGER):REAL;
//ForStatement = FOR ident ":=" expression TO expression [BY ConstExpression] DO StatementSequence END.
		//ex. FOR i:=3 TO 9 BY 2 DO k:=k+1 END
//FPSection = [VAR] ident {"," ident} ":" FormalType.
		//ex. PROCEDURE myproc(VAR x,y:INTEGER):REAL;
//hexDigit = digit | "A" | "B" | "C" | "D" | "E" | "F".
		//ex. 3BE4
//identdef = ident ["*"].
		//ex. VAR k*:INTEGER;
//ident = letter {letter | digit}.
		//ex. class5mark
//IdentList = identdef {"," identdef}.
		//ex. VAR x*, y* : INTEGER;
//IfStatement = IF expression THEN StatementSequence {ELSIF expression THEN StatementSequence} [ELSE StatementSequence] END.
		//ex. IF a=0 THEN b:=9 ELSIF a=1 THEN b:=8 ELSIF a=2 THEN b:=7 ELSE b:=6 END
//import = ident [":=" ident].
		//ex. IMPORT mymodule := mm;
//ImportList = IMPORT import {"," import} ";".
		//ex. IMPORT mymodule1, mymodule2;
//integer = digit {digit} | digit {hexDigit} "H".
		//ex. 32 | 0BH | 5BH
		//note that hexadecimal numbers start with a digit, ends with an H.
//label = integer | string | qualident.
		//ex. 43 | "hi" | myrecord.myelement
		//ex. CASE myvar OF "hi":Out.String("hi")END
//LabelRange = label [".." label].
		//ex. CASE myvar OF 3..5:Out.String("hi")END
//length = ConstExpression.
		//ex. 5+7
//letter = "A" | "B" | ... | "Z" | "a" | "b" | ... | "z".
		//ex. capital and small letters
//module = MODULE ident ";" [ImportList] DeclarationSequence [BEGIN StatementSequence] END ident "." .
		//ex. MODULE mymod; IMPORT mymodule; VAR x:INTEGER; BEGIN x:=9 END mymod .
		//note that there is a full stop mark at the END of module identifier.
//MulOperator = "*" | "/" | DIV | MOD | "&".
		//ex. * | / | DIV | MOD | &
//number = integer | real.
		//ex. 34 | 3.4
//PointerType = POINTER TO type.
		//ex. TYPE x: POINTER TO myrecord;
//ProcedureBody = DeclarationSequence [BEGIN StatementSequence] [RETURN expression] END.
		//ex. PROCEDURE myproc(x:INTEGER):REAL; VAR y:INTEGER; BEGIN y:=1; RETURN x+y END myproc;
//ProcedureCall = designator [ActualParameters].
		//ex. myproc | myfunc(4,6)
//ProcedureDeclaration = ProcedureHeading ";" ProcedureBody ident.
		//ex. PROCEDURE myproc(x:INTEGER):REAL; VAR y:INTEGER; BEGIN y:=1; RETURN x+y END myproc;
//ProcedureHeading = PROCEDURE identdef [FormalParameters].
		//ex. PROCEDURE myproc*(x:INTEGER):REAL; VAR y:INTEGER; BEGIN y:=1; RETURN x+y END myproc;
//ProcedureType = PROCEDURE [FormalParameters].
		//ex. TYPE mp: PROCEDURE(a,b:INTEGER):INTEGER;
//qualident = [ident "."] ident.
		//ex. mymodule.myvar | myvar
//real = digit {digit} "." {digit} [ScaleFactor].
		//ex. 34.4 | 34.4E-21
//RecordType = RECORD ["(" BaseType ")"] [FieldListSequence] END.
		//ex. TYPE mytype = RECORD (basetype) x,y:INTEGER; z:CHAR END;
//relation = "=" | "#" | "<" | "<=" | ">" | ">=" | IN | IS.
		//ex. = | # | < | <= | > | >= | IN | IS
//RepeatStatement = REPEAT StatementSequence UNTIL expression.
		//ex. REPEAT a:=a+1 UNTIL a>10
//ScaleFactor = "E" ["+" | "-"] digit {digit}.
		//ex. 34.4E-21
//selector = "." ident | "[" ExpList "]" | "^" | "(" qualident ")".
		//ex. record.element | array[rowindex, columnindex] | recordpointer^.element (= recordpointer.element) | p(Circle).radius (here p is of type Figure)
//set = "{" [element {"," element}] "}".
		//ex. a set is {1, 2..5, n+1..2*k}
//SimpleExpression = ["+" | "-"] term {AddOperator term}.
		//ex. -3 | -3 + 4 | -3 - 4
//statement = [assignment | ProcedureCall | IfStatement | CaseStatement | WhileStatement | RepeatStatement | ForStatement].
		//ex. a:=1 | myproc | myproc(3) | IF a=0 THEN b:=9 END | CASE myvar OF 1: Out.String("hi") END | WHILE a>9 DO b:=1 END | REPEAT a:=a+1 UNTIL a>10 | FOR i:=3 TO 9 BY 2 DO k:=k+1 END
//StatementSequence = statement {";" statement}.
		//ex. a:=2;b:=6
//string = """ {character} """ | digit {hexDigit} "X".
		//ex. "abcd" | 3BX
		//note that hex string starts with a digit, ends with an X.
//term = factor {MulOperator factor}.
		//ex. 3*4 | 5/8
//TypeDeclaration = identdef "=" type.
		//ex. TYPE T* = INTEGER;
//type = qualident | ArrayType | RecordType | PointerType | ProcedureType.
		//ex. TYPE k: mymod.mytype; | TYPE k: ARRAY 3+1,4 OF INTEGER; | TYPE mytype = RECORD (basetype) x,y:INTEGER; z:CHAR END; | TYPE x: POINTER TO myrecord; | TYPE mp: PROCEDURE(a,b:INTEGER):INTEGER;
//VariableDeclaration = IdentList ":" type.
		//ex. VAR x,y: INTEGER;
//WhileStatement = WHILE expression DO StatementSequence {ELSIF expression DO StatementSequence} END.
		//ex. WHILE a>9 DO b:=1 ELSIF a>5 DO b:=2 ELSIF a>3 DO b:=3 END
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
//type =      -ref | ref (PTR basetype
//                      | ARR basetype len size
//                      | PRO basetype {param} 0).
//                      | REC basetype exno nofpar size {field} 0
//param =                              (VAR | PAR) rdo type.
//field =                                                FLD name type offset.
//basetype = type
//-------------------


//Meaning of addressing modes:
//----------------------------
//Immediate: here is the value itself
//Direct: value is at this address
//Register: value is in this register
//RegisterIndirect: value is at address contained in this register
