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
    int lev; //import order number of a module = modules's level; also used to denote string length
    int exno; //export number; each exported variable is assigned a number
    int expo; //boolean; denotes if exported with '*' mark
    int rdo; //boolean; denotes if read-only
    Object next; //all variables, types, constants, procedures, parameters, fields are connected through this
    Object dsc; //module object's dsc holds its constants, types and variables, procedures
    Type type; //type of vars, consts etc
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
    int len; //number of elements for arrays; (len < 0) => open array; addr of descriptor for records
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
//mode     type->form    r      a             b
//----------------------------------------------------------
//Const    Int           -      value         -               (immediate value)
//Const    Real          -      float value   -
//Const    Char          -      ascii value   -
//Const    NilTyp        -      0             -
//Const    String        -      str addr      len(str+'\0')
//Const    Bool          -      0/1           -
//Const    Proc          -      -             (proc addr)
//Var                    base   off           -                   (direct addr)
//Par                    -      off0          off1              (indirect addr)
//Fld                    -      -             -
//Typ                    -      -             -
//----------------------------------------------------------
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


//Oberon7 EBNF grammar and First & Follow sets
//--------------------------------------------
//Tool used: http://homepage.divms.uiowa.edu/~jones/compiler/gtools/
//Setup instructions:
//# sh < gtools.shar
//# make all
//Oberon7 EBNF grammar to input to the tool:
//(first line denote start symbol for the grammar,
//second line denote epsilon symbol for the grammar.)
//[] means 0 or once; i.e. optional
//{} means 0 or more; i.e. optional but repetatable
//letter and digit has not been defined here due to its intuitiveness

/*
> module
/ E
ActualParameters = '(' [ ExpList ] ')'
AddOperator = '+' | '-' | OR
ArrayType = ARRAY length { ',' length } OF type
assignment = designator ':=' expression
BaseType = qualident
case = [ CaseLabelList ':' StatementSequence ]
CaseLabelList = LabelRange { ',' LabelRange }
CaseStatement = CASE expression OF case { '|' case } END
character = letter | digit
ConstDeclaration = identdef '=' ConstExpression
ConstExpression = expression
DeclarationSequence = [ CONST { ConstDeclaration ';' } ] [ TYPE { TypeDeclaration ';' } ] [ VAR { VariableDeclaration ';' } ] { ProcedureDeclaration ';' }
designator = qualident { selector }
element = expression [ '..' expression ]
ExpList = expression { ',' expression }
expression = SimpleExpression [ relation SimpleExpression ]
factor = number | string | NIL | TRUE | FALSE | set | designator [ ActualParameters ] | '(' expression ')' | '~' factor
FieldList = IdentList ':' type
FieldListSequence = FieldList { ';' FieldList }
FormalParameters = '(' [ FPSection { ';' FPSection } ] ')' [ ':' qualident ]
FormalType = { ARRAY OF } qualident | ProcedureType
ForStatement = FOR ident ':=' expression TO expression [ BY ConstExpression ] DO StatementSequence END
FPSection = [ VAR ] ident { ',' ident } ':' FormalType
hexDigit = digit | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
identdef = ident [ '*' ]
ident = letter { letter | digit }
IdentList = identdef { ',' identdef }
IfStatement = IF expression THEN StatementSequence { ELSIF expression THEN StatementSequence } [ ELSE StatementSequence ] END
import = ident [ ':=' ident ]
ImportList = IMPORT import { ',' import } ';'
integer = digit { digit } | digit { hexDigit } 'H'
label = integer | string | qualident
LabelRange = label [ '..' label ]
length = ConstExpression
module = MODULE ident ';' [ ImportList ] DeclarationSequence [ BEGIN StatementSequence ] END ident '.'
MulOperator = '*' | '/' | DIV | MOD | '&'
number = integer | real
PointerType = POINTER TO type
ProcedureBody = DeclarationSequence [ BEGIN StatementSequence ] [ RETURN expression ] END
ProcedureCall = designator [ ActualParameters ]
ProcedureDeclaration = ProcedureHeading ';' ProcedureBody ident
ProcedureHeading = PROCEDURE identdef [ FormalParameters ]
ProcedureType = PROCEDURE [ FormalParameters ]
qualident = [ ident '.' ] ident
real = digit { digit } '.' { digit } [ ScaleFactor ]
RecordType = RECORD [ '(' BaseType ')' ] [ FieldListSequence ] END
relation = '=' | '#' | '<' | '<=' | '>' | '>=' | IN | IS
RepeatStatement = REPEAT StatementSequence UNTIL expression
ScaleFactor = 'E' [ '+' | '-' ] digit { digit }
selector = '.' ident | '[' ExpList ']' | '^' | '(' qualident ')'
set = '{' [ element { ',' element } ] '}'
SimpleExpression = [ '+' | '-' ] term { AddOperator term }
statement = [ assignment | ProcedureCall | IfStatement | CaseStatement | WhileStatement | RepeatStatement | ForStatement ]
StatementSequence = statement { ';' statement }
string = '"' { character } '"' | digit { hexDigit } 'X'
term = factor { MulOperator factor }
TypeDeclaration = identdef '=' type
type = qualident | ArrayType | RecordType | PointerType | ProcedureType
VariableDeclaration = IdentList ':' type
WhileStatement = WHILE expression DO StatementSequence { ELSIF expression DO StatementSequence } END
*/

//Commands to get First & Follow set:
//# ./gdeebnf < my.ebnf > my.bnf_e
//# ./gdeebnf < my.ebnf | ./gdeempty > my.bnf
//# ./gdeebnf < my.ebnf | ./gdeempty | ./gstartfollow > my.sf
//First, input EBNF is converted by gdeebnf to BNF grammar with epsilon.
//Then, epsilon is removed from the BNF grammar by gdeempty.
//Then, First & Follow set is generated by gstartfollow.

#if 0

//Below we list
//each production from EBNF grammar,
//some examples for it,
//its equivalent epsilon-less BNF grammar and
//its First & Follow set.

# terminals:   MODULE ';' END '.' letter digit IMPORT ':=' ',' CONST '=' '*' '+'
#              '-' NIL TRUE FALSE '(' ')' '~' 'H' 'A' 'B' 'C' 'D' 'E' 'F' '"'
#              'X' '{' '}' '..' '[' ']' '^' '/' DIV MOD '&' OR '#' '<' '<=' '>'
#              '>=' IN IS TYPE ARRAY OF RECORD ':' POINTER TO PROCEDURE VAR
#              BEGIN IF THEN ELSIF ELSE CASE '|' WHILE DO REPEAT UNTIL FOR BY
#              RETURN

//ActualParameters = '(' [ ExpList ] ')'
		//ex. myproc(4,6)
ActualParameters ::= '(' ActualParameters-a ')'
                  |  '(' ')'
# start set:   '('
# follow set:  '*' '/' DIV MOD '&' '+' '-' OR '=' '#' '<' '<=' '>' '>=' IN IS
#              ')' '..' ',' THEN OF DO TO BY ';' ELSIF END ELSE UNTIL RETURN '}'
#              ']' '|'

ActualParameters-a ::= ExpList
# start set:   '+' '-' digit NIL TRUE FALSE '(' '~' '"' '{' letter
# follow set:  ')'

//AddOperator = '+' | '-' | OR
		//ex. + | - | OR
AddOperator ::= '+'
             |  '-'
             |  OR
# start set:   '+' '-' OR
# follow set:  digit NIL TRUE FALSE '(' '~' '"' '{' letter

//ArrayType = ARRAY length { ',' length } OF type
		//ex. TYPE k: ARRAY 3+1,4 OF INTEGER;
ArrayType ::= ARRAY length ArrayType-a OF type
           |  ARRAY length OF type
# start set:   ARRAY
# follow set:  ';' END

ArrayType-a ::= ',' length ArrayType-a
             |  ',' length
# start set:   ','
# follow set:  OF

//assignment = designator ':=' expression
		//ex. a := 8+9;
assignment ::= designator ':=' expression
# start set:   letter
# follow set:  ';' ELSIF END ELSE UNTIL RETURN '|'

//BaseType = qualident
		//ex. TYPE mytype = POINTER TO mymodule.myrecord;
BaseType ::= qualident
# start set:   letter
# follow set:  ')'

//case = [ CaseLabelList ':' StatementSequence ]
		//ex. CASE myvar OF 1,3:Out.String("hi");Out.String("bye")END
case ::= case-a
# start set:   digit '"' letter
# follow set:  '|' END

case-a ::= CaseLabelList ':' StatementSequence
        |  CaseLabelList ':'
# start set:   digit '"' letter
# follow set:  '|' END

//CaseLabelList = LabelRange { ',' LabelRange }
		//ex. CASE myvar OF 1,3..5:Out.String("hi");Out.String("bye") | 6:Out.String("bye");Out.String("hi") END
CaseLabelList ::= LabelRange CaseLabelList-a
               |  LabelRange
# start set:   digit '"' letter
# follow set:  ':'

CaseLabelList-a ::= ',' LabelRange CaseLabelList-a
                 |  ',' LabelRange
# start set:   ','
# follow set:  ':'

//CaseStatement = CASE expression OF case { '|' case } END
		//ex. CASE myvar OF 1:Out.String("hi");Out.String("bye") | 2:Out.String("bye");Out.String("hi") END
CaseStatement ::= CASE expression OF case CaseStatement-a END
               |  CASE expression OF case END
               |  CASE expression OF END
               |  CASE expression OF CaseStatement-a END
# start set:   CASE
# follow set:  ';' ELSIF END ELSE UNTIL RETURN '|'

CaseStatement-a ::= '|' case CaseStatement-a
                 |  '|' case
                 |  '|'
                 |  '|' CaseStatement-a
# start set:   '|'
# follow set:  END

//character = letter | digit
		//ex. A, 6
character ::= letter
           |  digit
# start set:   letter digit
# follow set:  letter digit '"'

//ConstDeclaration = identdef '=' ConstExpression
		//ex. CONST PI* = 3.142;
ConstDeclaration ::= identdef '=' ConstExpression
# start set:   letter
# follow set:  ';'

//ConstExpression = expression
		//ex. 5+7
ConstExpression ::= expression
# start set:   '+' '-' digit NIL TRUE FALSE '(' '~' '"' '{' letter
# follow set:  ';' ',' OF DO

//DeclarationSequence = [ CONST { ConstDeclaration ';' } ] [ TYPE { TypeDeclaration ';' } ] [ VAR { VariableDeclaration ';' } ] { ProcedureDeclaration ';' }
		//ex. CONST PI* = 3.142; TYPE T* = INTEGER; VAR x: INTEGER; PROCEDURE myproc(x:INTEGER):REAL; VAR y:INTEGER; BEGIN y:=1; RETURN x+y END myproc;
		//note that there is no semicolon at the end of RETURN
DeclarationSequence ::= DeclarationSequence-a DeclarationSequence-b
                        DeclarationSequence-c DeclarationSequence-d
                     |  DeclarationSequence-a DeclarationSequence-b
                        DeclarationSequence-c
                     |  DeclarationSequence-a DeclarationSequence-b
                     |  DeclarationSequence-a
                     |  DeclarationSequence-b
                     |  DeclarationSequence-a DeclarationSequence-c
                     |  DeclarationSequence-c
                     |  DeclarationSequence-b DeclarationSequence-c
                     |  DeclarationSequence-a DeclarationSequence-b
                        DeclarationSequence-d
                     |  DeclarationSequence-a DeclarationSequence-d
                     |  DeclarationSequence-d
                     |  DeclarationSequence-b DeclarationSequence-d
                     |  DeclarationSequence-a DeclarationSequence-c
                        DeclarationSequence-d
                     |  DeclarationSequence-c DeclarationSequence-d
                     |  DeclarationSequence-b DeclarationSequence-c
                        DeclarationSequence-d
# start set:   CONST TYPE VAR PROCEDURE
# follow set:  BEGIN END RETURN

DeclarationSequence-a ::= CONST DeclarationSequence-a-a
                       |  CONST
# start set:   CONST
# follow set:  TYPE VAR PROCEDURE BEGIN END RETURN

DeclarationSequence-a-a ::= ConstDeclaration ';' DeclarationSequence-a-a
                         |  ConstDeclaration ';'
# start set:   letter
# follow set:  TYPE VAR PROCEDURE BEGIN END RETURN

DeclarationSequence-b ::= TYPE DeclarationSequence-b-a
                       |  TYPE
# start set:   TYPE
# follow set:  VAR PROCEDURE BEGIN END RETURN

DeclarationSequence-b-a ::= TypeDeclaration ';' DeclarationSequence-b-a
                         |  TypeDeclaration ';'
# start set:   letter
# follow set:  VAR PROCEDURE BEGIN END RETURN

DeclarationSequence-c ::= VAR DeclarationSequence-c-a
                       |  VAR
# start set:   VAR
# follow set:  PROCEDURE BEGIN END RETURN

DeclarationSequence-c-a ::= VariableDeclaration ';' DeclarationSequence-c-a
                         |  VariableDeclaration ';'
# start set:   letter
# follow set:  PROCEDURE BEGIN END RETURN

DeclarationSequence-d ::= ProcedureDeclaration ';' DeclarationSequence-d
                       |  ProcedureDeclaration ';'
# start set:   PROCEDURE
# follow set:  BEGIN END RETURN

//designator = qualident { selector }
		//ex. myvar | mymodule.myvar | mymodule.myvar[i]
		//see selector for more exmples
designator ::= qualident designator-a
            |  qualident
# start set:   letter
# follow set:  '(' ':=' '*' '/' DIV MOD '&' '+' '-' OR '=' '#' '<' '<=' '>' '>='
#              IN IS ')' '..' ',' THEN OF DO TO BY ';' ELSIF END ELSE UNTIL
#              RETURN '}' ']' '|'

designator-a ::= selector designator-a
              |  selector
# start set:   '.' '(' '[' '^'
# follow set:  '(' ':=' '*' '/' DIV MOD '&' '+' '-' OR '=' '#' '<' '<=' '>' '>='
#              IN IS ')' '..' ',' THEN OF DO TO BY ';' ELSIF END ELSE UNTIL
#              RETURN '}' ']' '|'

//digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9".
		//digits 0 to 9
		//remember that we hadn't defined it in Oberon7 grammar
		//hence its First & Follow set has not been generated.

//element = expression [ '..' expression ]
		//ex. see second element of the set {1, 2..5, n+1..2*k}
element ::= expression element-a
         |  expression
# start set:   '+' '-' digit NIL TRUE FALSE '(' '~' '"' '{' letter
# follow set:  ',' '}'

element-a ::= '..' expression
# start set:   '..'
# follow set:  ',' '}'

//ExpList = expression { ',' expression }
		//ex. multiple expressions separated by comma as in 5,n+2,9
ExpList ::= expression ExpList-a
         |  expression
# start set:   '+' '-' digit NIL TRUE FALSE '(' '~' '"' '{' letter
# follow set:  ']' ')'

ExpList-a ::= ',' expression ExpList-a
           |  ',' expression
# start set:   ','
# follow set:  ']' ')'

//expression = SimpleExpression [ relation SimpleExpression ]
		//ex. -6 < k
expression ::= SimpleExpression expression-a
            |  SimpleExpression
# start set:   '+' '-' digit NIL TRUE FALSE '(' '~' '"' '{' letter
# follow set:  ')' '..' ',' THEN OF DO TO BY ';' '}' ']' END ELSIF ELSE UNTIL
#              RETURN '|'

expression-a ::= relation SimpleExpression
# start set:   '=' '#' '<' '<=' '>' '>=' IN IS
# follow set:  ')' '..' ',' THEN OF DO TO BY ';' '}' ']' END ELSIF ELSE UNTIL
#              RETURN '|'

//factor = number | string | NIL | TRUE | FALSE | set | designator [ ActualParameters ] | '(' expression ')' | '~' factor
		//ex. 3 | "z" | "hi" | {1, 2} | myvar | myproc(9,6) | (6*7) | ~mybool
		//note thatin Oberon, we have no character literal,
		//string literal of length 2 is treated as character literal
		//length 2 includes '\0'
factor ::= number
        |  string
        |  NIL
        |  TRUE
        |  FALSE
        |  set
        |  designator factor-a
        |  designator
        |  '(' expression ')'
        |  '~' factor
# start set:   digit NIL TRUE FALSE '(' '~' '"' '{' letter
# follow set:  '*' '/' DIV MOD '&' '+' '-' OR '=' '#' '<' '<=' '>' '>=' IN IS
#              ')' '..' ',' THEN OF DO TO BY ';' '}' ']' END ELSIF ELSE UNTIL
#              RETURN '|'

factor-a ::= ActualParameters
# start set:   '('
# follow set:  '*' '/' DIV MOD '&' '+' '-' OR '=' '#' '<' '<=' '>' '>=' IN IS
#              ')' '..' ',' THEN OF DO TO BY ';' '}' ']' END ELSIF ELSE UNTIL
#              RETURN '|'

//FieldList = IdentList ':' type
		//ex. x,y,z:INTEGER
FieldList ::= IdentList ':' type
# start set:   letter
# follow set:  ';' END

//FieldListSequence = FieldList { ';' FieldList }
		//ex. x,y:INTEGER;z:REAL
FieldListSequence ::= FieldList FieldListSequence-a
                   |  FieldList
# start set:   letter
# follow set:  END

FieldListSequence-a ::= ';' FieldList FieldListSequence-a
                     |  ';' FieldList
# start set:   ';'
# follow set:  END

//FormalParameters = '(' [ FPSection { ';' FPSection } ] ')' [ ':' qualident ]
		//ex. PROCEDURE myproc(VAR x,y:INTEGER; z:CHAR):REAL;
FormalParameters ::= '(' FormalParameters-a ')' FormalParameters-b
                  |  '(' FormalParameters-a ')'
                  |  '(' ')'
                  |  '(' ')' FormalParameters-b
# start set:   '('
# follow set:  ';' ')' END

FormalParameters-a ::= FPSection FormalParameters-a-a
                    |  FPSection
# start set:   letter VAR
# follow set:  ')'

FormalParameters-a-a ::= ';' FPSection FormalParameters-a-a
                      |  ';' FPSection
# start set:   ';'
# follow set:  ')'

FormalParameters-b ::= ':' qualident
# start set:   ':'
# follow set:  ';' ')' END

//FormalType = { ARRAY OF } qualident | ProcedureType
		//ex. PROCEDURE myproc(VAR x,y:INTEGER):REAL;
		//ex. PROCEDURE myproc(x,y: ARRAY OF module.type):REAL;
		//ex. PROCEDURE myproc(VAR x,y: PROCEDURE):REAL;
		//ex. PROCEDURE myproc(x,y: PROCEDURE(a,b:INTEGER):INTEGER):REAL;
FormalType ::= FormalType-a qualident
            |  qualident
            |  ProcedureType
# start set:   ARRAY letter PROCEDURE
# follow set:  ';' ')'

FormalType-a ::= ARRAY OF FormalType-a
              |  ARRAY OF
# start set:   ARRAY
# follow set:  letter

//ForStatement = FOR ident ':=' expression TO expression [ BY ConstExpression ] DO StatementSequence END
		//ex. FOR i:=3 TO 9 BY 2 DO k:=k+1 END
ForStatement ::= FOR ident ':=' expression TO expression ForStatement-a DO
                 StatementSequence END
              |  FOR ident ':=' expression TO expression ForStatement-a DO END
              |  FOR ident ':=' expression TO expression DO END
              |  FOR ident ':=' expression TO expression DO StatementSequence
                 END
# start set:   FOR
# follow set:  ';' ELSIF END ELSE UNTIL RETURN '|'

ForStatement-a ::= BY ConstExpression
# start set:   BY
# follow set:  DO

//FPSection = [ VAR ] ident { ',' ident } ':' FormalType
		//ex. PROCEDURE myproc(VAR x,y:INTEGER):REAL;
FPSection ::= FPSection-a ident FPSection-b ':' FormalType
           |  FPSection-a ident ':' FormalType
           |  ident ':' FormalType
           |  ident FPSection-b ':' FormalType
# start set:   letter VAR
# follow set:  ';' ')'

FPSection-a ::= VAR
# start set:   VAR
# follow set:  letter

FPSection-b ::= ',' ident FPSection-b
             |  ',' ident
# start set:   ','
# follow set:  ':'

//hexDigit = digit | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
		//ex. 3BE4
hexDigit ::= digit
          |  'A'
          |  'B'
          |  'C'
          |  'D'
          |  'E'
          |  'F'
# start set:   digit 'A' 'B' 'C' 'D' 'E' 'F'
# follow set:  digit 'A' 'B' 'C' 'D' 'E' 'F' 'H' 'X'

//identdef = ident [ '*' ]
		//ex. VAR k*:INTEGER;
identdef ::= ident identdef-a
          |  ident
# start set:   letter
# follow set:  '=' ',' '(' ':' ';'

identdef-a ::= '*'
# start set:   '*'
# follow set:  '=' ',' '(' ':' ';'

//ident = letter { letter | digit }
		//ex. class5mark
ident ::= letter ident-a
       |  letter
# start set:   letter
# follow set:  ';' '.' ':=' '*' ',' ':' '=' '(' '[' '^' ')' '/' DIV MOD '&' '+'
#              '-' OR '#' '<' '<=' '>' '>=' IN IS '..' THEN OF DO TO BY ELSIF
#              END ELSE UNTIL RETURN '}' ']' '|'

ident-a ::= letter ident-a
         |  letter
         |  digit ident-a
         |  digit
# start set:   letter digit
# follow set:  ';' '.' ':=' '*' ',' ':' '=' '(' '[' '^' ')' '/' DIV MOD '&' '+'
#              '-' OR '#' '<' '<=' '>' '>=' IN IS '..' THEN OF DO TO BY ELSIF
#              END ELSE UNTIL RETURN '}' ']' '|'

//IdentList = identdef { ',' identdef }
		//ex. VAR x*, y* : INTEGER;
IdentList ::= identdef IdentList-a
           |  identdef
# start set:   letter
# follow set:  ':'

IdentList-a ::= ',' identdef IdentList-a
             |  ',' identdef
# start set:   ','
# follow set:  ':'

//IfStatement = IF expression THEN StatementSequence { ELSIF expression THEN StatementSequence } [ ELSE StatementSequence ] END
		//ex. IF a=0 THEN b:=9 ELSIF a=1 THEN b:=8 ELSIF a=2 THEN b:=7 ELSE b:=6 END
IfStatement ::= IF expression THEN StatementSequence IfStatement-a IfStatement-b
                END
             |  IF expression THEN StatementSequence IfStatement-a END
             |  IF expression THEN StatementSequence END
             |  IF expression THEN END
             |  IF expression THEN IfStatement-a END
             |  IF expression THEN StatementSequence IfStatement-b END
             |  IF expression THEN IfStatement-b END
             |  IF expression THEN IfStatement-a IfStatement-b END
# start set:   IF
# follow set:  ';' ELSIF END ELSE UNTIL RETURN '|'

IfStatement-a ::= ELSIF expression THEN StatementSequence IfStatement-a
               |  ELSIF expression THEN StatementSequence
               |  ELSIF expression THEN
               |  ELSIF expression THEN IfStatement-a
# start set:   ELSIF
# follow set:  ELSE END

IfStatement-b ::= ELSE StatementSequence
               |  ELSE
# start set:   ELSE
# follow set:  END

//import = ident [ ':=' ident ]
		//ex. IMPORT mymodule := mm;
import ::= ident import-a
        |  ident
# start set:   letter
# follow set:  ',' ';'

import-a ::= ':=' ident
# start set:   ':='
# follow set:  ',' ';'

//ImportList = IMPORT import { ',' import } ';'
		//ex. IMPORT mymodule1, mymodule2;
ImportList ::= IMPORT import ImportList-a ';'
            |  IMPORT import ';'
# start set:   IMPORT
# follow set:  CONST TYPE VAR PROCEDURE END BEGIN

ImportList-a ::= ',' import ImportList-a
              |  ',' import
# start set:   ','
# follow set:  ';'

//integer = digit { digit } | digit { hexDigit } 'H'
		//ex. 32 | 0BH | 5BH
		//note that hexadecimal numbers start with a digit, ends with an H.
integer ::= digit integer-a
         |  digit
         |  digit integer-b 'H'
         |  digit 'H'
# start set:   digit
# follow set:  '*' '/' DIV MOD '&' '+' '-' OR '=' '#' '<' '<=' '>' '>=' IN IS
#              ')' '..' ',' THEN OF DO TO BY ';' ':' '}' ']' END ELSIF ELSE
#              UNTIL RETURN '|'

integer-a ::= digit integer-a
           |  digit
# start set:   digit
# follow set:  '*' '/' DIV MOD '&' '+' '-' OR '=' '#' '<' '<=' '>' '>=' IN IS
#              ')' '..' ',' THEN OF DO TO BY ';' ':' '}' ']' END ELSIF ELSE
#              UNTIL RETURN '|'

integer-b ::= hexDigit integer-b
           |  hexDigit
# start set:   digit 'A' 'B' 'C' 'D' 'E' 'F'
# follow set:  'H'

//label = integer | string | qualident
		//ex. 43 | "hi" | myrecord.myelement
		//ex. CASE myvar OF "hi":Out.String("hi")END
label ::= integer
       |  string
       |  qualident
# start set:   digit '"' letter
# follow set:  '..' ',' ':'

//LabelRange = label [ '..' label ]
		//ex. CASE myvar OF 3..5:Out.String("hi")END
LabelRange ::= label LabelRange-a
            |  label
# start set:   digit '"' letter
# follow set:  ',' ':'

LabelRange-a ::= '..' label
# start set:   '..'
# follow set:  ',' ':'

//length = ConstExpression
		//ex. 5+7
length ::= ConstExpression
# start set:   '+' '-' digit NIL TRUE FALSE '(' '~' '"' '{' letter
# follow set:  ',' OF

//letter = "A" | "B" | ... | "Z" | "a" | "b" | ... | "z".
		//ex. capital and small letters
		//remember that we hadn't defined it in Oberon7 grammar
		//hence its First & Follow set has not been generated.

//module = MODULE ident ';' [ ImportList ] DeclarationSequence [ BEGIN StatementSequence ] END ident '.'
		//ex. MODULE mymod; IMPORT mymodule; VAR x:INTEGER; BEGIN x:=9 END mymod .
		//note that there is a full stop mark at the END of module identifier.
module ::= MODULE ident ';' module-a DeclarationSequence module-b END ident '.'
        |  MODULE ident ';' module-a DeclarationSequence END ident '.'
        |  MODULE ident ';' module-a END ident '.'
        |  MODULE ident ';' END ident '.'
        |  MODULE ident ';' DeclarationSequence END ident '.'
        |  MODULE ident ';' module-a module-b END ident '.'
        |  MODULE ident ';' module-b END ident '.'
        |  MODULE ident ';' DeclarationSequence module-b END ident '.'
# start set:   MODULE

module-a ::= ImportList
# start set:   IMPORT
# follow set:  CONST TYPE VAR PROCEDURE END BEGIN

module-b ::= BEGIN StatementSequence
          |  BEGIN
# start set:   BEGIN
# follow set:  END

//MulOperator = '*' | '/' | DIV | MOD | '&'
		//ex. * | / | DIV | MOD | &
MulOperator ::= '*'
             |  '/'
             |  DIV
             |  MOD
             |  '&'
# start set:   '*' '/' DIV MOD '&'
# follow set:  digit NIL TRUE FALSE '(' '~' '"' '{' letter

//number = integer | real
		//ex. 34 | 3.4
number ::= integer
        |  real
# start set:   digit
# follow set:  '*' '/' DIV MOD '&' '+' '-' OR '=' '#' '<' '<=' '>' '>=' IN IS
#              ')' '..' ',' THEN OF DO TO BY ';' '}' ']' END ELSIF ELSE UNTIL
#              RETURN '|'

//PointerType = POINTER TO type
		//ex. TYPE x: POINTER TO myrecord;
PointerType ::= POINTER TO type
# start set:   POINTER
# follow set:  ';' END

//ProcedureBody = DeclarationSequence [ BEGIN StatementSequence ] [ RETURN expression ] END
		//ex. PROCEDURE myproc(x:INTEGER):REAL; VAR y:INTEGER; BEGIN y:=1; RETURN x+y END myproc;
ProcedureBody ::= DeclarationSequence ProcedureBody-a ProcedureBody-b END
               |  DeclarationSequence ProcedureBody-a END
               |  DeclarationSequence END
               |  END
               |  ProcedureBody-a END
               |  DeclarationSequence ProcedureBody-b END
               |  ProcedureBody-b END
               |  ProcedureBody-a ProcedureBody-b END
# start set:   END CONST TYPE VAR BEGIN RETURN PROCEDURE
# follow set:  letter

ProcedureBody-a ::= BEGIN StatementSequence
                 |  BEGIN
# start set:   BEGIN
# follow set:  RETURN END

ProcedureBody-b ::= RETURN expression
# start set:   RETURN
# follow set:  END

//ProcedureCall = designator [ ActualParameters ]
		//ex. myproc | myfunc(4,6)
ProcedureCall ::= designator ProcedureCall-a
               |  designator
# start set:   letter
# follow set:  ';' ELSIF END ELSE UNTIL RETURN '|'

ProcedureCall-a ::= ActualParameters
# start set:   '('
# follow set:  ';' ELSIF END ELSE UNTIL RETURN '|'

//ProcedureDeclaration = ProcedureHeading ';' ProcedureBody ident
		//ex. PROCEDURE myproc(x:INTEGER):REAL; VAR y:INTEGER; BEGIN y:=1; RETURN x+y END myproc;
ProcedureDeclaration ::= ProcedureHeading ';' ProcedureBody ident
# start set:   PROCEDURE
# follow set:  ';'

//ProcedureHeading = PROCEDURE identdef [ FormalParameters ]
		//ex. PROCEDURE myproc*(x:INTEGER):REAL; VAR y:INTEGER; BEGIN y:=1; RETURN x+y END myproc;
ProcedureHeading ::= PROCEDURE identdef ProcedureHeading-a
                  |  PROCEDURE identdef
# start set:   PROCEDURE
# follow set:  ';'

ProcedureHeading-a ::= FormalParameters
# start set:   '('
# follow set:  ';'

//ProcedureType = PROCEDURE [ FormalParameters ]
		//ex. TYPE mp: PROCEDURE(a,b:INTEGER):INTEGER;
ProcedureType ::= PROCEDURE ProcedureType-a
               |  PROCEDURE
# start set:   PROCEDURE
# follow set:  ';' ')' END

ProcedureType-a ::= FormalParameters
# start set:   '('
# follow set:  ';' ')' END

//qualident = [ ident '.' ] ident
		//ex. mymodule.myvar | myvar
qualident ::= qualident-a ident
           |  ident
# start set:   letter
# follow set:  '.' '(' '[' '^' ')' ':=' '*' '/' DIV MOD '&' '+' '-' OR '=' '#'
#              '<' '<=' '>' '>=' IN IS '..' ',' THEN OF DO TO BY ';' ':' ELSIF
#              END ELSE UNTIL RETURN '}' ']' '|'

qualident-a ::= ident '.'
# start set:   letter
# follow set:  letter

//real = digit { digit } '.' { digit } [ ScaleFactor ]
		//ex. 34.4 | 34.4E-21
real ::= digit real-a '.' real-b real-c
      |  digit real-a '.' real-b
      |  digit real-a '.'
      |  digit '.'
      |  digit '.' real-b
      |  digit real-a '.' real-c
      |  digit '.' real-c
      |  digit '.' real-b real-c
# start set:   digit
# follow set:  '*' '/' DIV MOD '&' '+' '-' OR '=' '#' '<' '<=' '>' '>=' IN IS
#              ')' '..' ',' THEN OF DO TO BY ';' '}' ']' END ELSIF ELSE UNTIL
#              RETURN '|'

real-a ::= digit real-a
        |  digit
# start set:   digit
# follow set:  '.'

real-b ::= digit real-b
        |  digit
# start set:   digit
# follow set:  'E' '*' '/' DIV MOD '&' '+' '-' OR '=' '#' '<' '<=' '>' '>=' IN
#              IS ')' '..' ',' THEN OF DO TO BY ';' '}' ']' END ELSIF ELSE UNTIL
#              RETURN '|'

real-c ::= ScaleFactor
# start set:   'E'
# follow set:  '*' '/' DIV MOD '&' '+' '-' OR '=' '#' '<' '<=' '>' '>=' IN IS
#              ')' '..' ',' THEN OF DO TO BY ';' '}' ']' END ELSIF ELSE UNTIL
#              RETURN '|'

//RecordType = RECORD [ '(' BaseType ')' ] [ FieldListSequence ] END
		//ex. TYPE mytype = RECORD (basetype) x,y:INTEGER; z:CHAR END;
RecordType ::= RECORD RecordType-a RecordType-b END
            |  RECORD RecordType-a END
            |  RECORD END
            |  RECORD RecordType-b END
# start set:   RECORD
# follow set:  ';' END

RecordType-a ::= '(' BaseType ')'
# start set:   '('
# follow set:  letter END

RecordType-b ::= FieldListSequence
# start set:   letter
# follow set:  END

//relation = '=' | '#' | '<' | '<=' | '>' | '>=' | IN | IS
		//ex. = | # | < | <= | > | >= | IN | IS
relation ::= '='
          |  '#'
          |  '<'
          |  '<='
          |  '>'
          |  '>='
          |  IN
          |  IS
# start set:   '=' '#' '<' '<=' '>' '>=' IN IS
# follow set:  '+' '-' digit NIL TRUE FALSE '(' '~' '"' '{' letter

//RepeatStatement = REPEAT StatementSequence UNTIL expression
		//ex. REPEAT a:=a+1 UNTIL a>10
RepeatStatement ::= REPEAT StatementSequence UNTIL expression
                 |  REPEAT UNTIL expression
# start set:   REPEAT
# follow set:  ';' ELSIF END ELSE UNTIL RETURN '|'

//ScaleFactor = 'E' [ '+' | '-' ] digit { digit }
		//ex. 34.4E-21
ScaleFactor ::= 'E' ScaleFactor-a digit ScaleFactor-b
             |  'E' ScaleFactor-a digit
             |  'E' digit
             |  'E' digit ScaleFactor-b
# start set:   'E'
# follow set:  '*' '/' DIV MOD '&' '+' '-' OR '=' '#' '<' '<=' '>' '>=' IN IS
#              ')' '..' ',' THEN OF DO TO BY ';' '}' ']' END ELSIF ELSE UNTIL
#              RETURN '|'

ScaleFactor-a ::= '+'
               |  '-'
# start set:   '+' '-'
# follow set:  digit

ScaleFactor-b ::= digit ScaleFactor-b
               |  digit
# start set:   digit
# follow set:  '*' '/' DIV MOD '&' '+' '-' OR '=' '#' '<' '<=' '>' '>=' IN IS
#              ')' '..' ',' THEN OF DO TO BY ';' '}' ']' END ELSIF ELSE UNTIL
#              RETURN '|'

//selector = '.' ident | '[' ExpList ']' | '^' | '(' qualident ')'
		//ex. record.element | array[rowindex, columnindex] | recordpointer^.element (= recordpointer.element) | p(Circle).radius (here p is of type Figure)
selector ::= '.' ident
          |  '[' ExpList ']'
          |  '^'
          |  '(' qualident ')'
# start set:   '.' '(' '[' '^'
# follow set:  '.' '(' '[' '^' ':=' '*' '/' DIV MOD '&' '+' '-' OR '=' '#' '<'
#              '<=' '>' '>=' IN IS ')' '..' ',' THEN OF DO TO BY ';' ELSIF END
#              ELSE UNTIL RETURN '}' ']' '|'

//set = '{' [ element { ',' element } ] '}'
		//ex. a set is {1, 2..5, n+1..2*k}
set ::= '{' set-a '}'
     |  '{' '}'
# start set:   '{'
# follow set:  '*' '/' DIV MOD '&' '+' '-' OR '=' '#' '<' '<=' '>' '>=' IN IS
#              ')' '..' ',' THEN OF DO TO BY ';' '}' ']' END ELSIF ELSE UNTIL
#              RETURN '|'

set-a ::= element set-a-a
       |  element
# start set:   '+' '-' digit NIL TRUE FALSE '(' '~' '"' '{' letter
# follow set:  '}'

set-a-a ::= ',' element set-a-a
         |  ',' element
# start set:   ','
# follow set:  '}'

//SimpleExpression = [ '+' | '-' ] term { AddOperator term }
		//ex. -3 | -3 + 4 | -3 - 4
SimpleExpression ::= SimpleExpression-a term SimpleExpression-b
                  |  SimpleExpression-a term
                  |  term
                  |  term SimpleExpression-b
# start set:   '+' '-' digit NIL TRUE FALSE '(' '~' '"' '{' letter
# follow set:  '=' '#' '<' '<=' '>' '>=' IN IS ')' '..' ',' THEN OF DO TO BY ';'
#              '}' ']' END ELSIF ELSE UNTIL RETURN '|'

SimpleExpression-a ::= '+'
                    |  '-'
# start set:   '+' '-'
# follow set:  digit NIL TRUE FALSE '(' '~' '"' '{' letter

SimpleExpression-b ::= AddOperator term SimpleExpression-b
                    |  AddOperator term
# start set:   '+' '-' OR
# follow set:  '=' '#' '<' '<=' '>' '>=' IN IS ')' '..' ',' THEN OF DO TO BY ';'
#              '}' ']' END ELSIF ELSE UNTIL RETURN '|'

//statement = [ assignment | ProcedureCall | IfStatement | CaseStatement | WhileStatement | RepeatStatement | ForStatement ]
		//ex. a:=1 | myproc | myproc(3) | IF a=0 THEN b:=9 END | CASE myvar OF 1: Out.String("hi") END | WHILE a>9 DO b:=1 END | REPEAT a:=a+1 UNTIL a>10 | FOR i:=3 TO 9 BY 2 DO k:=k+1 END
statement ::= statement-a
# start set:   IF CASE WHILE REPEAT FOR letter
# follow set:  ';' ELSIF END ELSE UNTIL RETURN '|'

statement-a ::= assignment
             |  ProcedureCall
             |  IfStatement
             |  CaseStatement
             |  WhileStatement
             |  RepeatStatement
             |  ForStatement
# start set:   IF CASE WHILE REPEAT FOR letter
# follow set:  ';' ELSIF END ELSE UNTIL RETURN '|'

//StatementSequence = statement { ';' statement }
		//ex. a:=2;b:=6
StatementSequence ::= statement StatementSequence-a
                   |  statement
                   |  StatementSequence-a
# start set:   ';' IF CASE WHILE REPEAT FOR letter
# follow set:  ELSIF END ELSE UNTIL RETURN '|'

StatementSequence-a ::= ';' statement StatementSequence-a
                     |  ';' statement
                     |  ';'
                     |  ';' StatementSequence-a
# start set:   ';'
# follow set:  ELSIF END ELSE UNTIL RETURN '|'

//string = '"' { character } '"' | digit { hexDigit } 'X'
		//ex. "abcd" | 3BX
		//note that hex string starts with a digit, ends with an X.
string ::= '"' string-a '"'
        |  '"' '"'
        |  digit string-b 'X'
        |  digit 'X'
# start set:   digit '"'
# follow set:  '*' '/' DIV MOD '&' '+' '-' OR '=' '#' '<' '<=' '>' '>=' IN IS
#              ')' '..' ',' THEN OF DO TO BY ';' ':' '}' ']' END ELSIF ELSE
#              UNTIL RETURN '|'

string-a ::= character string-a
          |  character
# start set:   letter digit
# follow set:  '"'

string-b ::= hexDigit string-b
          |  hexDigit
# start set:   digit 'A' 'B' 'C' 'D' 'E' 'F'
# follow set:  'X'

//term = factor { MulOperator factor }
		//ex. 3*4 | 5/8
term ::= factor term-a
      |  factor
# start set:   digit NIL TRUE FALSE '(' '~' '"' '{' letter
# follow set:  '+' '-' OR '=' '#' '<' '<=' '>' '>=' IN IS ')' '..' ',' THEN OF
#              DO TO BY ';' '}' ']' END ELSIF ELSE UNTIL RETURN '|'

term-a ::= MulOperator factor term-a
        |  MulOperator factor
# start set:   '*' '/' DIV MOD '&'
# follow set:  '+' '-' OR '=' '#' '<' '<=' '>' '>=' IN IS ')' '..' ',' THEN OF
#              DO TO BY ';' '}' ']' END ELSIF ELSE UNTIL RETURN '|'

//TypeDeclaration = identdef '=' type
		//ex. TYPE T* = INTEGER;
TypeDeclaration ::= identdef '=' type
# start set:   letter
# follow set:  ';'

//type = qualident | ArrayType | RecordType | PointerType | ProcedureType
		//ex. TYPE k: mymod.mytype; | TYPE k: ARRAY 3+1,4 OF INTEGER; | TYPE mytype = RECORD (basetype) x,y:INTEGER; z:CHAR END; | TYPE x: POINTER TO myrecord; | TYPE mp: PROCEDURE(a,b:INTEGER):INTEGER;
type ::= qualident
      |  ArrayType
      |  RecordType
      |  PointerType
      |  ProcedureType
# start set:   letter ARRAY RECORD POINTER PROCEDURE
# follow set:  ';' END

//VariableDeclaration = IdentList ':' type
		//ex. VAR x,y: INTEGER;
VariableDeclaration ::= IdentList ':' type
# start set:   letter
# follow set:  ';'

//WhileStatement = WHILE expression DO StatementSequence { ELSIF expression DO StatementSequence } END
		//ex. WHILE a>9 DO b:=1 ELSIF a>5 DO b:=2 ELSIF a>3 DO b:=3 END
WhileStatement ::= WHILE expression DO StatementSequence WhileStatement-a END
                |  WHILE expression DO StatementSequence END
                |  WHILE expression DO END
                |  WHILE expression DO WhileStatement-a END
# start set:   WHILE
# follow set:  ';' ELSIF END ELSE UNTIL RETURN '|'

WhileStatement-a ::= ELSIF expression DO StatementSequence WhileStatement-a
                  |  ELSIF expression DO StatementSequence
                  |  ELSIF expression DO
                  |  ELSIF expression DO WhileStatement-a
# start set:   ELSIF
# follow set:  END

#endif

//instruction formats
//-------------------
//
//void Put0(int op, int a, int b, int c)
//
//Format0:   00u0  --a-  --b-  -op-                    --c-
//bit num: 32----28----24----20----16----12----08----04----
//
//op   mnemonic   normal meaning    special meaning if u=1
//--   --------   --------------    ----------------------
// 0   MOV a,0,c  Ra := Rc          when c=0, Ra := H; when c=1, Ra := 0000NZCV
// 1   LSL a,b,c  Ra := Rb << Rc
// 2   ASR a,b,c  Ra := Rb >> Rc
// 3   ROR a,b,c  Ra := Rb ror Rc
//
// 4   AND a,b,c  Ra := Rb & Rc
// 5   ANN a,b,c  Ra := Rb & ~Rc
// 6   IOR a,b,c  Ra := Rb or Rc
// 7   XOR a,b,c  Ra := Rb xor Rc
//
// 8   ADD a,b,c  Ra := Rb + Rc     ADD considers C bit
// 9   SUB a,b,c  Ra := Rb – Rc     SUB considers C bit
//10   MUL a,b,c  Ra := Ra * Rc     MUL does unsigned multiplication
//11   DIV a,b,c  Ra := Rb div Rc
//
//12   FAD a,b,c  Ra := Rb + Rc
//13   FSB a,b,c  Ra := Rb – Rc
//14   FML a,b,c  Ra := Ra * Rc
//15   FDV a,b,c  Ra := Rb / Rc
//
//MUL deposits high 32 bits of product in auxiliary register H
//DIV deposits remainder in auxiliary register H
//--------------
//
//void Put1(int op, int a, int b, int im)
//void Put1a(int op, int a, int b, int im)
//
//Format1:   00uv  --a-  --b-  -op-  <---------im--------->
//bit num: 32----28----24----20----16----12----08----04----
//
//immediate value (im) is 16bit inside instructions
//it is expanded when transfered to 32bit registers
//if v=0, while expanding im, MSBs of im is filled with 0
//if v=1, while expanding im, MSBs of im is filled with 1
//
//op   mnemonic    normal meaning    special meaning if u=1
//--   --------    --------------    ----------------------
// 0   MOV a,0,im  Ra := im          Ra := im << 16
// 1   LSL a,b,im  Ra := Rb << im
// 2   ASR a,b,im  Ra := Rb >> im
// 3   ROR a,b,im  Ra := Rb ror im
//
// 4   AND a,b,im  Ra := Rb & im
// 5   ANN a,b,im  Ra := Rb & ~im
// 6   IOR a,b,im  Ra := Rb or im
// 7   XOR a,b,im  Ra := Rb xor im
//
// 8   ADD a,b,im  Ra := Rb + im     ADD considers C bit
// 9   SUB a,b,im  Ra := Rb – im     SUB considers C bit
//10   MUL a,b,im  Ra := Ra * im     MUL does unsigned multiplication
//11   DIV a,b,im  Ra := Rb div im
//
//12   FAD a,b,im  Ra := Rb + im
//13   FSB a,b,im  Ra := Rb – im
//14   FML a,b,im  Ra := Ra * im
//15   FDV a,b,im  Ra := Rb / im
//
//MUL deposits high 32 bits of product in auxiliary register H
//DIV deposits remainder in auxiliary register H
//--------------
//
//void Put2(int op, int a, int b, int off)
//op: Ldr = 8, Str = 10
//
//Format2:   10uv  --a-  --b-  <------------off----------->
//bit num: 32----28----24----20----16----12----08----04----
//
//load:  Ra := Mem[Rb + off]
//store: Mem[Rb + off] := Ra
//            uv
//if op = 0b1000 = 8, it means load word
//if op = 0b1001 = 9, it means load byte
//if op = 0b1010 = 10, it means store word
//if op = 0b1011 = 11, it means store byte
//--------------
//
//void Put3(int op, int cond, int off)
//op: BR = 0, BLR = 1, BC = 2, BL = 3
//cond: MI = 0, PL = 8,
//      EQ = 1, NE = 9,
//      LT = 5, GE = 13,
//      LE = 6, GT = 14;
//
//             u
//Format3:   110v  cond                          0000  --c-
//bit num: 32----28----24----20----16----12----08----04----
//
//           111v  cond  <---------------off-------------->
//bit num: 32----28----24----20----16----12----08----04----
//
//u=0 means branch to addr in Rc
//u=1 means branch to PC + 1 + off
//v=0 means link addr is NOT stored in R15
//v=1 means link addr PC + 1 is stored in R15
//
//BR  = 0 = (u=0,v=0) = branch to addr in Rc, link addr is NOT stored in R15
//BLR = 1 = (u=0,v=1) = branch to addr in Rc, link addr PC + 1 is stored in R15
//BC  = 2 = (u=1,v=0) = branch to PC + 1 + off, link addr is NOT stored in R15
//BL  = 3 = (u=1,v=1) = branch to PC + 1 + off, link addr PC + 1 is stored in R15
//
//cond  mnemonic  meaning           evaluation
//----  --------  -------           ----------
//0000  MI        negative(minus)   N
//0001  EQ        equal(zero)       Z
//0010  CS        carry set         C
//0011  VS        overflow set      V
//0100  LS        less or same      ~C|Z
//0101  LT        less than         N#V
//0110  LE        less or equal     (N#V)|Z
//0111  --        always            true
//
//1000  PL        positive(plus)    ~N
//1001  NE        not equal         ~Z
//1010  CC        carry clear       ~C
//1011  VC        overflow clear    ~V
//1100  HI        high              ~(~C|Z)
//1101  GE        greater or equal  ~(N#V)
//1110  GT        greater than      ~((N#V)|Z)
//1111  --        never             flase
//--------------

#if 0

*topScope--->Object | *next--->Object | *next
             --------------    --------------
             class- Head       class- Module
             name-             name- 
             *type-            *type---.
             *dsc---.          *dsc-    \   
                    |                    `------>Type | *base--->Type | *base
                    |                            ------------    ------------
                    |                            form-           form-       
                    |                            size-           size-       
                    |                            len-            len-        
                    V                            *dsc-           *dsc-       
             Object | *next--->Object | *next
             --------------    --------------
             class- Head       class- Module
             name-             name- 
             *dsc---.          *dsc-    \   
                    |                    `------>Type | *base--->Type | *base
                    |                            ------------    ------------
                    |                            form-           form-       
                    |                            size-           size-       
                    |                            len-            len-        
                    V                            *dsc-           *dsc-       
             Object | *next--->Object | *next
             --------------    --------------
             class- Head       class- Module
             name-             name- 

#endif