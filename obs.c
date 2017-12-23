//Oberon7 scanner
//Based on Niklaus Wirth's implementation
//Compile using gcc
//$ gcc -Wall obs.c obt.c obg.c obp.c obc.c

//This code is distributed under the GPL License.
//For more info, check: http://www.gnu.org/copyleft/gpl.html

//04 June 2016: Srinivas Nayak: This file created
//04 June 2016: Srinivas Nayak: Coding started
//10 July 2016: Srinivas Nayak: Code updated with NW's version of 04 July 2016

//Oberon scanner does lexical analysis.
//Input is Oberon text, output is sequence of symbols,
//i.e identifiers, numbers, strings, and special symbols.
//Recognises all Oberon keywords and skips comments.
//Get(sym) delivers next symbol from input text with Reader R.
//Mark(msg) records error and delivers error message with Writer W.

//repeate...until: The process repeats until the given condition becomes true.
//do...while: The process repeates until the given condition becomes false.

#include "obc.h"

#define STR_SIZE 256
#define MAX_EXP 38
#define NUM_KEYWORDS 34

char id[ID_LEN];
int ival;
float rval;
char str[STR_SIZE];
int slen;

int k;
//keytab[] contains keyword number and keyword name.
struct
{
    int sym;
    char id[12];
} keytab[NUM_KEYWORDS];
int kwx[10];

//last character read by scanner is available here
char ch;

int errpos;
int errcnt;
FILE* f; //source file (.Mod)

//lexical symbols or tokens
int MUL = 1, RDIV = 2, DIV = 3, MOD = 4, AND = 5, //MulOperator = '*' | '/' | DIV | MOD | '&'
    PLUS = 6, MINUS = 7, OR = 8, //AddOperator = '+' | '-' | OR
    EQL = 9, NEQ = 10, LSR = 11, LEQ = 12, GTR = 13, GEQ = 14, IN = 15, IS = 16, //relation = '=' | '#' | '<' | '<=' | '>' | '>=' | IN | IS
    ARROW = 17, PERIOD = 18,
    CHAR = 20, INT = 21, REAL = 22, FALS = 23, TRU = 24, NIL = 25, STRING = 26, NOT = 27, LPAREN = 28, LBRAK = 29, LBRACE = 30, IDENT = 31, //factor = number | string | NIL | TRUE | FALSE | set | designator [ActualParameters] | '(' expression ')' | '~' factor
    IF = 32, WHILE = 34, REPEAT = 35, CASE = 36, FOR = 37,
    COMMA = 40, COLON = 41, BECOMES = 42, UPTO = 43, RPAREN = 44, RBRAK = 45, RBRACE = 46,
    THEN = 47, OF = 48, DO = 49, TO = 50, BY = 51, SEMICOLON = 52, END = 53, BAR = 54, ELSE = 55, ELSIF = 56, UNTIL = 57, RETURN = 58,
    ARRAY = 60, RECORD = 61, POINTER = 62,
    CONST = 63, TYPE = 64, VAR = 65, PROCEDURE = 66, BEGIN = 67, IMPORT = 68, MODULE = 69, EOT = 70; //eof = EOT

//fills keytab[] with language keywords
void enter_kw(int sym, char* name)
{
    strcpy(keytab[k].id, name);
    keytab[k].sym = sym;
    k++;
}

//returns current position of scanner in the source file
int pos()
{
    return ftell(f) - 1;
}

//prints compilation error message to console
void Mark(char * msg)
{
    int p;
    p = pos();
    //compiler prints maximum 25 errors at a run
    if( (p > errpos) && (errcnt < 25) )
        printf("pos %d: %s\n", p, msg);
    errcnt++;
    errpos = p + 4; //why +4? why not +5? It is just a heuristics; it can be +10 also!
}

//consumes a comment for scanner from source code
void comment()
{
    ch = fgetc(f);
    do
    {
        while( !feof(f) && (ch != '*'))
        {
            if( ch == '(' )
            {
                ch = fgetc(f);
                if (ch == '*' ) comment();
            }
            else ch = fgetc(f);
        }

        while (ch == '*')
        {
            ch = fgetc(f);
        }
    }
    while(! ((ch == ')') || feof(f)));

    if (!feof(f))
    {
        ch = fgetc(f);
    }
    else
    {
        Mark("unterminated comment");
    }
}

//This function can find power 'e' of any 'number'.
//input is 'e'. 'number' hard coded to t.
//how it works? say e=13 and number is 7.
//   e =      1     1     0     1    (binary of 13)
//   e =     2^3 + 2^2 + 2^1 + 2^0   (wherever bit is 1)
//   e =      8  +  4  +  2  +  1    (wherever bit is 1)
// 7^e = 7^(  8  +  4  +  2  +  1  ) (wherever bit is 1)
// 7^e =     7^8 * 7^4 * 7^2 * 7^1   (wherever bit is 1)
// itr :      4     3     2     1    (itr is iteration number; here 4 iterations needed to visit all bits)
//   t :     t4    t3    t2    t1    (t1, t2... are value of t at begining of iteration 1, 2...)
//   r =     t4  * t3     *    t1    (r selectively multiplies values of t, wherever bit is 1)
float ten(int e)
{
    //t for temporary
    //r for result
    float r, t;
    r = 1.0;
    t = 10.0;
    while( e > 0) //till we visit all bits
    {
        if ((e & 1) == 1) //if LSBit == 1
        {
            r = t * r;
        }
        t = t * t;
        e = e >> 1; //right shift 1 bit
    }
    return r;
}

//consumes an integer literal or real literal
void number(int *sym)
{
    int max = 2147483647; //2^31 - 1
    int i, k, e, n, s, h;
    float x;
    int d[16];
    int neg_exp;

    ival = 0;
    i = 0;
    n = 0;
    k = 0;

    do
    {
        if (n < 16)
        {
            d[n] = ch - 0x30;
            n++;
        }
        else
        {
            Mark("too many digits");
            n = 0;
        }
        ch = fgetc(f);
    }
    while(!( (ch < '0') || ((ch > '9') && (ch < 'A')) || (ch > 'F')));

    if( (ch == 'H') || (ch == 'R') || (ch == 'X') ) //hex
    {
        do
        {
            h = d[i];
            if( h >= 10)
            {
                h = h-7;
            }
            k = k*0x10 + h;
            i++; //no overflow check
        }
        while( !(i == n));
        if (ch == 'X')
        {
            *sym = CHAR;
            if( k < 0x100)
            {
                ival = k;
            }
            else
            {
                Mark("illegal value");
                ival = 0;
            }
        }
        else if( ch == 'R')
        {
            *sym = REAL;

            rval = (float)k;
        }
        else
        {
            *sym = INT;
            ival = k;
        }
        ch = fgetc(f);
    }
    else if( ch == '.')
    {
        ch = fgetc(f);
        //if double dot
        //may be we saw 3..67 or something like this
        //then we got a decimal number and then "upto" symbol ".."
        //so now convert the second '.' to 0x7F.
        //Get() function will treat it as "upto".
        if( ch == '.' )
        {
            ch = 0x7F;
            do
            {
                if( d[i] < 10)
                {
                    if (k <= ((max-d[i]) / 10) )
                    {
                        k = k *10 + d[i];
                    }
                    else
                    {
                        Mark("too large");
                        k = 0;
                    }
                }
                else
                {
                    Mark("bad integer");
                }
                i++;
            }
            while( !(i == n));
            *sym = INT;
            ival = k;
        }
        else
        {
            x = 0.0;
            e = 0; //real number
            //integer part
            do
            {
                x = x * 10.0 + (float)(d[i]);
                i++;
            }
            while(!( i == n));

            while( (ch >= '0') && (ch <= '9') )   //fraction
            {
                x = x * 10.0 + (float)(ch - 0x30);
                e--;
                ch = fgetc(f);
            }

            if( (ch == 'E') || (ch == 'D') )   //scale factor
            {
                ch = fgetc(f);
                s = 0;
                if( ch == '-' )
                {
                    neg_exp = TRUE;
                    ch = fgetc(f);
                }
                else
                {
                    neg_exp = FALSE;
                    if( ch == '+' )
                    {
                        ch = fgetc(f);
                    }
                }
                if( (ch >= '0') && (ch <= '9') )
                {
                    do
                    {
                        s = s*10 + (ch-0x30);
                        ch = fgetc(f);
                    }
                    while(!((ch < '0') || (ch >'9')));

                    if( neg_exp )
                    {
                        e = e-s;
                    }
                    else
                    {
                        e = e+s;
                    }
                }
                else
                {
                    Mark("digit?");
                }
            }

            if( e < 0 )
            {
                if( e >= -MAX_EXP )
                {
                    x = x / ten(-e);
                }
                else
                {
                    x = 0.0;
                }
            }
            else if( e > 0 )
            {
                if( e <= MAX_EXP )
                {
                    x = ten(e) * x;
                }
                else
                {
                    x = 0.0;
                    Mark("too large");
                }
            }
            *sym = REAL;
            rval = x;
        }
    }
    else //decimal integer
    {
        do
        {
            if( d[i] < 10 )
            {
                if( k <= ((max-d[i]) / 10) )
                {
                    k = k*10 + d[i];
                }
                else
                {
                    Mark("too large");
                    k = 0;
                }
            }
            else
            {
                Mark("bad integer");
            }
            i++;
        }
        while(i != n); //if true loop, else break
        *sym = INT;
        ival = k;
    }
}

//consumes a hexstring
void hexstring()
{
    int i, m, n;
    i = 0;
    ch = fgetc(f);
    while( !feof(f) && (ch != '$') )
    {
        while( (ch == ' ') || (ch == 0x9) || (ch == 0x0A) || (ch == 0x0D) )
        {
            ch = fgetc(f); //skip
        }

        if( ('0' <= ch) && (ch <= '9') )
        {
            m = ch - 0x30;
        }
        else if( ('A' <= ch) && (ch <= 'F') )
        {
            m = ch - 0x37;
        }
        else
        {
            m = 0;
            Mark("hexdig expected");
        }

        ch = fgetc(f);

        if( ('0' <= ch) && (ch <= '9') )
        {
            n = ch - 0x30;
        }
        else if( ('A' <= ch) && (ch <= 'F') )
        {
            n = ch - 0x37;
        }
        else
        {
            n = 0;
            Mark("hexdig expected");
        }

        if( i < STR_SIZE )
        {
            str[i] = (char)(m*0x10 + n); //in a byte, put 0xMN, so M << 4 = M*16
            i++;
        }
        else
        {
            Mark("string too long");
        }

        ch = fgetc(f);
    }
    ch = fgetc(f);
    slen = i;  //no 0X appended!
}

//consumes an identifier
void identifier(int *sym)
{
    int i, k;
    i = 0;
    do
    {
        if( i < ID_LEN-1 )
        {
            id[i] = ch;
            i++;
        }
        ch = fgetc(f);
    }
    while( !((ch < '0') || ((ch > '9') && (ch < 'A')) || ((ch > 'Z') && (ch < 'a')) || (ch > 'z')) );
    id[i] = '\0';
    if (i < 10)
    {
        k = kwx[i-1];  //search for keyword
        while( strcmp(id, keytab[k].id) != 0 && (k < kwx[i]) )
        {
            k++;
        }
        if (k < kwx[i] )
        {
            *sym = keytab[k].sym;
        }
        else *sym = IDENT;
    }
    else *sym = IDENT;
}

//consumes a string literal
void string()
{
    int i;
    i = 0;
    ch = fgetc(f);
    while( !feof(f) && (ch != 0x22))
    {
        if (ch >= ' ') //ignore control characters, control characters are below Space
        {
            if (i < STR_SIZE-1)
            {
                str[i] = ch;
                i++;
            }
            else
            {
                Mark("string too long");
            }
        }
        ch = fgetc(f);
    }
    str[i] = '\0';
    i++;
    ch = fgetc(f);
    slen = i; //slen includes '\0' character, so 1 bigger than actual string length
}

//ASCII Table
//     |  0 NUL|  1 SOH|  2 STX|  3 ETX|  4 EOT|  5 ENQ|  6 ACK|  7 BEL|
//     |  8 BS |  9 HT | 10 NL | 11 VT | 12 NP | 13 CR | 14 SO | 15 SI |
//     | 16 DLE| 17 DC1| 18 DC2| 19 DC3| 20 DC4| 21 NAK| 22 SYN| 23 ETB|
//     | 24 CAN| 25 EM | 26 SUB| 27 ESC| 28 FS | 29 GS | 30 RS | 31 US |
//     | 32 SP | 33  ! | 34  " | 35  # | 36  $ | 37  % | 38  & | 39  ' |
//     | 40  ( | 41  ) | 42  * | 43  + | 44  , | 45  - | 46  . | 47  / |
//     | 48  0 | 49  1 | 50  2 | 51  3 | 52  4 | 53  5 | 54  6 | 55  7 |
//     | 56  8 | 57  9 | 58  : | 59  ; | 60  < | 61  = | 62  > | 63  ? |
//     | 64  @ | 65  A | 66  B | 67  C | 68  D | 69  E | 70  F | 71  G |
//     | 72  H | 73  I | 74  J | 75  K | 76  L | 77  M | 78  N | 79  O |
//     | 80  P | 81  Q | 82  R | 83  S | 84  T | 85  U | 86  V | 87  W |
//     | 88  X | 89  Y | 90  Z | 91  [ | 92  \ | 93  ] | 94  ^ | 95  _ |
//     | 96  ` | 97  a | 98  b | 99  c |100  d |101  e |102  f |103  g |
//     |104  h |105  i |106  j |107  k |108  l |109  m |110  n |111  o |
//     |112  p |113  q |114  r |115  s |116  t |117  u |118  v |119  w |
//     |120  x |121  y |122  z |123  { |124  | |125  } |126  ~ |127 DEL|

//this is the main function of the scanner
//it returns token number for a found token
void Get(int *sym)
{
    do
    {
        while( !feof(f) && (ch <= ' '))
        {
            ch = fgetc(f); //ignore control characters which are <= Space(or 32)
        }

        if(feof(f))
        {
            *sym = EOT;
        }
        else if(ch < 'A' )
        {
            if(ch < '0' )
            {
                if(ch == 0x22 )
                {
                    string();
                    *sym = STRING;
                }
                else if(ch == '#' )
                {
                    ch = fgetc(f);
                    *sym = NEQ;
                }
                else if(ch == '$' )
                {
                    hexstring();
                    *sym = STRING;
                }
                else if(ch == '&' )
                {
                    ch = fgetc(f);
                    *sym = AND;
                }
                else if(ch == '(' )
                {
                    ch = fgetc(f);
                    if(ch == '*' )
                    {
                        *sym = 0;
                        comment();
                    }
                    else *sym = LPAREN;
                }
                else if(ch == ')' )
                {
                    ch = fgetc(f);
                    *sym = RPAREN;
                }
                else if(ch == '*' )
                {
                    ch = fgetc(f);
                    *sym = MUL;
                }
                else if(ch == '+' )
                {
                    ch = fgetc(f);
                    *sym = PLUS;
                }
                else if(ch == ',' )
                {
                    ch = fgetc(f);
                    *sym = COMMA;
                }
                else if(ch == '-' )
                {
                    ch = fgetc(f);
                    *sym = MINUS;
                }
                else if(ch == '.' )
                {
                    ch = fgetc(f);
                    if(ch == '.' )
                    {
                        ch = fgetc(f);
                        *sym = UPTO;
                    }
                    else *sym = PERIOD;
                }
                else if(ch == '/' )
                {
                    ch = fgetc(f);
                    *sym = RDIV;
                }
                else
                {
                    ch = fgetc(f);
                    *sym = 0;
                }
                // ! % '
            }
            else if(ch < ':' )
            {
                number(sym);
            }
            else if(ch == ':' )
            {
                ch = fgetc(f);
                if(ch == '=' )
                {
                    ch = fgetc(f);
                    *sym = BECOMES;
                }
                else *sym = COLON;
            }
            else if(ch == ';' )
            {
                ch = fgetc(f);
                *sym = SEMICOLON;
            }
            else if(ch == '<' )
            {
                ch = fgetc(f);
                if(ch == '=' )
                {
                    ch = fgetc(f);
                    *sym = LEQ;
                }
                else *sym = LSR;
            }
            else if(ch == '=' )
            {
                ch = fgetc(f);
                *sym = EQL;
            }
            else if(ch == '>' )
            {
                ch = fgetc(f);
                if(ch == '=' )
                {
                    ch = fgetc(f);
                    *sym = GEQ;
                }
                else *sym = GTR;
            }
            else
            {
                ch = fgetc(f);
                *sym = 0;
            }
            // ? @
        }
        else if(ch < '[' )
        {
            identifier(sym);
        }
        else if(ch < 'a' )
        {
            if(ch == '[' )
            {
                *sym = LBRAK;
            }
            else if(ch == ']' )
            {
                *sym = RBRAK;
            }
            else if(ch == '^' )
            {
                *sym = ARROW;
            }
            else *sym = 0; // _ `
            ch = fgetc(f);
        }
        else if(ch < '{' )
        {
            identifier(sym);
        }
        else
        {
            if(ch == '{' ) *sym = LBRACE;
            else if(ch == '}' ) *sym = RBRACE;
            else if(ch == '|' ) *sym = BAR;
            else if(ch == '~' ) *sym = NOT;
            else if(ch == 0x7F ) *sym = UPTO; //read the procedure number(); there we may get "3..67", then number() may set ch to 0X7F
            else *sym = 0;
            ch = fgetc(f);
        }
    }
    while(*sym == 0); //if true loop, else break
}

//this must be called before initScanner()
//this fills keytab[] (with language keywords) and kwx[]
void initObs()
{
    k = 0;
    kwx[0] = 0;
    kwx[1] = 0;
    enter_kw(IF, "IF");
    enter_kw(DO, "DO");
    enter_kw(OF, "OF");
    enter_kw(OR, "OR");
    enter_kw(TO, "TO");
    enter_kw(IN, "IN");
    enter_kw(IS, "IS");
    enter_kw(BY, "BY");
    kwx[2] = k;
    enter_kw(END, "END");
    enter_kw(NIL, "NIL");
    enter_kw(VAR, "VAR");
    enter_kw(DIV, "DIV");
    enter_kw(MOD, "MOD");
    enter_kw(FOR, "FOR");
    kwx[3] = k;
    enter_kw(ELSE, "ELSE");
    enter_kw(THEN, "THEN");
    enter_kw(TRU, "TRUE");
    enter_kw(TYPE, "TYPE");
    enter_kw(CASE, "CASE");
    kwx[4] = k;
    enter_kw(ELSIF, "ELSIF");
    enter_kw(FALS, "FALSE");
    enter_kw(ARRAY, "ARRAY");
    enter_kw(BEGIN, "BEGIN");
    enter_kw(CONST, "CONST");
    enter_kw(UNTIL, "UNTIL");
    enter_kw(WHILE, "WHILE");
    kwx[5] = k;
    enter_kw(RECORD, "RECORD");
    enter_kw(REPEAT, "REPEAT");
    enter_kw(RETURN, "RETURN");
    enter_kw(IMPORT, "IMPORT");
    enter_kw(MODULE, "MODULE");
    kwx[6] = k;
    enter_kw(POINTER, "POINTER");
    kwx[7] = k;
    kwx[8] = k;
    enter_kw(PROCEDURE, "PROCEDURE");
    kwx[9] = k;
}

//this must be called after initObs()
//points to first character of source file
void initScanner(FILE* f, int pos)
{
    errpos = pos;
    errcnt = 0;
    //point to first character of source file
    fseek(f, pos, SEEK_SET);
    //read one character so that scanner Get() will proceed in a loop
    ch = fgetc(f);
}

//copies identifier name from scanner variable id[] to desired destination
void CopyId(char *ident)
{
    strcpy(ident, id);
}
