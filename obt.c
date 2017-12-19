//Oberon7 symbol table
//Based on Niklaus Wirth's implementation
//Compile using gcc
//$ gcc -Wall obs.c obt.c obg.c obp.c obc.c

//This code is distributed under the GPL License.
//For more info, check: http://www.gnu.org/copyleft/gpl.html

//15 June 2016: Srinivas Nayak: This file created
//15 June 2016: Srinivas Nayak: Coding started

//Definition of Object and Type, which toGether form the data structure called "symbol table".
//Contains procedures for creation of Objects, and for search:
//NewObj, this, thisimport, thisfield (and OpenScope, CloseScope).
//Handling of import and export, i.e. reading and writing of "symbol files" is done by procedures Import and Export.
//This module contains the list of standard identifiers, with which
//the symbol table (universe), and that of the pseudo-module SYSTEM are initialized.

#include "obc.h"

#define MAXTYPTAB 64
int versionkey = 1;

//class values
int Head = 0, Const = 1, Var = 2, Par = 3, Fld = 4, Typ = 5,
    SProc = 6, SFunc = 7, Mod = 8;

//form values
int Byte = 1, Bool = 2, Char = 3, Int = 4, Real = 5, Set = 6,
    Pointer = 7, NilTyp = 8, NoTyp = 9, Proc = 10,
    String = 11, Array = 12, Record = 13;

Object topScope, universe, System;
Type byteType, boolType, charType, intType, realType, setType, nilType, noType, strType;
int nofmod, Ref;
Type typtab[MAXTYPTAB]; //each entry points to a TypeDesc struct
void NEW(void **p, int size)
{
    *p = malloc(size);
    if(*p == 0)
    {
        printf("malloc failed\n");
        exit(0);
    }
    memset(*p, 0, size);
}

//insert a new Object with name id
//or else if id already exists, returns it
void NewObj(Object *obj, char *id, int class)
{
    Object new=0, x=0;
    x = topScope;
    while ((x->next != 0) && (strcmp(x->next->name, id) != 0))
    {
        x = x->next;
    }

    if( x->next == 0 )
    {
        NEW((void **)&new, sizeof(ObjDesc));
        strcpy(new->name, id);
        new->class = class;
        new->next = 0;
        new->rdo = FALSE;
        new->dsc = 0;
        //insert it at tail
        x->next = new;
        *obj = new;
    }
    else
    {
        *obj = x->next;
        Mark("mult def");
    }
}

//search and if found return Object of id
//else return 0
//our higher level scopes stay as descendant of lower level scopes
//so, if id is not found in current scope, descend, that means,
//go one scope level up and search there
Object thisObj()
{
    Object s, x;
    s = topScope;
    do
    {
        x = s->next;
        while( (x != 0) && (strcmp(x->name, id) != 0) ) //loop till object name doesn't match
        {
            x = x->next;
        }
        s = s->dsc;
    }
    while( (x == 0) && (s != 0) ); //till this is true, loop
	                               //that means if object not yet found and higher scope is still available, loop
    return x;
}

//if module is not readonly, return 0
//else return Object of id if found
Object thisimport(Object mod)
{
    Object obj;
    if( mod->rdo )
    {
        if( mod->name[0] != '\0' )
        {
            obj = mod->dsc;
            while( (obj != 0) && (strcmp(obj->name, id) != 0) )
            {
                obj = obj->next;
            }
        }
        else
        {
            obj = 0;
        }
    }
    else
    {
        obj = 0;
    }
    return obj;
}

//returns Object of a record field
Object thisfield(Type rec)
{
    Object fld;
    fld = rec->dsc;
    while( (fld != 0) && (strcmp(fld->name, id) != 0) )
    {
        fld = fld->next;
    }
    return fld;
}

//creates a new scope for variables
//our higher level scopes stay as descendant of lower level scopes
//creates a Head Object which is pointed by topScope
void OpenScope()
{
    Object s=0;
    NEW((void **)&s, sizeof(ObjDesc));
    s->class = Head;
    s->dsc = topScope;
    s->next = 0;
    topScope = s;
}

//close scope descends, that means we go one level higher up
void CloseScope()
{
    topScope = topScope->dsc;
}


//------------------------------- Import --------------------------------------



//create file name adding name and ext
void MakeFileName(char *FName, char *name, char *ext)
{
    int i, j;
    i = 0; //assume name suffix less than 4 characters
    j = 0;
    while( (i < ID_LEN-5) && (name[i] > '\0') )
    {
        FName[i] = name[i];
        i++;
    }
    do
    {
        FName[i] = ext[j];
        i++;
        j++;
    }
    while(!( ext[j] == '\0' ));
    FName[i] = '\0';
}

//search the Object of 'name' module
Object ThisModule(char* name, char *orgname, int non, int key)
{
    Module mod=0;
    Object obj, obj1;
    obj1 = topScope;
    obj = obj1->next;
    //search for module
    while( (obj != 0) && (strcmp(obj->name, name) != 0) )
    {
        obj1 = obj;
        obj = obj1->next;
    }
    //if not found, insert new module
    if( obj == 0 )
    {
        NEW((void **)&mod, sizeof(ModDesc));
        mod->objdesc.class = Mod;
        mod->objdesc.rdo = FALSE;
        strcpy(mod->objdesc.name, name);
        strcpy(mod->orgname, orgname);
        mod->objdesc.val = key;
        mod->objdesc.lev = nofmod;
        nofmod++;
        mod->objdesc.type = noType;
        mod->objdesc.dsc = 0;
        mod->objdesc.next = 0;
        obj1->next = (Object)mod;
        obj = (Object)mod;
    }
    else //module already present
    {
        if( non )
        {
            Mark("invalid import order");
        }
    }
    return obj;
}

//read a byte from file
//if value is < 128, (i.e <= 127) return it
//else return -128, -127, -126,...,-1
void Read(FILE *R, int *x)
{
    unsigned char b;
    b = fgetc(R); //fgetc returns an int, not char
    if( b < 0x80 )
    {
        *x = b;
    }
    else
    {
        *x = b - 0x100;
    }
}

unsigned int ROR(unsigned int x, int n)
{
    return (x >> (n%32))|(x << ((32-n)%32));
}

int ASR(int x, int n)
{
    return (x >> n%32);
}

void ReadNum(FILE* R, int* x)
{
    int y, n;
    unsigned char b;

    n = 32;
    y = 0;
    b = fgetc(R);

    while( b >= 0x80 )
    {
        y = ROR(y|(b-0x80), 7);
        n = n-7;
        b = fgetc(R);
    }

    if( n <= 4 )
    {
        *x = ROR(y|(b&0xF), 4);
    }
    else
    {
        *x = ASR(ROR(y|b, 7), n-7);
    }
}

void WriteNum(FILE* R, int x)
{
    unsigned char b;

    while( (x < -0x40) || (x >= 0x40) )
    {
        b = (x & 0x7F)|0x80;
        fputc(b, R);
        x = ASR(x, 7);
    }

    b = (x & 0x7F);
    fputc(b, R);
}


void ReadString(FILE *R, char *buf, int bufsize)
{
    int i = 0;
    char c;
    c = fgetc(R); //reads at least one character!
    while(!feof(R) && c != '\0')
    {
        if(i < bufsize-1)
        {
            buf[i] = c;
            i++;
        }
        c = fgetc(R);
    }
    buf[i] = '\0';
}

//recursive procedure!
void InType(FILE *R, Object thismod, Type *T)
{
    int key;
    int ref=0, class=0, form=0, np=0, readonly=0;
    Object fld=0, par=0, obj=0, mod=0;
    Type t=0;
    char name[ID_LEN], modname[ID_LEN];

    printf("pos = %ld ", ftell(R));

    Read(R, &ref);
    printf("type_ref=%d ", ref);
    if( ref < 0 ) //already read
    {
        *T = typtab[-ref];
    }
    else
    {
        NEW((void **)&t, sizeof(TypeDesc));
        *T = t;
        typtab[ref] = t;
        t->mno = thismod->lev;
        Read(R, &form);
        t->form = form;
        printf("type_form=%d ", form);
        if( form == Pointer )
        {
            InType(R, thismod, &(t->base));
            t->size = 4;
        }
        else if( form == Array )
        {
            InType(R, thismod, &(t->base));
            ReadNum(R, &(t->len));
            ReadNum(R, &(t->size));
            printf("type_arrlen=%d ", t->len);
            printf("type_arrsize=%d ", t->size);
        }
        else if( form == Record )
        {
            InType(R, thismod, &(t->base));
            if( t->base->form == NoTyp )
            {
                t->base = 0;
                obj = 0;
            }
            else
            {
                obj = t->base->dsc;
            }
            ReadNum(R, &(t->len)); //TypeDesc adr/exno //why? should it be t->exno?
            ReadNum(R, &(t->nofpar)); //ext level
            ReadNum(R, &(t->size));
            printf("type_reclen=%d ", t->len);
            printf("type_recnofpar=%d ", t->nofpar);
            printf("type_recsize=%d ", t->size);
            Read(R, &class);
            printf("fld_class=%d ", class);
            while( class != 0 ) //fields
            {
                NEW((void **)&fld, sizeof(ObjDesc));
                fld->class = class;
                ReadString(R, fld->name, ID_LEN);
                printf("fld_name=%s ", fld->name);
                if( fld->name[0] != 0x0 )
                {
                    fld->expo = TRUE;
                    InType(R, thismod, &(fld->type));
                }
                else
                {
                    fld->expo = FALSE;
                    fld->type = nilType;
                }
                ReadNum(R, &(fld->val));
                printf("fld_val=%d ", fld->val); //offset
                fld->next = obj; //base record's elements follows derived record's elements
                obj = fld; //this way record extension works!
                Read(R, &class);
                printf("fld_class=%d ", class);
            }
            t->dsc = obj; //first this type's elements are there and after that we have elements of base record connected
        }
        else if( form == Proc )
        {
            InType(R, thismod, &(t->base));
            obj = 0;
            np = 0;
            Read(R, &class);
            printf("par_class=%d ", class);
            while( class != 0 ) //parameters
            {
                NEW((void **)&par, sizeof(ObjDesc));
                par->class = class;
                Read(R, &readonly);
                printf("par_rdo=%d ", readonly);
                par->rdo = (readonly == 1);
                InType(R, thismod, &(par->type));
                par->next = obj;
                obj = par;
                np++;
                Read(R, &class);
                printf("par_class=%d ", class);
            }
            t->dsc = obj;
            t->nofpar = np;
            t->size = 4;
        }
        //why? is this needed? needed for reexported types
        ReadString(R, modname, ID_LEN);
        printf("reexpo_modname=%s ", modname);
        if( modname[0] != 0x0 ) //re-import
        {
            fread(&(key), sizeof(int), 1, R);
            ReadString(R, name, ID_LEN);
            printf("reexpo_key=%#x ", key);
            printf("reexpo_objname=%s ", name);
            mod = ThisModule(modname, modname, FALSE, key);
            obj = mod->dsc; //search type
            while( (obj != 0) && (strcmp(obj->name, name)!=0) )
            {
                obj = obj->next;
            }
            if( obj != 0 ) //type object found in object list of mod
            {
                *T = obj->type;
            }
            else //insert new type object in object list of mod
            {
                NEW((void **)&obj, sizeof(ObjDesc));
                strcpy(obj->name, name);
                obj->class = Typ;
                obj->next = mod->dsc;
                mod->dsc = obj;
                obj->type = t;
                t->mno = mod->lev;
                t->typobj = obj;
                *T = t;
            }
            typtab[ref] = *T;
        }
    }
}

void Import(char *modid, char *modid1)
{
    int key=0, class=0, k=0;
    Object obj=0;
    Type t=0;
    Object thismod=0;
    char modname[ID_LEN];
    char fname[ID_LEN];
    FILE* R;

	//if imported module is SYSTEM, add it
    if( strcmp(modid1, "SYSTEM") == 0 )
    {
        thismod = ThisModule(modid, modid1, TRUE, key); //create a module ObjDesc
        nofmod--;
        thismod->lev = 0;
        thismod->dsc = System; //module's 'next' points to another module
        thismod->rdo = TRUE; //module's 'dsc' points to modules's constants, types, variables and procedures
    }
    else //else import from modulename.smb file
    {
        MakeFileName(fname, modid1, ".smb");
        R = fopen(fname, "rb"); //modulename.smb file opened
        if( R != 0 )
        {
            fread(&(key), sizeof(int), 1, R);
            printf("\nimport_k=%d ", key);
            fread(&(key), sizeof(int), 1, R);
            printf("import_key=%#x ", key); //why ReadInt twice? see Export()
            ReadString(R, modname, ID_LEN);
            printf("import_modname=%s ", modname);
            thismod = ThisModule(modid, modid1, TRUE, key); //create a module ObjDesc
            thismod->rdo = TRUE;
            Read(R, &class); //version key
            printf("import_versionkey=%d ", class);
            if( class != versionkey )
            {
                Mark("wrong version");
            }
            //start reading {object}
            Read(R, &class);
            printf("\nobj_class=%d ", class);
            while( (!feof(R)) && (class != 0) )
            {
                NEW((void **)&obj, sizeof(ObjDesc));
                obj->class = class;
                ReadString(R, obj->name, 32);
                printf("obj_name=%s ", obj->name);
                InType(R, thismod, &(obj->type));
                obj->lev = -(thismod->lev); //all objects are at module's level
                if( class == Typ )
                {
                    t = obj->type;
                    t->typobj = obj;
                    Read(R, &k); //fixup bases of previously declared pointer types
                    printf("obj_fixref=%d ", k);
                    while( k != 0 )
                    {
                        typtab[k]->base = t;
                        Read(R, &k);
                        printf("obj_fixref=%d ", k);
                    }
                }
                else
                {
                    if( class == Const )
                    {
                        if( obj->type->form == Real )
                        {
                            fread(&(obj->val), sizeof(int), 1, R);
                            printf("obj_realval=%#x ", obj->val);
                        }
                        else
                        {
                            ReadNum(R, &(obj->val));
                            printf("obj_intval=%d ", obj->val);
                        }
                    }
                    else if( class == Var )
                    {
                        ReadNum(R, &(obj->val));
                        printf("obj_varexno=%d ", obj->val); //why? should it be obj->exno?
                        obj->rdo = TRUE;
                    }
                }
                obj->next = thismod->dsc;
                thismod->dsc = obj;
                Read(R, &class);
                printf("\nobj_class=%d ", class);
            }
            fclose(R);
        }
        else
        {
            Mark("import not available");
        }
    }
}


//------------------------------- Export ------------------------------


//write one byte only
void Write(FILE *R, int x)
{
    fputc(x, R);
}

//writes a string with terminating '\0'
void WriteString(FILE *R, char *buf)
{
    int i = 0;
    char c;
    do
    {
        c = buf[i];
        fputc(c, R);
        i++;
    }
    while(c != '\0');
}

void OutType(FILE *R, Type t);
void OutPar( FILE *R, Object par, int n)
{
    int cl;
    if( n > 0 )
    {
        OutPar(R, par->next, n-1); //last parameter written first
        cl = par->class;
        Write(R, cl);
        printf("par_class=%d ", cl);
        if( par->rdo )
        {
            Write(R, 1);
            printf("par_rdo=%d ", par->rdo);
        }
        else
        {
            Write(R, 0);
            printf("par_!rdo=%d ", par->rdo);
        }
        OutType(R, par->type);
    }
}

//this is needed for Garbage Collector
void FindHiddenPointers(FILE *R, Type typ, int offset)
{
    Object fld;
    int i, n;
    if( (typ->form == Pointer) || (typ->form == NilTyp) ) //why? why NilTyp?
    {
        Write(R, Fld);
        printf("fld_class=%d ", Fld);
        Write(R, 0); //why? field name not necessary? what about field type?
        printf("fld_name=%d ", 0);
        WriteNum(R, offset);
        printf("fld_offset=%d ", offset);
    }
    else if( typ->form == Record )
    {
        fld = typ->dsc;
        while( fld != 0 )
        {
            FindHiddenPointers(R, fld->type, fld->val + offset);
            fld = fld->next;
        }
    }
    else if( typ->form == Array )
    {
        i = 0;
        n = typ->len;
        while( i < n )
        {
            FindHiddenPointers(R, typ->base, typ->base->size * i + offset);
            i++;
        }
    }
}

void OutType(FILE *R, Type t)
{
    Object obj, mod, fld;

    if( t->ref > 0 ) //if this type was already output to smb file
    {   //all primary types comes to this case
        Write(R, -(t->ref)); //write its negative ref now onwards, for this type references
        printf("type_ref=%d ", -(t->ref));
    }
    else //this type is written to smb file for first time; ref = 0
    {   //all secondary types such as array, pointer, record and procedure comes to this case
        obj = t->typobj;
        if( obj != 0 )
        {
            Write(R, Ref); //write next Ref number
            printf("type_Ref=%d ", Ref); //anonymous
            t->ref = Ref; //put this ref number for this type for future reference in this function
            Ref++; //increment global Ref count
        }
        else
        {
            Write(R, 0); //write 0
            printf("type_Ref=%d ", 0);
        }

        Write(R, t->form); //form is written for both primary and secondary types
        printf("type_form=%d ", t->form);
        if( t->form == Pointer )
        {
            OutType(R, t->base);
        }
        else if( t->form == Array )
        {
            OutType(R, t->base);
            WriteNum(R, t->len);
            printf("type_arrlen=%d ", t->len);
            WriteNum(R, t->size);
            printf("type_arrsize=%d ", t->size);
        }
        else if( t->form == Record )
        {
            if( t->base != 0 )
            {
                OutType(R, t->base); //if extension has been used
            }
            else
            {
                OutType(R, noType); //why? can't we put 0?
            }

            if( obj != 0 )
            {
                WriteNum(R, obj->exno);
                printf("type_recexno=%d ", obj->exno);
            }
            else
            {
                Write(R, 0);
                printf("type_recexno=%d ", 0);
            }

            WriteNum(R, t->nofpar);
            WriteNum(R, t->size);
            printf("type_recnofpar=%d ", t->nofpar);
            printf("type_recsize=%d ", t->size);
            fld = t->dsc;
            while( fld != 0 ) //fields
            {
                if( fld->expo )
                {
                    Write(R, Fld);
                    WriteString(R, fld->name);
                    printf("fld_class=%d ", Fld);
                    printf("fld_name=%s ", fld->name);
                    OutType(R, fld->type);
                    WriteNum(R, fld->val); //offset
                    printf("fld_val=%#x ", fld->val);
                }
                else
                {
                    FindHiddenPointers(R, fld->type, fld->val); //offset
                }
                fld = fld->next;
            }
            Write(R, 0);
            printf("0=%d ", 0);
        }
        else if( t->form == Proc )
        {
            OutType(R, t->base);
            OutPar(R, t->dsc, t->nofpar);
            Write(R, 0);
            printf("0=%d ", 0);
        }
        //why? this code needed? needed for reexported types
        if( (t->mno > 0) && (obj != 0) ) //re-export, output name
        {
            mod = topScope->next;
            while( (mod != 0) && (mod->lev != t->mno) )
            {
                mod = mod->next;
            }
            if( mod != 0 )
            {
                WriteString(R, mod->name);
                fwrite(&(mod->val), sizeof(int), 1, R); //why? because mod->val is module key
                WriteString(R, obj->name);
                printf("reexpo_modname=%s ", mod->name);
                printf("reexpo_modval=%#x ", mod->val);
                printf("reexpo_objname=%s ", obj->name);
            }
            else
            {
                Mark("re-export not found");
                Write(R, 0);
                printf("reexpo_modname=%d ", 0);
            }
        }
        else
        {
            Write(R, 0);
            printf("reexpo_modname=%d ", 0);
        }
    }
}

int file_length(FILE *f)
{
    int len;
    fpos_t position;

    fgetpos (f, &position);

    fseek (f, 0, SEEK_END);
    len=ftell(f);

    fsetpos (f, &position);
    return len;
}

void Export(char* modid, int *newSF, int *key)
{
    int x, sum, oldkey;
    Object obj, obj0;
    char filename[ID_LEN];
    FILE *R, *R1;
    int k=0, t;
    char oldname[ID_LEN], newname[ID_LEN];

    Ref = Record + 1; //ref starts at 14
    MakeFileName(filename, modid, ".tsf"); //tsf is for temporary symbol file
    strcpy(newname, filename); //newname = modulename.tsf
    R = fopen(newname, "wb"); //modulename.tsf created
    if(R == NULL)
    {
        Mark("can't create temp symbol file");
    }
    fwrite(&(k), sizeof(int), 1, R); //placeholder not used now
    printf("export_k=%d ", k);
    fwrite(&(k), sizeof(int), 1, R); //placeholder for key to be inserted at the end
    printf("export_key=%d ", k);
    WriteString(R, modid); //module name with '\0'
    Write(R, versionkey); //1 byte version info
    printf("export_modid=%s ", modid);
    printf("export_versionkey=%d ", versionkey);
    obj = topScope->next; //go to first ObjDesc after Head ObjDesc
    while( obj != 0 )
    {
        if( obj->expo ) //if object is exported with a '*' mark
        {
            Write(R, obj->class); //class of object, Typ, Var, Const etc.
            WriteString(R, obj->name); //identifier name
            printf("\nobj_class=%d ", obj->class);
            printf("obj_name=%s ", obj->name);
            OutType(R, obj->type);
            if( obj->class == Typ ) //if it is from TYPE section of code
            {
                if( obj->type->form == Record )
                {
                    obj0 = topScope->next; //start from the begining objects
                    while( obj0 != obj ) //check previously declared objects (denoted by obj0)
                    {
                        if( (obj0->type->form == Pointer) && (obj0->type->base == obj->type) && (obj0->type->ref > 0)
                          ) //check whether obj is base of previously declared pointer types (denoted by obj0)
                        {
                            Write(R, obj0->type->ref); //{fix} are ref of previous pointers refering to this record
                            printf("obj_fixref=%d ", obj0->type->ref);
                        }
                        obj0 = obj0->next;
                    }
                }
                Write(R, 0); //end with a 0
                printf("0=%d ", 0);
            }
            else if( obj->class == Const ) //if it is from CONST section of code
            {
                if( obj->type->form == Proc )
                {
                    WriteNum(R, obj->exno); //export number of procedure
                    printf("obj_procexno=%d ", obj->exno);
                }
                else if( obj->type->form == Real )
                {
                    fwrite(&(obj->val), sizeof(int), 1, R); //value of REAL CONST
                    printf("obj_realval=%#x ", obj->val);
                }
                else
                {
                    WriteNum(R, obj->val); //value of INTEGER CONST
                    printf("obj_intval=%d ", obj->val);
                }
            }
            else if( obj->class == Var ) //if it is from VAR section of code
            {
                WriteNum(R, obj->exno); //export number of variable
                printf("obj_varexno=%d ", obj->exno);
                //why? below code seems invalid
                if( obj->type->form == String )
                {
                    t = obj->val >> 16;
                    WriteNum(R, t);
                    printf("obj_strval=%#x ", t);
                    t = obj->val & 0xFFFF;
                    obj->val = t;
                }
            }
        }
        obj = obj->next;
    }
    //file length should be multiple of 4
    //so fill the rest space with 0
    do
    {
        Write(R, 0);
        printf("0=%d ", 0);
    }
    while(file_length(R)%4 != 0);
    //clean typtab[]
    for (Ref = Record+1; Ref <= MAXTYPTAB-1; Ref++)
    {
        typtab[Ref] = 0;
    }
    fclose(R);

    R = fopen(newname, "rb");
    if(R == NULL)
    {
        Mark("can't open temp symbol file");
    }
    sum = 0;
    fread(&(x), sizeof(int), 1, R);
    while( !feof(R) )
    {
        sum = sum + x; //compute checksum; this will be module key
        fread(&(x), sizeof(int), 1, R);
    }
    fclose(R);

    MakeFileName(filename, modid, ".smb");
    strcpy(oldname, filename); //oldname = modulename.smb
    R1 = fopen(oldname, "rb"); //old modulename.smb file opened
    if(R1 == NULL)
    {
        printf("symbol file was not there\n"); //sum is new key
    }

    if( R1 != NULL) //symbol file was there
    {
        fseek( R1, 4, SEEK_SET );
        fread(&(oldkey), sizeof(int), 1, R1); //so read old key
    }
    else //symbol file was not there
    {
        oldkey = sum+1; //so just make old key different than current checksum
    }

    if(R1 != NULL)
    {
        fclose(R1);
    }


    R = fopen(newname, "rb+"); //open modulename.tsf file
    if(R == NULL)
    {
        Mark("can't open temp symbol file");
    }

    if( sum != oldkey ) //key different means modulename.smb file need to be (over)written
    {
        if( newSF || (R1 == 0) ) //if compiler option given or old modulename.smb file was not there
        {
            *key = sum;
            *newSF = TRUE;
            fseek( R, 4, SEEK_SET );
            fwrite(&(sum), sizeof(int), 1, R); //insert checksum in the modulename.tsf file
            printf("\nexport_sum=%#x ", sum);
            fclose(R);
            //rename .tsf to .smb
            //newname = modulename.tsf
            //oldname = modulename.smb
            rename( newname, oldname );
        }
        else
        {
            Mark("new symbol file inhibited");
            fclose(R);
        }
    }
    else //old key and new key same; so symbol file need not be overwritten
    {
        *newSF = FALSE;
        *key = sum;
        fclose(R);
        printf("new symbol file not needed\n");
        remove( newname); //
    }
}

//------------------------------- Initialization ------------------------------

//creates one TypeDesc structure and fills in details
//keeps the TypeDesc pointer in typtab array
Type type(int ref, int form, int size)
{
    Type tp=0;
    NEW((void **)&tp, sizeof(TypeDesc));
    tp->ref = ref; //reference number of primary types
    tp->form = form;
    tp->size = size;
    tp->base = 0;
    typtab[ref] = tp;
    return tp;
}

//creates one ObjDesc structure and fills in details
//for primary data types, it keeps the ObjDesc pointer in TypDesc
//not for predefined functions and procedures
void enter(char *name, int cl, Type type, int n)
{
    Object obj=0;
    NEW((void **)&obj, sizeof(ObjDesc));
    strcpy(obj->name, name);
    obj->class = cl;
    obj->type = type;
    obj->val = n;
    obj->dsc = 0;
    if( cl == Typ )
    {
        type->typobj = obj;
    }
    obj->next = System;
    System = obj;
}

//this must be called before InitSymbolTable()
void initObt()
{
	//TypeDesc is created for each primary data types
	//typetab[Byte] points to Byte's TypeDesc
	//typetab[Bool] points to Bool's TypeDesc etc.
	//also for convenience, byteType points to Byte's TypeDesc
	//boolType points to Bool's TypeDesc etc.
    byteType = type(Byte, Int, 1); //The type BYTE is compatible with the type INTEGER, and vice-versa.
    boolType = type(Bool, Bool, 1);
    charType = type(Char, Char,1);
    intType = type(Int, Int, 4);
    realType = type(Real, Real, 4);
    setType = type(Set, Set,4);

    nilType = type(NilTyp, NilTyp, 4); //its corresponding Object doesn't exist! why? size 4?
    noType = type(NoTyp, NoTyp, 4); //its corresponding Object doesn't exist!
    strType = type(String, String, 8); //its corresponding Object doesn't exist!

    //initialize universe with primary datatypes and predefined procedures
    //by default available in language

    //all the enter() below makes a link list of ObjDesc
    //for primary datatypes and predefined procedures
    //System now temporarily points to this linked list
    System = 0; //System is temporarily being used here

    //functions
    enter("UML", SFunc, intType, 132); //n = procno*10 + nofpar; why? see StandFunc()
    enter("SBC", SFunc, intType, 122); //not in language definition
    enter("ADC", SFunc, intType, 112); //not in language definition
    enter("ROR", SFunc, intType, 92);
    enter("ASR", SFunc, intType, 82);
    enter("LSL", SFunc, intType, 72);
    enter("LEN", SFunc, intType, 61);
    enter("CHR", SFunc, charType, 51);
    enter("ORD", SFunc, intType, 41);
    enter("FLT", SFunc, realType, 31);
    enter("FLOOR", SFunc, intType, 21);
    enter("ODD", SFunc, boolType, 11);
    enter("ABS", SFunc, intType, 1);

    //procedures
    enter("LED", SProc, noType, 81);    //not in language definition
    enter("UNPK", SProc, noType, 72);
    enter("PACK", SProc, noType, 62);
    enter("NEW", SProc, noType, 51);
    enter("ASSERT", SProc, noType, 41);
    enter("EXCL", SProc, noType, 32);
    enter("INCL", SProc, noType, 22);
    enter("DEC", SProc, noType, 11);
    enter("INC", SProc, noType, 1);

    //types
    enter("SET", Typ, setType, 0);
    enter("BOOLEAN", Typ, boolType, 0);
    enter("BYTE", Typ, byteType, 0);
    enter("CHAR", Typ, charType, 0);
    enter("LONGREAL", Typ, realType, 0); //LONGREAL is synonym to REAL
    enter("REAL", Typ, realType, 0);
    enter("LONGINT", Typ, intType, 0); //LONGINT is synonym to INTEGER
    enter("INTEGER", Typ, intType, 0);
    //above enter()s link ObjDesc to TypeDesc and vice versa.

    topScope = 0;
    //OpenScope() creates a Head ObjDesc to which topScope points
    OpenScope();
    //Head ObjDesc points to the link list of ObjDesc created by previous enter()s.
    topScope->next = System;
    //universe and topScope are same now, pointing to same Head ObjDesc
    //that means, universe points to Head Object which again points to all ObjDesc
    //that corresponds to procedures, functions and types available in the language by default
    universe = topScope;

    //now initialize 'unsafe' pseudo-module SYSTEM
    //all the enter() below makes a link list of ObjDesc to which System points
    //that means, System points to all ObjDesc that corresponds to procedures and functions
    //available in module SYSTEM. SYSTEM module procedures/functions are hardware dependent.
    System = 0;

    //functions
    enter("H", SFunc, intType, 201);
    enter("COND", SFunc, boolType, 191); //not in language definition
    enter("SIZE", SFunc, intType, 181);
    enter("ADR", SFunc, intType, 171);
    enter("VAL", SFunc, intType, 162);
    enter("REG", SFunc, intType, 151); //not in language definition
    enter("BIT", SFunc, boolType, 142);

    //procedures
    enter("LDREG", SProc, noType, 142); //not in language definition
    enter("LDPSR", SProc, noType, 131); //not in language definition
    enter("COPY", SProc, noType, 123);
    enter("PUT", SProc, noType, 112);
    enter("GET", SProc, noType, 102);
}

//this must be called after initObt()
//makes topScope pointing to universe
void InitSymbolTable()
{
    topScope = universe;
    nofmod = 1;
}
