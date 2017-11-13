//Oberon7 compiler
//Based on Niklaus Wirth's implementation
//Compile using gcc
//$ gcc -Wall obs.c obt.c obg.c obp.c obc.c

//This code is distributed under the GPL License.
//For more info, check: http://www.gnu.org/copyleft/gpl.html

//15 June 2016: Srinivas Nayak: This file created
//15 June 2016: Srinivas Nayak: Coding started

#include "obc.h"

int main(int argc, char **argv)
{
    if(argc!=2) //if a source file name is not given, exit.
        exit(0);

    f=fopen(argv[1],"r"); //open the source file to read.

    if(f==NULL)
    {
        printf("Error opening source file\n");
        exit(0);
    }

    initObs();
    initObt();
    initObp();
    initObg();

    Compile();

    return 0;
}


