# o7c

An Oberon7 cross compiler.

It compiles Oberon7 language and generates code for FPGA RISC-5 processor on Linux.
It is updated to Wirth's compiler update of 4 July 2016.
[More info at: https://www.inf.ethz.ch/personal/wirth/]

Compile:
$ gcc -Wall -o obc obs.c obt.c obg.c obp.c obc.c

Run:
$./obc Hello.Mod
