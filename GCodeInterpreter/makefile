interpreter : main.o instruction_type.o symbolTable.o stack.o stats.o heap.o gthread.o garbagecollection.o ginstructions.o
	gcc main.o instruction_type.o symbolTable.o stack.o stats.o heap.o gthread.o garbagecollection.o ginstructions.o -lgsl -lgslcblas -lm -o interpreter

ginstructions.o : ginstructions.h ginstructions.c
	gcc -c ginstructions.c

instruction_type.o : instruction_type.h instruction_type.c
	gcc -c instruction_type.c

symbolTable.o : instruction_type.h symbolTable.h
	gcc -c symbolTable.c

heap.o : heap.h gthread.h
	gcc -c heap.c

stack.o : heap.h stack.h
	gcc -c stack.c

stats.o : stats.h stats.c
	gcc -c stats.c

gthread.o : gthread.h
	gcc -c gthread.c

garbagecollection.o : garbagecollection.h heap.h stack.h gthread.h machine.h
	gcc -c garbagecollection.c

main.o : main.c stack.c heap.c instruction_type.c gthread.c symbolTable.c garbagecollection.c lex.yy.c ginstructions.c
	gcc -c main.c

lex.yy.c: gcode.lex
	flex gcode.lex

clean :
	rm instruction_type.o main.o symbolTable.o stack.o stats.o heap.o gthread.o garbagecollection.o ginstructions.o

debug : lex.yy.c
	gcc -Wall -g main.c instruction_type.c symbolTable.c stack.c stats.c heap.c gthread.c garbagecollection.c ginstructions.c -lgsl -lgslcblas -lm -o interpreter
