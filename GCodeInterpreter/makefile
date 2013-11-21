interpreter : main.o instructions.o symbolTable.o stack.o stats.o heap.o gthread.o garbagecollection.o
	gcc main.o instructions.o symbolTable.o stack.o stats.o heap.o gthread.o garbagecollection.o -o interpreter

instructions.o : instructions.h instructions.c
	gcc -c instructions.c

symbolTable.o : instructions.h symbolTable.h
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

main.o : main.c stack.c heap.c instructions.c gthread.c symbolTable.c garbagecollection.c lex.yy.c
	gcc -c main.c

lex.yy.c: gcode.lex
	flex gcode.lex

clean :
	rm instructions.o main.o symbolTable.o stack.o stats.o heap.o gthread.o garbagecollection.o

debug : lex.yy.c
	gcc -Wall -g main.c instructions.c symbolTable.c stack.c stats.c heap.c gthread.c garbagecollection.c -lgsl -lgslcblas -lm -o interpreter
