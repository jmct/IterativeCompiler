interpreter : main.o instructions.o symbolTable.o stack.o heap.o gthread.o garbagecollection.o
	gcc main.o instructions.o symbolTable.o stack.o heap.o gthread.o garbagecollection.o -o interpreter

instructions.o : instructions.h instructions.c
	gcc -c instructions.c

symbolTable.o : instructions.h symbolTable.h
	gcc -c symbolTable.c

heap.o : heap.h gthread.h
	gcc -c heap.c

stack.o : heap.h stack.h
	gcc -c stack.c

gthread.o : gthread.h
	gcc -c gthread.c

garbagecollection.o : garbagecollection.h heap.h stack.h gthread.h machine.h
	gcc -c garbagecollection.c

main.o : main.c stack.c heap.c instructions.c gthread.c symbolTable.c garbagecollection.c
	gcc -c main.c

clean :
	rm instructions.o main.o symbolTable.o stack.o heap.o gthread.o
