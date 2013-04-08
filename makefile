interpreter : main.o instructions.o symbolTable.o stack.o heap.o gthread.o
	gcc main.o instructions.o symbolTable.o stack.o heap.o gthread.o -o interpreter

instructions.o : instructions.h instructions.c
	gcc -c instructions.c

symbolTable.o : instructions.h symbolTable.h symbolTable.c
	gcc -c symbolTable.c

heap.o : heap.h
	gcc -c heap.c

stack.o : heap.h stack.h stack.c
	gcc -c stack.c

gthread.o: gthread.h
	gcc -c gthread.c

main.o : main.c stack.c heap.c instructions.c gthread.c symbolTable.c
	gcc -c main.c

clean :
	rm instructions.o main.o symbolTable.o stack.o heap.o gthread.o
