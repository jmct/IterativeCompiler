interpreter : main.o instructions.o symbolTable.o stack.o heap.o
	gcc main.o instructions.o symbolTable.o stack.o heap.o -o interpreter

instruction.o : instructions.h instructions.c
	gcc -c instructions.c

symbolTable.o : instructions.h symbolTable.h symbolTable.c
	gcc -c symbolTable.c

heap.o : heap.h
	gcc -c heap.c

stack.o : heap.h stack.h
	gcc -c stack.c

main.o : main.c
	gcc -c main.c

clean :
	rm instructions.o main.o symbolTable.o stack.o heap.o
