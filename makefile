interpreter : main.o instructions.o symbolTable.o
	gcc main.o instructions.o symbolTable.o -o interpreter

instruction.o : instructions.h instructions.c
	gcc -c instructions.c

symbolTable.o : instructions.h symbolTable.h symbolTable.c
	gcc -c symbolTable.c

main.o : main.c
	gcc -c main.c

clean :
	rm instructions.o main.o symbolTable.o
