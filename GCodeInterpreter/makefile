LDLIBS=-lgsl -lgslcblas -lm
CFLAGS=-Wall -Werror
OBJS=main.o garbagecollection.o ginstructions.o gthread.o heap.o instruction_type.o searches.o stack.o stats.o symbolTable.o
CC=gcc

default : interpreter

interpreter : $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) $(LDLIBS) -o interpreter

main.o : main.c instruction_type.h symbolTable.h stack.h stats.h heap.h gthread.h garbagecollection.h ginstructions.h searches.h machine.h

ginstructions.o : ginstructions.h ginstructions.c machine.h instruction_type.h symbolTable.h heap.h stack.h gthread.h stats.h

instruction_type.o : instruction_type.h instruction_type.c symbolTable.h lex.yy.c

symbolTable.o : instruction_type.h symbolTable.h

heap.o : heap.h gthread.h heap.c garbagecollection.h symbolTable.h

stack.o : heap.h stack.h stack.c

stats.o : stats.h stats.c machine.h instruction_type.h

gthread.o : gthread.h gthread.c machine.h

searches.o : searches.h machine.h instruction_type.h searches.c stats.h

garbagecollection.o : garbagecollection.h garbagecollection.c heap.h stack.h gthread.h machine.h

#main.o : main.c stack.c heap.c instruction_type.c gthread.c symbolTable.c garbagecollection.c lex.yy.c ginstructions.c searches.c

lex.yy.c: gcode.lex
	flex gcode.lex

clean :
	rm -f instruction_type.o main.o symbolTable.o stack.o stats.o heap.o gthread.o garbagecollection.o ginstructions.o searches.o interpreter

clang : CC=clang
clang : interpreter

debug : CFLAGS+=-g
debug : lex.yy.c interpreter

profile : CFLAGS+=-pg
profile : interpreter
