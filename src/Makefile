YACC = bison -y -d
LEX = flex
CC = gcc
TARGET = ../bin/romen
CFLAGS = -std=gnu99 -g -Wall

SRCS=$(filter-out y.tab.c lex.yy.c, $(wildcard *.c))
OBJS:=$(SRCS:.c=.o) parse.o
DEPS:=$(OBJS:.o=.d)

all : $(TARGET)

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

y.tab.c y.tab.h : parse.y lex.yy.c
	$(YACC) -o y.tab.c parse.y

lex.yy.c lex.yy.h : lex.l
	$(LEX) --header-file=lex.yy.h -o lex.yy.c lex.l

parse.o : y.tab.c lex.yy.c
	$(CC) -c y.tab.c -o parse.o

ast.o : y.tab.h lex.yy.h

$(TARGET) : $(OBJS)
	mkdir -p "$$(dirname $(TARGET))"
	$(CC) $(CFLAGS) $(OBJS) -o $(TARGET) -lgc

clean :
	rm -f y.tab.c y.tab.h
	rm -f lex.yy.c lex.yy.h
	rm -f *.d *.o $(TARGET)

.PHONY : all clean
