
all: main.exe

main.exe: main.o
	gcc main.o -o main.exe

main.o: main.c *.h
	gcc -Wall -Werror -c main.c
