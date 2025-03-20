TARGET = x86_task

ASM = nasm
CC = gcc

ASM_FLAGS = -f elf32 -g -F dwarf
CFLAGS = -m32 -g -no-pie

ASM_SRC = hex_print.asm
C_SRC = x86_task.c  
OBJ = hex_print.o x86_task.o

all: $(TARGET)

$(TARGET): $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^

hex_print.o: $(ASM_SRC)
	$(ASM) $(ASM_FLAGS) -o $@ $<

x86_task.o: $(C_SRC)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(TARGET) *.o

run: $(TARGET)
	./$(TARGET)

debug: $(TARGET)
	gdb ./$(TARGET)
