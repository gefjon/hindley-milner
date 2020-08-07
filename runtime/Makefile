CFLAGS = -Wall --std=c11

.PHONY: clean build

build: main.o

clean:
	rm *.o

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<
