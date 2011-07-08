UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
	SO = libnacl.dylib
	CFLAGS = -m32 -fPIC -c -g -O3 -Wall
	SOCOMMAND = -m32 -dynamiclib -o $(SO) -dylib
else ifeq ($(UNAME), Linux)
	SO = libnacl.so
	CFLAGS = -fPIC -c -g -O3 -Wall
	SOCOMMAND = -shared -o $(SO)
endif

all: so

so: cpucycles.o randombytes.o libnacl.a box.o
	$(CC) $(SOCOMMAND) $^

box.o: box.c
	$(CC) $(CFLAGS) -o $@ $<

install:
	cp $(SO) $(RACKET_LIBS)

clean:
	rm -rf box.o $(SO)

.PHONY: all so clean install