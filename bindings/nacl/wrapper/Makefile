UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
	SO = libnacl.dylib
	CFLAGS = -m32 -fPIC -c -g -O3 -Wall -Wextra -Werror
	SOCOMMAND = -m32 -dynamiclib -o $(SO) -dylib
else ifeq ($(UNAME), Linux)
	SO = libnacl.so
	CFLAGS = -fPIC -c -g -O3 -Wall -Wextra -Werror
	SOCOMMAND = -fPIC -O3 -g -shared -o $(SO)
endif

all: so

so: box.o sign.o hash.o cpucycles.o randombytes.o libnacl.a
	$(CC) $(SOCOMMAND) $^

%.o: %.c
	$(CC) $(CFLAGS) -o $@ $<

install:
	cp $(SO) $(RACKET_LIBS)

clean:
	rm -rf box.o sign.o hash.o $(SO)

.PHONY: all so clean install
