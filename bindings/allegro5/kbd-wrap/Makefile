CFLAGS = -fPIC -g -c -Wall -lallegro
LDFLAGS = -shared -soname
SONAME = libracket-allegro-keyboard-wrap.so
VERSION_MAJOR = 1
VERSION_FULL = 1.0.1

all: so

so: wrap.o
	ld $(LDFLAGS) $(SONAME).$(VERSION_MAJOR) -o $(SONAME).$(VERSION_FULL) -lc $^

%.o: %.c
	cc $(CFLAGS) -o $@ $<

install:
	sudo cp $(SONAME).$(VERSION_FULL) $(RACKET_LIBS)

clean:
	rm *.o
	rm $(SONAME).$(VERSION_FULL)

.PHONY: all so clean install
