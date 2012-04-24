SONAME = libracket-speex-wrapper.so
CFLAGS = -fPIC -c -g -O3 -Wall -Wextra
LIBDEPS = $$(pkg-config --cflags --libs libpulse-simple ao speex speexdsp)

all: so

so: speex.o
	cc -shared -o $(SONAME) $(LIBDEPS) $^

%.o: %.c
	cc $(CFLAGS) -o $@ $<

install:
	cp $(SONAME) $(RACKET_LIBS)

clean:
	rm -rf *.o
	rm -rf *.so*

.PHONY: all so clean install
