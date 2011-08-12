SONAME = libracket-vorbis-wrapper.so
CFLAGS = -fPIC -c -g -Wall -O3
LIBDEPS = $$(pkg-config --cflags --libs ao vorbis vorbisenc)

all: so

so: dec.o enc.o ogg.o conversions.o
	cc -shared -o $(SONAME) $(LIBDEPS) $^

%.o: %.c
	cc $(CFLAGS) -o $@ $<

install:
	cp $(SONAME) $(RACKET_LIBS)

clean:
	rm -rf *.o
	rm -rf *.so*

.PHONY: all so clean install
