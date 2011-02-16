CFLAGS = -fPIC -g -c -Wall $$(pkg-config --cflags --libs gstreamer-0.10)
LDFLAGS = -shared -soname
SONAME = libracket-gst.so
VERSION_MAJOR = 1
VERSION_FULL = 1.0

all: so

so: wrap.o
	ld $(LDFLAGS) $(SONAME).$(VERSION_MAJOR) -o $(SONAME).$(VERSION_FULL) -lc $^

%.o: %.c
	cc $(CFLAGS) -o $@ $<

install:
	cp $(SONAME).$(VERSION_FULL) $(RACKET_LIBS)

clean:
	rm *.o
	rm $(SONAME).$(VERSION_FULL)

.PHONY: all so clean install
