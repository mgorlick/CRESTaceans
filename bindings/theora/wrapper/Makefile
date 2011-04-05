SONAME = libracket-theora-wrapper.so
VERSION_MAJOR = 1
VERSION_FULL = 1.0
LDFLAGS = -Wl,-soname,$(SONAME).$(VERSION_MAJOR)
CFLAGS = -fPIC -c -g -Wall $$(pkg-config --cflags --libs theoraenc) $$(pkg-config --cflags --libs theora)
LIBDEPS = /usr/lib/libtheoradec.so /usr/lib/libtheoraenc.so /usr/lib/libtheora.so

all: so

so: dec.o enc.o v4l2.o
	cc -shared $(LDFLAGS) \
		-o $(SONAME).$(VERSION_FULL) \
		 $(LIBDEPS) $^

%.o: %.c
	cc $(CFLAGS) -o $@ $<

install:
	cp $(SONAME).$(VERSION_FULL) $(RACKET_LIBS)
	ln -sf $(RACKET_LIBS)/$(SONAME).$(VERSION_FULL) \
		$(RACKET_LIBS)/$(SONAME)
	ln -sf $(RACKET_LIBS)/$(SONAME).$(VERSION_FULL) \
		$(RACKET_LIBS)/$(SONAME).$(VERSION_MAJOR)

clean:
	rm -rf *.o
	rm -rf $(SONAME).$(VERSION_FULL)

.PHONY: all so clean install
