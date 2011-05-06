SONAME = libracket-vp8-wrapper.so
VERSION_MAJOR = 1
VERSION_FULL = 1.0
LDFLAGS = -Wl,-soname,$(SONAME).$(VERSION_MAJOR)
CFLAGS = -fPIC -c -g -O3 -Wall
LIBDEPS = $$(pkg-config --cflags --libs sdl)

all: so

so: enc.o dec.o /usr/lib/libvpx.a
	cc -shared $(LDFLAGS) -o $(SONAME).$(VERSION_FULL) $^

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
