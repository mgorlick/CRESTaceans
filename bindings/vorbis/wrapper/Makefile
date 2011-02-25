SONAME = libracket-vorbis-wrapper.so
VERSION_MAJOR = 1
VERSION_FULL = 1.0
LDFLAGS = -Wl,-soname,$(SONAME).$(VERSION_MAJOR)
CFLAGS = -fPIC -c -g -Wall $$(pkg-config --cflags --libs vorbis)

all: so

so: wrapper.o
	cc -shared $(LDFLAGS) \
		-o $(SONAME).$(VERSION_FULL) \
		/usr/lib/libvorbis.so $^

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
