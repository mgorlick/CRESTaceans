VP8WRAPPER = libracket-vp8-wrapper.so
VIDWRAPPER = libracket-v4l2-wrapper.so
CFLAGS = -fPIC -c -g -O3 -Wall -Wextra
LIBDEPS = $$(pkg-config --cflags --libs libswscale)
ifeq ($(strip $(OS)),Linux)
	VP8WRAPPER = libracket-vp8-wrapper.so
	VIDWRAPPER = libracket-v4l2-wrapper.so
else
	VP8WRAPPER = libracket-vp8-wrapper.dylib
endif


all: vp8 vid

vid: v4l2.o
	cc -shared -o $(VIDWRAPPER) $(LIBDEPS) $^

vp8: misc.o enc.o dec.o /usr/lib/libvpx.a
	cc -shared -o $(VP8WRAPPER) $(LIBDEPS) $^

%.o: %.c
	cc $(CFLAGS) -o $@ $<

install:
	cp $(VP8WRAPPER) $(RACKET_LIBS)
	cp $(VIDWRAPPER) $(RACKET_LIBS)

clean:
	rm -rf *.o
	rm -rf *.so*
	rm -rf *.dylib

.PHONY: all so clean install
