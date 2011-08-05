VP8WRAPPER = libracket-vp8-wrapper.so
VIDWRAPPER = libracket-v4l2-wrapper.so
CFLAGS = -fPIC -c -g -O3 -Wall
LIBDEPS = $$(pkg-config --cflags --libs libswscale)

all: vp8 vid

vid: v4l2.o
	cc -shared -o $(VIDWRAPPER) $(LIBDEPS) $^

vp8: enc.o dec.o /usr/lib/libvpx.a
	cc -shared -o $(VP8WRAPPER) $(LIBDEPS) $^

%.o: %.c
	cc $(CFLAGS) -o $@ $<

install:
	cp $(VP8WRAPPER) $(RACKET_LIBS)
	cp $(VIDWRAPPER) $(RACKET_LIBS)

clean:
	rm -rf *.o
	rm -rf *.so*

.PHONY: all so clean install
