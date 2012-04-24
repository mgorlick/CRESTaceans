UNAME := $(shell uname)
CFLAGS = -fPIC -c -g -O3 -Wall -Wextra $$(pkg-config --cflags libswscale vpx)
LIBDEPS = $$(pkg-config --libs libswscale vpx)
ifeq ($(UNAME),Linux)
	VP8WRAPPER = libracket-vp8-wrapper.so
	VIDWRAPPER = libracket-v4l2-wrapper.so
	TARGETS = vp8 vid
	ALL_SO = $(VP8WRAPPER) $(VIDWRAPPER)
endif
ifeq ($(UNAME),Darwin)
	VP8WRAPPER = libracket-vp8-wrapper.dylib
	TARGETS = vp8
	ALL_SO = $(VP8WRAPPER)
endif

all: $(TARGETS)

vid: v4l2.o
	cc -shared -o $(VIDWRAPPER) $^

vp8: misc.o enc.o dec.o
	cc -shared -o $(VP8WRAPPER) $^ $(LIBDEPS)

%.o: %.c
	cc $(CFLAGS) -o $@ $<

install:
	cp $(ALL_SO) $(RACKET_LIBS)

clean:
	rm -rf *.o
	rm -rf *.so*
	rm -rf *.dylib

.PHONY: all so clean install
