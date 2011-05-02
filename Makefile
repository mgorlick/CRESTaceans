all: c r

r: peer
	find "apps" -name "*.rkt" -print0 | xargs -0 raco make -v

peer: bindings
	find "peer" -name "*.rkt" -print0 | xargs -0 raco make -v

bindings:
	find "bindings" -name "*.rkt" -print0 | xargs -0 raco make -v

c:
	cd bindings/allegro5/kbd-wrap && make
	cd bindings/gstreamer/common-wrap && make
	cd bindings/vorbis/wrapper && make
	cd bindings/theora/wrapper && make

install:
	cd bindings/allegro5/kbd-wrap && make install RACKET_LIBS=$(RACKET_LIBS)
	cd bindings/gstreamer/common-wrap && make install RACKET_LIBS=$(RACKET_LIBS)
	cd bindings/vorbis/wrapper && make install RACKET_LIBS=$(RACKET_LIBS)
	cd bindings/theora/wrapper && make install RACKET_LIBS=$(RACKET_LIBS)

clean:
	find . -name "compiled" -type d -print0 | xargs -0 rm -rfv
	cd bindings/gstreamer/common-wrap && make clean
	cd bindings/allegro5/kbd-wrap && make clean
	cd bindings/vorbis/wrapper && make clean
	cd bindings/theora/wrapper && make clean

clean-junk:
	find . -name *~ -print0 | xargs -0 rm

.PHONY: apps peer bindings c install clean clean-junk
