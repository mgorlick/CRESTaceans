all: apps

apps: peer
	find "apps" -name "*.rkt" -print0 | xargs -0 raco make -v

peer: bindings
	find "peer" -name "*.rkt" -print0 | xargs -0 raco make -v

bindings:
	find "bindings" -name "*.rkt" -print0 | xargs -0 raco make -v
	cd bindings/allegro5/kbd-wrap && make
	cd bindings/gstreamer/common-wrap && make
	cd bindings/vorbis/wrapper && make

install:
	cd bindings/allegro5/kbd-wrap && make install RACKET_LIBS=$(RACKET_LIBS)
	cd bindings/gstreamer/common-wrap && make install RACKET_LIBS=$(RACKET_LIBS)
	cd bindings/vorbis/wrapper && make install RACKET_LIBS=$(RACKET_LIBS)

clean:
	find . -name "compiled" -type d -print0 | xargs -0 rm -rfv
	cd bindings/gstreamer/common-wrap && make clean
	cd bindings/allegro5/kbd-wrap && make clean
	cd bindings/vorbis/wrapper && make clean

clean-junk:
	find . -name *~ -print0 | xargs -0 rm

.PHONY: apps peer bindings install clean clean-junk
