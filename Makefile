all: c r

r: peer
	find "apps" -name "*.rkt" -print0 | xargs -0 raco make -v

peer: bindings
	find "peer" -name "*.rkt" -print0 | xargs -0 raco make -v

bindings:
	find "bindings" -name "*.rkt" -print0 | xargs -0 raco make -v

c:
	cd bindings/nacl/wrapper && make
	cd apps/coastcast/bindings/vp8/wrapper && make

install:
	cd bindings/nacl/wrapper && make install RACKET_LIBS=$(RACKET_LIBS)
	cd apps/coastcast/bindings/vp8/wrapper && make install RACKET_LIBS=$(RACKET_LIBS)

clean:
	find . -name "compiled" -type d -print0 | xargs -0 rm -rfv
	cd bindings/nacl/wrapper && make clean
	cd apps/coastcast/bindings/vp8/wrapper && make clean

clean-junk:
	find . -name *~ -print0 | xargs -0 rm

.PHONY: apps peer bindings c install clean clean-junk
