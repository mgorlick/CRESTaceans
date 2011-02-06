all: apps

apps: peer
	find "apps" -name "*.rkt" -print0 | xargs -0 raco make -v

peer: bindings
	find "peer" -name "*.rkt" -print0 | xargs -0 raco make -v

bindings:
	sh bindings/gstreamer/common-wrap/compile
	find "bindings" -name "*.rkt" -print0 | xargs -0 raco make -v

install:
	find "bindings" -name "*.so*" -print0 | xargs -0 sudo cp -t $(RACKET_LIBS)

clean:
	find "bindings" -name "*.so*" -print0 | xargs -0 rm -v
	find "bindings" -name "*.o" -print0 | xargs -0 rm -v
	find . -name "compiled" -type d -print0 | xargs -0 rm -rfv

clean-junk:
	find . -name *~ -print0 | xargs -0 rm

.PHONY: apps peer bindings install clean clean-junk
