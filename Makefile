VPATH = bindings

all: apps

apps: peer
	find "apps" -name "*.rkt" -print0 | xargs -0 raco make -v

peer: bindings
	find "peer" -name "*.rkt" -print0 | xargs -0 raco make -v

bindings:
	find "bindings" -name "*.rkt" -print0 | xargs -0 raco make -v
	cd bindings/gstreamer/common-wrap && make

install:
	cd bindings/gstreamer/common-wrap && make install

clean:
	find . -name "compiled" -type d -print0 | xargs -0 rm -rfv
	cd bindings/gstreamer/common-wrap && make clean

clean-junk:
	find . -name *~ -print0 | xargs -0 rm

.PHONY: apps peer bindings install clean clean-junk
