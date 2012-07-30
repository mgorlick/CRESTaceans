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

install: check-env
	cd bindings/nacl/wrapper && make install RACKET_LIBS=$(RACKET_LIBS)
	cd apps/coastcast/bindings/vp8/wrapper && make install RACKET_LIBS=$(RACKET_LIBS)

clean:
	find . -name "compiled" -type d -print0 | xargs -0 rm -rfv
	cd bindings/nacl/wrapper && make clean
	cd apps/coastcast/bindings/vp8/wrapper && make clean

clean-junk:
	find . -name *~ -print0 | xargs -0 rm

check-env:
ifndef RACKET_LIBS
	@echo 1>&2 "RACKET_LIBS environment variable must be set! e.g., ~/racket/lib"
	exit 1
endif

setup: check-env
	@/bin/bash -c "pushd .;\
	cd libs; \
	./install-fastlz.sh;\
	./install-nacl.sh;\
	find `pwd` -path '*build/*/include*' -name '*.h' -type f -print0 | xargs -0 cp -t '../bindings/nacl/wrapper';\
	find `pwd` -path '*build/*/lib*' -name '*.[o|a]' -type f -print0 | xargs -0 cp -t '../bindings/nacl/wrapper';\
	popd;\
	make;\
	make install;"

.PHONY: apps peer bindings c install clean clean-junk setup check-env
