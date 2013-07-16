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
	cd apps/coastcast/bindings/vp8/wrapper && make install
	RACKET_LIBS=$(RACKET_LIBS)
	raco link -n COAST ~/CRESTaceans/

clean:
	find . -name "compiled" -type d -print0 | xargs -0 rm -rfv
	cd bindings/nacl/wrapper && make clean
	cd apps/coastcast/bindings/vp8/wrapper && make clean

clean-junk:
	find . -name *~ -print0 | xargs -0 rm

check-env:
ifndef RACKET_LIBS
	make export-env-vars
else

ifneq (RACKET_LIBS,"$(HOME)/racket/lib")
	make export-env-vars
endif

endif

export-env-vars:
	if ! grep "RACKET_LIBS" $(HOME)/.bashrc; then echo "export PATH=$(HOME)/racket/bin:$(PATH)\nexport RACKET_LIBS=$(HOME)/racket/lib\nexport LD_LIBRARY_PATH=$(HOME)/racket/lib" >> $(HOME)/.bashrc; fi

setup: 
	make check-env
	make packages
	@/bin/bash -c "pushd .;\
	cd libs; \
	./install-fastlz.sh;\
	./install-libsodium.sh;\
	./install-nacl.sh;\
	find `pwd` -path '*build/*/include*' -name '*.h' -type f -print0 | xargs -0 cp -t '../bindings/nacl/wrapper';\
	find `pwd` -path '*build/*/lib*' -name '*.[o|a]' -type f -print0 | xargs -0 cp -t '../bindings/nacl/wrapper';\
	popd;\
	make;\
	make install;"

packages:
#	necessary packages for running COASTcast
	if ! dpkg -l | grep libswscale-dev -c >>/dev/null; then sudo apt-get install libswscale-dev; fi
	if ! dpkg -l | grep libvpx-dev -c >>/dev/null; then sudo apt-get install libvpx-dev; fi

.PHONY: export-env-vars apps peer bindings c install clean clean-junk setup check-env
