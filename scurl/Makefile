all:
	find . -name *.rkt -print0 | xargs -0 raco make -v
test:
	find . -name "run-tests.rkt" | while read FILE ; do racket $$FILE ; done