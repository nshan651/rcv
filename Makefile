LISP := sbcl

build:
	$(LISP)	--non-interactive --load 'build.lisp'

clean:
	rm -f bin/rcv
