# Binary executable
BIN := rcv
# Lisp implementation (default to sbcl).
LISP := sbcl

.PHONY: all clean install run uninstall

all: 
	$(LISP)	--non-interactive \
		--eval '(ql:quickload :rcv)' \
		--eval '(asdf:make :rcv)'

run:
	@ ./bin/$(BIN)

test:
	$(LISP)	--non-interactive --eval '(ql:quickload :rcv/tests)' 

install:
	mkdir -p $(DESTDIR)/usr/local/bin
	install -Dm755 ./bin/$(BIN) $(DESTDIR)/usr/local/bin/$(BIN)

uninstall:
	rm -f $(DESTDIR)/usr/local/bin/$(BIN)

clean: uninstall
	rm -f ./bin/$(BIN)
