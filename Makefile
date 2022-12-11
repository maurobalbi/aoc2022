SHELL := /bin/bash

FLAGS = -fPIC -O2 -g
HFLAGS = $(FLAGS) -threaded -rtsopts -v0
DONE1 = $(wildcard [1-9][ab].hs)
DONE2 = $(wildcard [12][0-9][ab].hs)

all: $(sort $(DONE1:%.hs=%.output)) $(sort $(DONE2:%.hs=%.output))
	@for output in $^; do /bin/echo -n "$${output/.output}: "; cat "$$output"; done

clean:
	@rm -f -- [0-9][0-9][ab] [0-9][ab] *.o *.hi *.so *.a

distclean:
	@rm -f -- *.output

%a.output: %a.hs %.input
	@runhaskell $< < $*.input > $@

%b.output: %b.hs %.input
	@runhaskell $< < $*.input > $@

.PRECIOUS: %
.PHONY: all clean distclean