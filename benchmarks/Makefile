ghc-prof-flags :=
ifdef ENABLE_PROFILING
	ghc-prof-flags += -prof -hisuf p_hi -osuf p_o
	lib-suffix := _p
else
	lib-suffix :=
endif

ifdef GHC
	ghc:= $(GHC)
else
	ghc := ghc
endif

package := unordered-containers
version := $(shell awk '/^version:/{print $$2}' ../$(package).cabal)
lib := ../dist/build/libHS$(package)-$(version)$(lib-suffix).a
ghc-flags := -Wall -O2 -hide-all-packages \
	-package-conf ../dist/package.conf.inplace -package base -package mtl \
	-package unordered-containers -package containers -package criterion \
	-package deepseq -package hashable -package random -package bytestring \
	$(ghc-prof-flags)

%.o: %.hs
	$(ghc) $(ghc-flags) -c -o $@ $<

programs := bench

.PHONY: all
all: $(programs)

bench: $(lib) Benchmarks.o Util/Int.o Util/ByteString.o Util/String.o
	ranlib $(lib)
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^) $(lib)

.PHONY: clean
clean:
	-find . \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;
	-rm -f $(programs)

Benchmarks.o: Util/Int.o Util/ByteString.o Util/String.o
Util/ByteString.o: Util/String.o
