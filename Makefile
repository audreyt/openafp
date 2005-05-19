validate ::
	ghc -isrc -H128m --make -static -o validate -O validate.hs

udc4skl ::
	ghc -isrc -H128m --make -static -o udc4skl -O udc4skl.hs

replace ::
	ghc -isrc -H128m --make -static -o replace -O replace.hs

afpdump ::
	ghc -isrc -H128m --make -static -o afpdump -O afpdump.hs -I/usr/local/include -I/usr/local/include/icu -L/usr/local/lib -L/usr/local/lib/icu/ -licuuc

fast ::
	ghc -isrc -O0 --make -o udc4skl udc4skl.hs

profiled ::
	ghc -isrc -O -prof -auto-all --make udc4skl.hs

test ::
	echo "1..1" && \
	ghc -O -isrc --make udc4skl.hs && \
	time ./a.out t/taishin3.afp t/output.afp && \
	afpdump.pl output.afp > x.html && \
	diff output.afp taishin3.afp && \
	echo "ok 1"

tags ::
	find src/OpenAFP -name '*.hs' | xargs hasktags -c *.hs

instance ::
	perl src/OpenAFP/Prelude/instances.pl

instances ::
	perl src/OpenAFP/Prelude/instances.pl

dist ::
	tar czf afp.tar.gz _darcs Makefile *.*hs OpenAFP/*.*hs OpenAFP/*/*.*hs OpenAFP/*/*.pl OpenAFP/*/*/*hs

clean ::
	rm -f *.hi *.o src/OpenAFP/*.hi src/OpenAFP/*.o src/OpenAFP/*/*.hi src/OpenAFP/*/*.o src/OpenAFP/*/*/*.hi src/OpenAFP/*/*/*.o

docs ::
	rm -rf docs
	mkdir docs
	cp -Rf src/OpenAFP docs/OpenAFP
	cp Main.hs docs/Main.hs
	perl -pi -e 's/IArray UArray/UArray/g' docs/OpenAFP/Internals/Binary.hs
	chdir docs && find . -name '*.*hs' | \
	xargs haddock \
	-i /usr/local/share/doc/ghc6/libraries/base,/usr/local/share/doc/ghc6/libraries/base/base.haddock \
	-i /usr/local/share/doc/ghc6/libraries/haskell98,/usr/local/share/doc/ghc6/libraries/haskell98/haskell98.haddock \
	-i /usr/local/share/doc/ghc6/libraries/haskell-src,/usr/local/share/doc/ghc6/libraries/haskell-src/haskell-src.haddock \
	-k haskell98 -k haskell-src -k base -h -t OpenAFP --gen-index && \
	rm -rf OpenAFP
