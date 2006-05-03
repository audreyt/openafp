all :: dist/build/libHSOpenAFP-1.0.a binaries
    
binaries :: afp-validate afp-udcfix afp-replace afp-dump

afp-validate ::
	ghc -H128m --make -static -o afp-validate -O bin/afp-validate.hs

afp-udcfix ::
	ghc -H128m --make -static -o afp-udcfix -O bin/afp-udcfix.hs

afp-replace ::
	ghc -H128m --make -static -o afp-replace -O bin/afp-replace.hs

afp-dump ::
	ghc -H128m --make -static -o afp-dump -O bin/afp-dump.hs

dist/build/libHSOpenAFP-1.0.a ::
	runghc Setup.lhs configure --user
	runghc Setup.lhs build
	sudo runghc Setup.lhs install

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
	cp -Rf src/OpenAFP.hs docs/OpenAFP.hs
	perl -pi -e 's/IArray UArray/UArray/g' docs/OpenAFP/Internals/Binary.hs
	chdir docs && find . -name '*.*hs' | \
	xargs haddock \
	-i /usr/local/share/doc/ghc6/libraries/base,/usr/local/share/doc/ghc6/libraries/base/base.haddock \
	-i /usr/local/share/doc/ghc6/libraries/haskell98,/usr/local/share/doc/ghc6/libraries/haskell98/haskell98.haddock \
	-i /usr/local/share/doc/ghc6/libraries/haskell-src,/usr/local/share/doc/ghc6/libraries/haskell-src/haskell-src.haddock \
	-k haskell98 -k haskell-src -k base -h -t OpenAFP --gen-index && \
	rm -rf OpenAFP
