all : rainbow.cabal tests/rainbow-tests.cabal

rainbow.cabal : versions.m4 rainbow.cabal.m4
	m4 versions.m4 rainbow.cabal.m4 > rainbow.cabal

tests/rainbow-tests.cabal : versions.m4 tests/rainbow-tests.cabal.m4
	m4 versions.m4 tests/rainbow-tests.cabal.m4 > tests/rainbow-tests.cabal

tarball : rainbow.cabal tests/rainbow-tests.cabal
	runghc sunlight-test.hs
	cabal sdist

clean :
	rm -f rainbow.cabal tests/rainbow-tests.cabal

.PHONY: all tarball clean
