build:
	cabal build

test: dist/build/spec/spec
	./dist/build/spec/spec -j 1

dist/build/spec/spec:
	cabal configure --enable-tests
	cabal build spec

.PHONY: build dist/build/spec/spec
