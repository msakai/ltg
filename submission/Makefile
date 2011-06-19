TARBALL=sampou.tar.gz

all:
	(cd .. && cabal clean && cabal configure && cabal build) && cp ../dist/build/run/run . && tar zcvf $(TARBALL) ./install ./run ../src

clean:
	rm -f run $(TARBALL)

