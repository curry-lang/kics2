########################################################################
# Makefile for ID compiler
########################################################################

# The major version number:
MAJORVERSION=0
# The minor version number:
MINORVERSION=1
# The version date:
COMPILERDATE="30/04/11"

.PHONY: all
all: idc REPL.state
	chmod -R go+rX .

# generate saved state for Curry->Haskell compiler:
idc: Installation Compile.curry
	pakcs -s Compile && mv Compile.state idc

# generate saved state for interactive compiler system:
REPL.state: Installation REPL.curry
	pakcs -s REPL

REPL: Installation REPL.curry
	bin/kics2 :l REPL :save :q

# install the complete system if the kics2 compiler is present
.PHONY: install
install: idc REPL.state
	cd cpns  && ${MAKE} # Curry Port Name Server demon
	cd tools && ${MAKE} # various tools
	chmod -R go+rX .

# generate module with basic installation information:
.PHONY: Installation
Installation:
	rm -f Installation.hs Installation.curry
	echo "-- This file is automatically generated, do not change it!" > Installation.hs
	echo "module Installation where" >> Installation.hs
	echo "" >> Installation.hs
	echo 'compilerName :: String' >> Installation.hs
	echo 'compilerName = "KiCS2 Curry -> Haskell Compiler"' >> Installation.hs
	echo "" >> Installation.hs
	echo 'installDir :: String' >> Installation.hs
	echo 'installDir = "'`pwd`'"' >> Installation.hs
	echo "" >> Installation.hs
	echo 'majorVersion :: Int' >> Installation.hs
	echo 'majorVersion = ${MAJORVERSION}' >> Installation.hs
	echo "" >> Installation.hs
	echo 'minorVersion :: Int' >> Installation.hs
	echo 'minorVersion = ${MINORVERSION}' >> Installation.hs
	echo "" >> Installation.hs
	echo 'compilerDate :: String' >> Installation.hs
	echo 'compilerDate = "'${COMPILERDATE}'"' >> Installation.hs
	echo "" >> Installation.hs
	echo 'installDate :: String' >> Installation.hs
	echo 'installDate = "'`date`'"' >> Installation.hs
	cp Installation.hs Installation.curry

# install required cabal packages

.PHONY: installhaskell
installhaskell:
	cabal install parallel
	cabal install tree-monad
	cabal install parallel-tree-search

.PHONY: clean
clean:
	bin/cleancurry -r
	rm -f idc Installation.hs Installation.curry REPL.state REPL
	rm -f *.hi *.o *.hi-boot *.o-boot
	rm -f lib/*.hi lib/*.o lib/*.nda lib/*.info lib/Curry_*.hs
	rm -f idsupply*/*.hi idsupply*/*.o
	rm -f ./examples/Curry_*.*
	cd cpns  ; ${MAKE} clean
	cd tools ; ${MAKE} clean
