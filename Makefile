########################################################################
# Makefile for ID compiler
########################################################################

# The major version number:
MAJORVERSION=0
# The minor version number:
MINORVERSION=1
# The version date:
COMPILERDATE="30/05/11"
# The Haskell installation info
INSTALLHS="./runtime/Installation.hs"
# The Curry installation info
INSTALLCURRY="./Installation.curry"

.PHONY: all
all: idc REPL.state
	chmod -R go+rX .

# generate saved state for Curry->Haskell compiler:
idc: Installation Compile.state
	cp -p Compile.state idc
	#cp -p Compile idc # for bootstrapping

Compile.state: Installation Compile.curry
	pakcs -s Compile

Compile: Installation Compile.curry
	bin/kics2 :l Compile :save :q

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
	cd www   && ${MAKE} # scripts for dynamic web pages
	chmod -R go+rX .

# generate module with basic installation information:
.PHONY: Installation
Installation:
	rm -f ${INSTALLHS} ${INSTALLCURRY}
	echo "-- This file is automatically generated, do not change it!" > ${INSTALLHS}
	echo "module Installation where" >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'compilerName :: String' >> ${INSTALLHS}
	echo 'compilerName = "KiCS2 Curry -> Haskell Compiler"' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'installDir :: String' >> ${INSTALLHS}
	echo 'installDir = "'`pwd`'"' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'majorVersion :: Int' >> ${INSTALLHS}
	echo 'majorVersion = ${MAJORVERSION}' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'minorVersion :: Int' >> ${INSTALLHS}
	echo 'minorVersion = ${MINORVERSION}' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'compilerDate :: String' >> ${INSTALLHS}
	echo 'compilerDate = "'${COMPILERDATE}'"' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'installDate :: String' >> ${INSTALLHS}
	echo 'installDate = "'`date`'"' >> ${INSTALLHS}
	cp ${INSTALLHS} ${INSTALLCURRY}

# install required cabal packages

.PHONY: installhaskell
installhaskell:
	cabal install parallel
	cabal install tree-monad
	cabal install parallel-tree-search

.PHONY: clean
clean:
	bin/cleancurry -r
	rm -f idc ${INSTALLHS} ${INSTALLCURRY} Compile.state Compile
	rm -f REPL.state REPL
	rm -f ./runtime/*.hi ./runtime/*.o ./runtime/*.hi-boot ./runtime/*.o-boot
	rm -f lib/*.hi lib/*.o lib/*.nda lib/*.info lib/Curry_*.hs
	rm -f ./runtime/idsupply*/*.hi ./runtime/idsupply*/*.o
	rm -f ./examples/Curry_*.*
	cd cpns  ; ${MAKE} clean
	cd tools ; ${MAKE} clean
	cd www   ; ${MAKE} clean
