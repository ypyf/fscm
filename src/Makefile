# Makefile for build FSCM
# Written by Yan Feng

# reduce .exe file size
#http://stackoverflow.com/questions/6115459/small-haskell-program-compiled-with-ghc-into-huge-binary?lq=1

# +RTS option: -prof -auto-all
SRC = Main.hs Scheme/Interp.hs Scheme/Eval.hs Scheme/Types.hs Scheme/Primitives.hs Scheme/Variables.hs Scheme/Scan.hs Scheme/Parser.hs
EXE = fscm.exe

all: fscm

fscm: $(SRC)
	ghc --make Main.hs -fwarn-missing-signatures -optc-Os -O2 -o $(EXE)
	strip -p --strip-unneeded --remove-section=.comment -o $(EXE) $(EXE)

Scheme/Scan.hs: Scheme/Scan.x
	alex Scheme/Scan.x

Scheme/Parser.hs: Scheme/Parser.y
	happy --strict Scheme/Parser.y

install:
	cp ./fscm.exe ../bin
	cp ./stdlib.scm ../bin

clean:
	rm -f $(EXE)
	rm -f *.o *.hi
	rm -f Scheme/*.o Scheme/*.hi
	rm -f Scheme/Scan.hs Scheme/Parser.hs
