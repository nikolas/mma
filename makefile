.PHONY: build clean
build:
	ghc --make -package GLUT -Wall -O2 Main.hs -o mma

clean:
	rm *.hi *.o mma \#*\#
