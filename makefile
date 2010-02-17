.PHONY: build clean
build:
	ghc --make -package GLUT -O2 Main.hs -o mma

clean:
	rm *.hi *.o mma
