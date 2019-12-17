.SUFFIXES:
.PHONY: clean

computer: computer.hs
	ghc -O -dynamic -o computer computer.hs

clean:
	rm -rf *.hi *.o computer
