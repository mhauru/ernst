lattice : lattice.hs
	ghc -threaded --make lattice.hs

optimised : lattice.hs
	ghc -threaded -O2 --make lattice.hs

tar : lattice lattice.glade
	tar -czf lattice.tar.gz lattice lattice.glade

clean :
	rm lattice.tar.gz lattice lattice.o	
