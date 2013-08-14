ernst : ernst.hs
	ghc -threaded --make ernst.hs

optimised : ernst.hs
	ghc -threaded -O2 --make ernst.hs

tar : ernst ernst.glade
	tar -czf ernst.tar.gz ernst ernst.glade

clean :
	rm ernst.tar.gz ernst ernst.o	
