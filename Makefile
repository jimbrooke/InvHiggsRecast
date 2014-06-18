decaylhe: decaylhe.o main.o writeevent.o pythia.o
	g++ -o decaylhe decaylhe.o main.o writeevent.o pythia.o -L$(EXTERNAL)/lib -lHepMC -lHepMCfio -lgfortran

pythia.o: pythia-6.4.28.f
	gfortran -c pythia-6.4.28.f

decaylhe.o: decaylhe.f
	gfortran -c decaylhe.f

main.o: main.cc
	g++ -c main.cc

writeevent.o: writeevent.cc
	g++ -c writeevent.cc -I$(EXTERNAL)/include
