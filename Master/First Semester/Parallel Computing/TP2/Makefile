CPP1 = g++ -Wall -Ofast -funroll-all-loops -march=native -flto
CPP2 = g++ -Wall  -fopenmp -Ofast -funroll-all-loops -march=native -flto 
SRCS = src/main.cpp src/fluid_solver.cpp src/EventManager.cpp

all: phase1 phase2

phase1:
	$(CPP1) $(SRCS) -o fluid_sim_seq

phase2:
	$(CPP2) $(SRCS) -o fluid_sim

clean:
	echo Cleaning up...
	@rm fluid_sim fluid_sim_seq
	@echo Done.

runseq:
	./fluid_sim_seq

runpar:
	./fluid_sim
