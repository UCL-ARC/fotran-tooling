FFLAGS = -ffree-line-length-none
FC = gfortran

.PHONY: build clean 

build:
	$(FC) $(FFLAGS) -c poisson.f90 main.f90
	$(FC) $(FFLAGS) poisson.o main.o -o poisson
	$(FC) $(FFLAGS) mesh_generator.f90 -o mesh_generator

clean:
	$(RM) poisson mesh_generator
