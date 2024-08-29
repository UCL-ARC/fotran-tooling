FFLAGS = -ffree-line-length-none

build:
	gfortran -c poisson.f90 main.f90
	gfortran poisson.o main.o -o poisson
	gfortran $(FFLAGS) -c poisson.f90 main.f90
	gfortran $(FFLAGS) poisson.o main.o -o poisson

	gfortran mesh_generator.f90 -o mesh_generator

clean:
	rm -f poisson