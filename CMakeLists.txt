cmake_minimum_required(VERSION 3.9 FATAL_ERROR)

# Set project name
project(
  "fortran-tooling"
  LANGUAGES "Fortran"
  VERSION "0.0.1"
  DESCRIPTION "Fortran tooling"
)

set(CMAKE_Fortran_FLAGS "-ffree-line-length-none")

# Define src files
set(PROJ_SRC_DIR "${PROJECT_SOURCE_DIR}/src")
file(GLOB PROJ_MESH_GENERATOR_SOURCES "${PROJ_SRC_DIR}/mesh_generator/*.f90")
file(GLOB PROJ_POISSON_SOURCES "${PROJ_SRC_DIR}/poisson/*.f90")

# Build src executables
add_executable("${PROJECT_NAME}-mesh-generator" "${PROJ_MESH_GENERATOR_SOURCES}")
add_executable("${PROJECT_NAME}-poisson" "${PROJ_POISSON_SOURCES}")
