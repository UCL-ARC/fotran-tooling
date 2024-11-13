# Install the test-drive dependency
set(test_lib "test-drive")
set(test_pkg "TEST_DRIVE")
set(test_url "https://github.com/fortran-lang/test-drive")

message(STATUS "Retrieving ${test_lib} from ${test_url}")
include(FetchContent)
FetchContent_Declare(
    "${test_lib}"
    GIT_REPOSITORY "${test_url}"
    GIT_TAG "HEAD"
    )
FetchContent_MakeAvailable("${test_lib}")

add_library("${test_lib}::${test_lib}" INTERFACE IMPORTED)
target_link_libraries("${test_lib}::${test_lib}" INTERFACE "${test_lib}")

# We need the module directory in the subproject before we finish the configure stage
FetchContent_GetProperties("${test_lib}" SOURCE_DIR "${test_pkg}_SOURCE_DIR")
FetchContent_GetProperties("${test_lib}" BINARY_DIR "${test_pkg}_BINARY_DIR")
if(NOT EXISTS "${${test_pkg}_BINARY_DIR}/include")
    make_directory("${${test_pkg}_BINARY_DIR}/include")
endif()

# Define test files
set(PROJ_TEST_DRIVE_DIR "${PROJECT_SOURCE_DIR}/testing/test-drive")
file(GLOB PROJ_TEST_DRIVE_SOURCES "${PROJ_TEST_DRIVE_DIR}/tests/*.f90")

# Filter out the main.f90 files. We can only have one main() function in our tests
set(PROJ_POISSON_SOURCES_EXC_MAIN ${PROJ_POISSON_SOURCES})
set(PROJ_MESH_GENERATOR_SOURCES_EXC_MAIN ${PROJ_MESH_GENERATOR_SOURCES})
list(FILTER PROJ_POISSON_SOURCES_EXC_MAIN EXCLUDE REGEX ".*main.f90")
list(FILTER PROJ_MESH_GENERATOR_SOURCES_EXC_MAIN EXCLUDE REGEX ".*main.f90")

# Unit testing
set(
  tests
  "mesh_generator"
)
set(
  test-srcs
  "main.f90"
)
foreach(t IN LISTS tests)
  string(MAKE_C_IDENTIFIER ${t} t) 
  list(APPEND test-srcs "${PROJ_TEST_DRIVE_DIR}/test_${t}.f90")
endforeach()

add_executable(
  "test_${PROJECT_NAME}-test-drive"
  "${test-srcs}"
  "${PROJ_TEST_DRIVE_SOURCES}"
  "${PROJ_POISSON_SOURCES_EXC_MAIN}"
  "${PROJ_MESH_GENERATOR_SOURCES_EXC_MAIN}"
)
target_link_libraries(
  "test_${PROJECT_NAME}-test-drive"
  PRIVATE
  "test-drive::test-drive"
)

foreach(t IN LISTS tests)
  add_test("${PROJECT_NAME}-test-drive/${t}" "test_${PROJECT_NAME}-test-drive" "${t}")
endforeach()
