# This file sets up CTest unit tests for fv3atm.
#
# Alex Richert, Jan 2024

function(add_fv3atm_mpi_test TESTNAME)
  add_executable(${TESTNAME} ${TESTNAME}.F90)
  target_link_libraries(${TESTNAME} PRIVATE fv3atm MPI::MPI_Fortran PIO::PIO_Fortran)
  add_test(${TESTNAME} ${MPIEXEC_EXECUTABLE} -n 2 ${CMAKE_CURRENT_BINARY_DIR}/${TESTNAME})
endfunction()

function(add_fv3atm_serial_test TESTNAME)
  add_executable(${TESTNAME} ${TESTNAME}.F90)
  target_link_libraries(${TESTNAME} PRIVATE fv3atm MPI::MPI_Fortran PIO::PIO_Fortran)
  add_test(${TESTNAME} ${CMAKE_CURRENT_BINARY_DIR}/${TESTNAME})
endfunction()

#foreach(testname test_write_netcdf)
#  add_fv3atm_mpi_test(${testname})
#endforeach()

foreach(testname test_fv3_io_def)
  add_fv3atm_serial_test(${testname})
endforeach()
