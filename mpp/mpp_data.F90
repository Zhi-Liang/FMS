module mpp_data_mod
#include <fms_platform.h>

#if defined(use_libMPI) && defined(sgi_mipspro)
  use mpi
#endif

  use mpp_parameter_mod, only : MAXPES

  implicit none
  private

! Include variable "version" to be written to log file.
#include<file_version.h>
  public version

#if defined(use_libMPI) && !defined(sgi_mipspro)
#include <mpif.h>  
!sgi_mipspro gets this from 'use mpi'
#endif

  !----------------------------------------------------------------!
  ! The following data is used in mpp_mod and its components       !
  !----------------------------------------------------------------!
  real(DOUBLE_KIND), allocatable :: mpp_stack(:)
#ifdef use_libMPI
  integer :: stat(MPI_STATUS_SIZE)
#else
  integer, parameter :: stat=-999
#endif

  !--- public data is used by mpp_mod
  public :: stat, mpp_stack

end module mpp_data_mod
