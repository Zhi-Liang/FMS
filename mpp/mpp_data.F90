!***********************************************************************
!*                   GNU Lesser General Public License
!*
!* This file is part of the GFDL Flexible Modeling System (FMS).
!*
!* FMS is free software: you can redistribute it and/or modify it under
!* the terms of the GNU Lesser General Public License as published by
!* the Free Software Foundation, either version 3 of the License, or (at
!* your option) any later version.
!*
!* FMS is distributed in the hope that it will be useful, but WITHOUT
!* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
!* FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
!* for more details.
!*
!* You should have received a copy of the GNU Lesser General Public
!* License along with FMS.  If not, see <http://www.gnu.org/licenses/>.
!***********************************************************************
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
