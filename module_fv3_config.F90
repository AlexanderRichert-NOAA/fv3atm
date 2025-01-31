
  module module_fv3_config
!------------------------------------------------------------------------
!
!*** fv3 configure variables from model_configure
!
! revision history
! 01/2017   Jun Wang    Initial code
!
!------------------------------------------------------------------------
!
  use esmf

  implicit none
!
  integer                  :: dt_atmos
  integer                  :: first_kdt
  integer                  :: fcst_mpi_comm, fcst_ntasks
!
  integer                  :: cpl_grid_id
  logical                  :: cplprint_flag
  logical                  :: quilting, quilting_restart
!
  real,dimension(:),allocatable                   :: output_fh
  character(esmf_maxstr),dimension(:),allocatable :: filename_base
  character(17)            :: calendar='                 '
!
  end module module_fv3_config
