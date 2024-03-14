! This is a test of the 'write_netcdf' routine.
program test_write_netcdf

  use ESMF
  use module_write_netcdf, only: write_netcdf

  implicit none

  type(ESMF_FieldBundle)             :: wrtfb
  type(ESMF_Field), allocatable :: fcstField(:), fields(:)
  type(ESMF_VM) :: vm
  type(ESMF_Grid) :: grid, ESMF_GridCreateNoPeriDim
  type(ESMF_DistGrid) :: distgrid, ESMF_DistGridCreate
  type(ESMF_Grid) :: wrtgrid
  type(ESMF_Array) :: array, ESMF_ArrayCreate
  real(ESMF_KIND_R8)                    :: nfhour=42.0
  character(*), parameter            :: infilename="ajwr.nc"
  character(*), parameter            :: outfilename="test_write_netcdf.output.nc"
  character(7) :: fieldname
  character(len=ESMF_MAXSTR) :: varcval
  logical                            :: use_parallel_netcdf=.false.
  integer                            :: mype
  integer                            :: grid_id=0
  integer :: rc, petCount, localPet, mpi_comm, i


  allocate(fcstField(15))
  allocate(fields(15))
  call ESMF_Initialize(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, mpiCommunicator=mpi_comm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  grid = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), maxIndex=(/10,20/), name="grid")
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/10,20/), rc=rc)
  array = ESMF_ArrayCreate(typekind=ESMF_TYPEKIND_R8,distgrid=distgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridSetCoord(grid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
  call ESMF_GridSetCoord(grid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  fields(1)=ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,   &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               indexflag=ESMF_INDEX_DELOCAL, name="clwmr", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  fields(2)=ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,   &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               indexflag=ESMF_INDEX_DELOCAL, name="delz", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  fields(3)=ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,   &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               indexflag=ESMF_INDEX_DELOCAL, name="dpres", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  fields(4)=ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,   &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               indexflag=ESMF_INDEX_DELOCAL, name="dzdt", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  fields(5)=ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,   &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               indexflag=ESMF_INDEX_DELOCAL, name="grle", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  fields(6)=ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,   &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               indexflag=ESMF_INDEX_DELOCAL, name="hgtsfc", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  fields(7)=ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,   &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               indexflag=ESMF_INDEX_DELOCAL, name="icmr", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  fields(8)=ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,   &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               indexflag=ESMF_INDEX_DELOCAL, name="o3mr", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  fields(9)=ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,   &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               indexflag=ESMF_INDEX_DELOCAL, name="pressfc", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  fields(10)=ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,   &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               indexflag=ESMF_INDEX_DELOCAL, name="rwmr", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  fields(11)=ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,   &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               indexflag=ESMF_INDEX_DELOCAL, name="snmr", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  fields(12)=ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,   &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               indexflag=ESMF_INDEX_DELOCAL, name="spfh", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  fields(13)=ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,   &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               indexflag=ESMF_INDEX_DELOCAL, name="tmp", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  fields(14)=ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,   &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               indexflag=ESMF_INDEX_DELOCAL, name="ugrd", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  fields(15)=ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8,   &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               indexflag=ESMF_INDEX_DELOCAL, name="vgrd", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  wrtfb=ESMF_FieldBundleCreate(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  wrtfb = ESMF_FieldBundleCreate(fieldList=fields(1:15), &
                                name="atmosphere data", rc=rc)

  call ESMF_FieldBundleRead(wrtfb, infilename, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_FieldBundleGet(wrtfb, fieldList=fcstField, grid=wrtgrid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_AttributeAdd(wrtfb, convention="NetCDF", purpose="FV3", &
                               attrList=(/"grid  "/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_AttributeSet(wrtfb, convention="NetCDF", purpose="FV3", &
                             name="grid", value="gaussian", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_AttributeAdd(grid, convention="NetCDF", purpose="FV3", &
                             attrList=(/"time  "/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_AttributeSet(grid, convention="NetCDF", purpose="FV3", &
                             attnestflag=ESMF_ATTNEST_OFF, &
                             name="time", value=nfhour, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_AttributeAdd(grid, convention="NetCDF", purpose="FV3", &
                             attrList=(/"time_iso  "/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_AttributeSet(grid, convention="NetCDF", purpose="FV3", &
                             attnestflag=ESMF_ATTNEST_OFF, &
                             name="time_iso", value="time_str", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_AttributeAdd(wrtfb, convention="NetCDF", purpose="FV3", &
                             attrList=(/"time  "/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_AttributeSet(wrtfb, convention="NetCDF", purpose="FV3", &
                             attnestflag=ESMF_ATTNEST_OFF, &
                             name="time", value=nfhour, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call write_netcdf(wrtfb, "test_write_netcdf_output.nc", &
                          use_parallel_netcdf, mpi_comm, localPet, &
                          grid_id, rc)

end program test_write_netcdf
