function mpp_global_max_2d( domain, field, locus, position )
  type(domain2D),     intent(in) :: domain
  class(*), intent(in)           :: field(:,:)
  integer, intent(out), optional :: locus(2)
  integer, intent(in),  optional :: position
  real(DOUBLE_KIND)              :: mpp_global_max_2d
  integer                        :: locus3D(3)
  class(*), pointer :: field3D(:,:,:) => NULL()
  integer           :: isize, jsize, ksize

  if(.not. is_contiguous(field)) call mpp_error(FATAL, "mpp_global_max_2d: field is not contiguous")
  isize=size(field,1); jsize=size(field,2); ksize=1
  call point_3D_to_2D(isize, jsize, field, field3D)
  if( PRESENT(locus) )then
     mpp_global_max_2d = mpp_global_max_3d( domain, field3D, locus3D, position )
     locus = locus3D(1:2)
  else
     mpp_global_max_2d = mpp_global_max_3d( domain, field3D, position = position )
  end if

  return
end function mpp_global_max_2d

function mpp_global_max_3d( domain, field, locus, position )
  type(domain2D),     intent(in) :: domain
  class(*),           intent(in) :: field(:,:,:)
  integer, intent(out), optional :: locus(3)
  integer, intent(in),  optional :: position
  real(DOUBLE_KIND)              :: mpp_global_max_3d
  integer, save :: l_locus(3)
  integer, save :: here               ! need storage class w/ global address
  integer :: isc, iec, jsc, jec, ioff, joff, nxc, nyc, nxd, nyd
  REAL(DOUBLE_KIND)      :: local
  REAL(DOUBLE_KIND),save :: g_val  ! need storage class w/ global address; not sure whether fn result has required class

  if( .NOT.module_is_initialized )call mpp_error( FATAL, &
          'do_global_max_3d: You must first call mpp_domains_init.' )
  call mpp_get_compute_domain(domain, xsize=nxc, ysize=nyc, position=position )
  call mpp_get_memory_domain(domain, xsize=nxd, ysize=nyd, position=position )
  if( size(field,1) .EQ. nxc  .AND. size(field,2) .EQ. nyc )then
     !field is on compute domain
     ioff = 0
     joff = 0
  else if( size(field,1) .EQ. nxd .AND. size(field,2) .EQ. nyd )then
     !field is on data domain
     ioff = domain%x(1)%compute%begin - domain%x(1)%data%begin
     joff = domain%y(1)%compute%begin - domain%y(1)%data%begin
  else
     call mpp_error( FATAL, 'do_global_max_3d: incoming field array must match either compute domain or data domain.')
  end if
  
  select type(field)
  type is(real(DOUBLE_KIND))
     local = maxval(field(1+ioff: nxc+ioff, 1+joff: nyc+joff,:))
  type is(real(FLOAT_KIND))
     local = maxval(field(1+ioff: nxc+ioff, 1+joff: nyc+joff,:))
  class default
     call mpp_error(FATAL, "mpp_global_max_3d: only support 4-byte and 8-byte real, contact developer")
  end select
  !find the global
  g_val = local
  call mpp_max( g_val, domain%list(:)%pe )
 !find locus of the global max
  if( present(locus) )then
     !which PE is it on? min of all the PEs that have it
     here = mpp_root_pe()+mpp_npes()+1 !z1l: need to add mpp_root_pe for concurrent run.
     if( g_val == local )here = pe
     call mpp_min( here, domain%list(:)%pe )
     !find the locus here
     if( pe.EQ.here ) then
     select type(field)
     type is(real(DOUBLE_KIND))
        l_locus = maxloc(field(isc-ioff: iec-ioff, jsc-joff: jec-joff,:))
     type is(real(FLOAT_KIND))
        l_locus = maxloc(field(isc-ioff: iec-ioff, jsc-joff: jec-joff,:))
     end select
     endif
     l_locus(1) = l_locus(1) + ioff
     l_locus(2) = l_locus(2) + joff
     call mpp_broadcast( l_locus, 3, here, domain%list(:)%pe )
     locus = l_locus
  end if
  mpp_global_max_3d = g_val

  return
end function mpp_global_max_3d

function mpp_global_max_4d( domain, field, locus, position )
  type(domain2D),     intent(in) :: domain
  class(*),           intent(in) :: field(:,:,:,:)
  integer, intent(out), optional :: locus(4)
  integer, intent(in),  optional :: position
  real(DOUBLE_KIND)              :: mpp_global_max_4d
  integer                        :: locus3D(3)
  class(*), pointer :: field3D(:,:,:) => NULL()
  integer           :: isize, jsize, ksize

  if(.not. is_contiguous(field)) call mpp_error(FATAL, "mpp_global_max_4d: field is not contiguous")
  isize=size(field,1); jsize=size(field,2); ksize=size(field,3)*size(field,4)
  call point_3D_to_4D(isize, jsize, size(field,3), size(field,4), field, field3D)
  if( PRESENT(locus) )then
     mpp_global_max_4d = mpp_global_max_3d( domain, field3D, locus3D, position )
     locus(1:2) = locus3D(1:2)
     locus(3) = modulo(locus3D(3),size(field,3))
     locus(4) = (locus3D(3)-locus(3))/size(field,3) + 1
     if( locus(3).EQ.0 )then
        locus(3) = size(field,3)
        locus(4) = locus(4) - 1
     end if
  else
     mpp_global_max_4d = mpp_global_max_3d( domain, field3D, position = position  )
  end if
  return

end function mpp_global_max_4d

function mpp_global_max_5d( domain, field, locus, position )
  type(domain2D),     intent(in) :: domain
  class(*), intent(in)           :: field(:,:,:,:,:)
  integer, intent(out), optional :: locus(5)
  integer, intent(in),  optional :: position
  real(DOUBLE_KIND)              :: mpp_global_max_5d
  integer                        :: locus3D(3)
  class(*), pointer :: field3D(:,:,:) => NULL()
  integer           :: isize, jsize, ksize

  if(.not. is_contiguous(field)) call mpp_error(FATAL, "mpp_global_max_5d: field is not contiguous")
  isize=size(field,1); jsize=size(field,2); ksize=size(field,3)*size(field,4)*size(field,5)
  call point_3D_to_5D(isize, jsize, size(field,3), size(field,4), size(field,5), field, field3D)
  if( PRESENT(locus) )then
     mpp_global_max_5d = mpp_global_max_3d( domain, field3D, locus3D, position )
     locus(1:2) = locus3D(1:2)
     locus(3) = modulo(locus3D(3),size(field,3))
     locus(4) = modulo(locus3D(3),size(field,3)*size(field,4))
     locus(5) = (locus3D(3)-locus(4))/size(field,3)/size(field,4) + 1
     if( locus(3).EQ.0 )then
        locus(3) = size(field,3)
        locus(4) = locus(4) - 1
     end if
  else
     mpp_global_max_5d = mpp_global_max_3d( domain, field3D, position = position  )
  end if
  return

end function mpp_global_max_5d

function mpp_global_min_2d( domain, field, locus, position )
  type(domain2D),     intent(in) :: domain
  class(*), intent(in)           :: field(:,:)
  integer, intent(out), optional :: locus(2)
  integer, intent(in),  optional :: position
  real(DOUBLE_KIND)              :: mpp_global_min_2d
  integer                        :: locus3D(3)
  class(*), pointer :: field3D(:,:,:) => NULL()
  integer           :: isize, jsize, ksize

  if(.not. is_contiguous(field)) call mpp_error(FATAL, "mpp_global_min_2d: field is not contiguous")
  isize=size(field,1); jsize=size(field,2); ksize=1
  call point_3D_to_2D(isize, jsize, field, field3D)
  if( PRESENT(locus) )then
     mpp_global_min_2d = mpp_global_min_3d( domain, field3D, locus3D, position )
     locus = locus3D(1:2)
  else
     mpp_global_min_2d = mpp_global_min_3d( domain, field3D, position = position )
  end if

  return
end function mpp_global_min_2d

function mpp_global_min_3d( domain, field, locus, position )
  type(domain2D),     intent(in) :: domain
  class(*),           intent(in) :: field(:,:,:)
  integer, intent(out), optional :: locus(3)
  integer, intent(in),  optional :: position
  real(DOUBLE_KIND)              :: mpp_global_min_3d
  integer, save :: l_locus(3)
  integer, save :: here               ! need storage class w/ global address
  integer :: isc, iec, jsc, jec, ioff, joff, nxc, nyc, nxd, nyd
  REAL(DOUBLE_KIND)      :: local
  REAL(DOUBLE_KIND),save :: g_val  ! need storage class w/ global address; not sure whether fn result has required class

  if( .NOT.module_is_initialized )call mpp_error( FATAL, &
          'do_global_min_3d: You must first call mpp_domains_init.' )
  call mpp_get_compute_domain(domain, xsize=nxc, ysize=nyc, position=position )
  call mpp_get_memory_domain(domain, xsize=nxd, ysize=nyd, position=position )
  if( size(field,1) .EQ. nxc  .AND. size(field,2) .EQ. nyc )then
     !field is on compute domain
     ioff = 0
     joff = 0
  else if( size(field,1) .EQ. nxd .AND. size(field,2) .EQ. nyd )then
     !field is on data domain
     ioff = domain%x(1)%compute%begin - domain%x(1)%data%begin
     joff = domain%y(1)%compute%begin - domain%y(1)%data%begin
  else
     call mpp_error( FATAL, 'do_global_min_3d: incoming field array must match either compute domain or data domain.')
  end if
  
  select type(field)
  type is(real(DOUBLE_KIND))
     local = minval(field(1+ioff: nxc+ioff, 1+joff: nyc+joff,:))
  type is(real(FLOAT_KIND))
     local = minval(field(1+ioff: nxc+ioff, 1+joff: nyc+joff,:))
  class default
     call mpp_error(FATAL, "mpp_global_min_3d: only support 4-byte and 8-byte real, contact developer")
  end select
  !find the global
  g_val = local
  call mpp_min( g_val, domain%list(:)%pe )
 !find locus of the global max
  if( present(locus) )then
     !which PE is it on? min of all the PEs that have it
     here = mpp_root_pe()+mpp_npes()+1 !z1l: need to add mpp_root_pe for concurrent run.
     if( g_val == local )here = pe
     call mpp_min( here, domain%list(:)%pe )
     !find the locus here
     if( pe.EQ.here ) then
     select type(field)
     type is(real(DOUBLE_KIND))
        l_locus = minloc(field(isc-ioff: iec-ioff, jsc-joff: jec-joff,:))
     type is(real(FLOAT_KIND))
        l_locus = minloc(field(isc-ioff: iec-ioff, jsc-joff: jec-joff,:))
     end select
     endif
     l_locus(1) = l_locus(1) + ioff
     l_locus(2) = l_locus(2) + joff
     call mpp_broadcast( l_locus, 3, here, domain%list(:)%pe )
     locus = l_locus
  end if
  mpp_global_min_3d = g_val

  return
end function mpp_global_min_3d

function mpp_global_min_4d( domain, field, locus, position )
  type(domain2D),     intent(in) :: domain
  class(*), intent(in)           :: field(:,:,:,:)
  integer, intent(out), optional :: locus(4)
  integer, intent(in),  optional :: position
  real(DOUBLE_KIND)              :: mpp_global_min_4d
  integer                        :: locus3D(3)
  class(*), pointer :: field3D(:,:,:) => NULL()
  integer           :: isize, jsize, ksize

  if(.not. is_contiguous(field)) call mpp_error(FATAL, "mpp_global_min_4d: field is not contiguous")
  isize=size(field,1); jsize=size(field,2); ksize=size(field,3)*size(field,4)
  call point_3D_to_4D(isize, jsize, size(field,3), size(field,4), field, field3D)
  if( PRESENT(locus) )then
     mpp_global_min_4d = mpp_global_min_3d( domain, field3D, locus3D, position )
     locus(1:2) = locus3D(1:2)
     locus(3) = modulo(locus3D(3),size(field,3))
     locus(4) = (locus3D(3)-locus(3))/size(field,3) + 1
     if( locus(3).EQ.0 )then
        locus(3) = size(field,3)
        locus(4) = locus(4) - 1
     end if
  else
     mpp_global_min_4d = mpp_global_min_3d( domain, field3D, position = position  )
  end if
  return

end function mpp_global_min_4d

function mpp_global_min_5d( domain, field, locus, position )
  type(domain2D),     intent(in) :: domain
  class(*), intent(in)           :: field(:,:,:,:,:)
  integer, intent(out), optional :: locus(5)
  integer, intent(in),  optional :: position
  real(DOUBLE_KIND)              :: mpp_global_min_5d
  integer                        :: locus3D(3)
  class(*), pointer :: field3D(:,:,:) => NULL()
  integer           :: isize, jsize, ksize

  if(.not. is_contiguous(field)) call mpp_error(FATAL, "mpp_global_min_5d: field is not contiguous")
  isize=size(field,1); jsize=size(field,2); ksize=size(field,3)*size(field,4)*size(field,5)
  call point_3D_to_5D(isize, jsize, size(field,3), size(field,4), size(field,5), field, field3D)
  if( PRESENT(locus) )then
     mpp_global_min_5d = mpp_global_min_3d( domain, field3D, locus3D, position )
     locus(1:2) = locus3D(1:2)
     locus(3) = modulo(locus3D(3),size(field,3))
     locus(4) = modulo(locus3D(3),size(field,3)*size(field,4))
     locus(5) = (locus3D(3)-locus(4))/size(field,3)/size(field,4) + 1
     if( locus(3).EQ.0 )then
        locus(3) = size(field,3)
        locus(4) = locus(4) - 1
     end if
  else
     mpp_global_min_5d = mpp_global_min_3d( domain, field3D, position = position  )
  end if
  return

end function mpp_global_min_5d

function mpp_global_sum_2d(domain, field, flags, position, tile_count, overflow_check)
  type(domain2D),    intent(in) :: domain
  class(*), intent(in)          :: field(:,:)
  integer, intent(in), optional :: flags
  integer, intent(in), optional :: position
  integer, intent(in), optional :: tile_count
  logical, intent(in), optional :: overflow_check
  real(DOUBLE_KIND)             :: mpp_global_sum_2d
  class(*), pointer :: field3D(:,:,:)=>NULL()
  integer     :: isize, jsize, ksize

  if(.not. is_contiguous(field)) call mpp_error(FATAL, "mpp_global_sum_2d: field is not contiguous")
  isize=size(field,1); jsize=size(field,2); ksize=1
  call point_3D_to_2D(isize, jsize, field, field3D)
  mpp_global_sum_2d = mpp_global_sum_3d(domain, field3D, flags, position, tile_count, overflow_check)

end function mpp_global_sum_2d

function mpp_global_sum_3d(domain, field, flags, position, tile_count, overflow_check)
  type(domain2D),    intent(in) :: domain
  class(*), intent(in)          :: field(:,:,:)
  integer, intent(in), optional :: flags
  integer, intent(in), optional :: position
  integer, intent(in), optional :: tile_count
  logical, intent(in), optional :: overflow_check
  real(DOUBLE_KIND)             :: mpp_global_sum_3d
  integer :: ioff,joff, ishift, jshift
  integer :: global_flag, tile, ntile, nlist

  if( domain%max_ntile_pe > MAX_TILES ) call mpp_error(FATAL, "mpp_global_sum_core: number of tiles is exceed MAX_TILES")
  ntile     = size(domain%x(:))
  nlist     = size(domain%list(:))
  tile = 1
  if(present(tile_count)) tile = tile_count
  global_flag = NON_BITWISE_EXACT_SUM
  if(present(flags)) global_flag = flags

  call mpp_get_domain_shift(domain, ishift, jshift, position)

  if(size(field,1).EQ.domain%x(tile)%compute%size+ishift .AND. size(field,2).EQ.domain%y(tile)%compute%size+jshift )then
     !field is on compute domain
     ioff = -domain%x(tile)%compute%begin + 1
     joff = -domain%y(tile)%compute%begin + 1
  else if(size(field,1).EQ.domain%x(tile)%memory%size+ishift .AND. size(field,2).EQ.domain%y(tile)%memory%size+jshift )then
     !field is on data domain
     ioff = -domain%x(tile)%data%begin + 1
     joff = -domain%y(tile)%data%begin + 1
  else
     call mpp_error( FATAL, 'mpp_global_sum_core: incoming field array must match either compute domain or data domain.' )
  end if

  select type(field)
  type is(real(DOUBLE_KIND))
     mpp_global_sum_3d = do_global_sum_r8(domain, field, global_flag, tile, ioff, joff, position, overflow_check)
  type is(real(FLOAT_KIND))
     mpp_global_sum_3d = do_global_sum_r4(domain, field, global_flag, tile, ioff, joff, position, overflow_check)
  class default
     call mpp_error(FATAL, "mpp_global_sum_core: only support 4-byte and 8-byte real, contact developer")
  end select

end function mpp_global_sum_3d

function mpp_global_sum_4d(domain, field, flags, position, tile_count, overflow_check)
  type(domain2D),    intent(in) :: domain
  class(*), intent(in)          :: field(:,:,:,:)
  integer, intent(in), optional :: flags
  integer, intent(in), optional :: position
  integer, intent(in), optional :: tile_count
  logical, intent(in), optional :: overflow_check
  real(DOUBLE_KIND)             :: mpp_global_sum_4d
  class(*), pointer :: field3D(:,:,:)=>NULL()
  integer     :: isize, jsize, ksize

  if(.not. is_contiguous(field)) call mpp_error(FATAL, "mpp_global_sum_4d: field is not contiguous")
  isize=size(field,1); jsize=size(field,2); ksize=size(field,3)*size(field,4)
  call point_3D_to_4D(isize, jsize, size(field,3), size(field,4), field, field3D)
  mpp_global_sum_4d = mpp_global_sum_3d(domain, field3D, flags, position, tile_count, overflow_check)

end function mpp_global_sum_4d

function mpp_global_sum_5d(domain, field, flags, position, tile_count, overflow_check)
  type(domain2D),    intent(in) :: domain
  class(*), intent(in)          :: field(:,:,:,:,:)
  integer, intent(in), optional :: flags
  integer, intent(in), optional :: position
  integer, intent(in), optional :: tile_count
  logical, intent(in), optional :: overflow_check
  real(DOUBLE_KIND)             :: mpp_global_sum_5d
  class(*), pointer :: field3D(:,:,:)=>NULL()
  integer     :: isize, jsize, ksize

  if(.not. is_contiguous(field)) call mpp_error(FATAL, "mpp_global_sum_5d: field is not contiguous")
  isize=size(field,1); jsize=size(field,2); ksize=size(field,3)*size(field,4)*size(field,5)
  call point_3D_to_5D(isize, jsize, size(field,3), size(field,4), size(field,5), field, field3D)
  mpp_global_sum_5d = mpp_global_sum_3d(domain, field3D, flags, position, tile_count, overflow_check)

end function mpp_global_sum_5d

function do_global_sum_r8(domain, field, global_flag, &
                          tile, ioff, joff, position, overflow_check) result(global_sum)
  type(domain2D),    intent(in) :: domain
  real(DOUBLE_KIND), intent(in) :: field(:,:,:)
  integer,           intent(in) :: global_flag, tile, ioff, joff
  integer, intent(in), optional :: position
  logical, intent(in), optional :: overflow_check
  real(DOUBLE_KIND)             :: global_sum
  real(DOUBLE_KIND), dimension(:,:), allocatable :: field2D, global2D
  real(DOUBLE_KIND), dimension(MAX_TILES), save  :: gsum, nbrgsum, mygsum

#include "do_global_sum.inc"

end function do_global_sum_r8

function do_global_sum_r4(domain, field, global_flag, &
                          tile, ioff, joff, position, overflow_check) result(global_sum)
  type(domain2D),    intent(in) :: domain
  real(FLOAT_KIND),  intent(in) :: field(:,:,:)
  integer,           intent(in) :: global_flag, tile, ioff, joff
  integer, intent(in), optional :: position
  logical, intent(in), optional :: overflow_check
  real(DOUBLE_KIND)             :: global_sum
  real(FLOAT_KIND), dimension(:,:), allocatable :: field2D, global2D
  real(FLOAT_KIND), dimension(MAX_TILES), save  :: gsum, nbrgsum, mygsum

#include "do_global_sum.inc"

end function do_global_sum_r4

