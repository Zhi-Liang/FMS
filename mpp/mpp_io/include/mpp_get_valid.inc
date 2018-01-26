!Based on presence/absence of attributes, defines valid range or missing
!value. For details, see section 8.1 of NetCDF User Guide.
subroutine mpp_get_valid(f, &
                         v)
    implicit none

    type(fieldtype),intent(in) :: f
    type(validtype),intent(out) :: v

    integer(kind=c_int) :: irange
    integer(kind=c_int) :: imin
    integer(kind=c_int) :: imax
    integer(kind=c_int) :: ifill
    integer(kind=c_int) :: imissing
    integer(kind=c_int) :: iscale
    integer(kind=c_int) :: valid_T
    integer(kind=c_int) :: scale_T

    v%is_range = .true.
    v%min = -HUGE(v%min)
    v%max = HUGE(v%max)
    if (f%natt .eq. 0) then
        return
    endif

    !Find indices of relevant attributes
    irange = mpp_find_att(f%att, &
                          "valid_range")
    imin = mpp_find_att(f%att, &
                        "valid_min")
    imax = mpp_find_att(f%att, &
                        "valid_max")
    ifill = mpp_find_att(f%att, &
                         "_FillValue")
    imissing = mpp_find_att(f%att, &
                            "missing_value")

    !Find the widest type of scale and offset; note that the code
    !uses assumption that NetCDF types are arranged in th order of rank,
    !that is MPP_IO_C_NC_BYTE < MPP_IO_C_NC_SHORT < MPP_IO_C_NC_INT <
    !MPP_IO_C_NC_FLOAT < MPP_IO_C_NC_DOUBLE.
    scale_T = 0
    iscale = mpp_find_att(f%att, &
                          "scale_factor")
    if (iscale .gt. 0) then
        scale_T = f%att(iscale)%type
    endif
    iscale = mpp_find_att(f%att, &
                          "add_offset")
    if (iscale .gt. 0) then
        scale_T = max(scale_T, &
                      f%att(iscale)%type)
    endif

    !Examine possible range attributes
    valid_T = 0
    if (irange .gt. 0) then
        v%min = f%att(irange)%fatt(1)
        v%max = f%att(irange)%fatt(2)
        valid_T = f%att(irange)%type
    elseif (imax .gt. 0 .or. imin .gt. 0) then
        if (imax .gt. 0) then
            v%max = f%att(imax)%fatt(1)
            valid_T = max(valid_T, &
                          f%att(imax)%type)
        endif
        if (imin .gt. 0) then
            v%min = f%att(imin)%fatt(1)
            valid_T = max(valid_T, &
                          f%att(imin)%type)
        endif
    elseif (imissing .gt. 0) then
        v%is_range = .false.
        !Here we always scale, since missing_value is supposed to be in
        !external representation.
        v%min = f%att(imissing)%fatt(1)*f%scale + f%add
    elseif (ifill>0) then
        !z1l ifdef is added in to be able to compile without using use_netCDF.
#ifdef use_netCDF
        !Define min and max according to _FillValue
        if (f%att(ifill)%fatt(1) .gt. 0) then
            ! if _FillValue is positive, then it defines valid maximum
            v%max = f%att(ifill)%fatt(1)
            select case(f%type)
                case (MPP_IO_C_NC_BYTE,MPP_IO_C_NC_SHORT,MPP_IO_C_NC_INT)
                    v%max = v%max-1
                case (MPP_IO_C_NC_FLOAT)
                    v%max = nearest(nearest(real(v%max,4),-1.0),-1.0)
                case (MPP_IO_C_NC_DOUBLE)
                    v%max = nearest(nearest(real(v%max,8),-1.0),-1.0)
            endselect
            !Always do the scaling, as the _FillValue is in external
            !representation.
            v%max = v%max*f%scale + f%add
        else
            !If _FillValue is negative or zero, then it defines valid minimum
            v%min = f%att(ifill)%fatt(1)
            select case(f%type)
                case (MPP_IO_C_NC_BYTE,MPP_IO_C_NC_SHORT,MPP_IO_C_NC_INT)
                    v%min = v%min+1
                case (MPP_IO_C_NC_FLOAT)
                    v%min = nearest(nearest(real(v%min,4),+1.0),+1.0)
                case (MPP_IO_C_NC_DOUBLE)
                    v%min = nearest(nearest(real(v%min,8),+1.0),+1.0)
            endselect
            !Always do the scaling, as the _FillValue is in external
            !representation.
            v%min = v%min*f%scale + f%add
        endif
#endif
    endif
    !If valid_range is the same type as scale_factor (actually the wider of
    !scale_factor and add_offset) and this is wider than the external data, then it
    !will be interpreted as being in the units of the internal (unpacked) data.
    !Otherwise it is in the units of the external (packed) data.
    !Note that it is not relevant if we went through the missing_data of _FillValue
    !brances, because in this case all irange, imin, and imax are less then 0
    if (.not. ((valid_T .eq. scale_T) .and. (scale_T .gt. f%type))) then
        if (irange .gt. 0 .or. imin .gt. 0) then
            v%min = v%min*f%scale + f%add
        endif
        if (irange .gt. 0 .or. imax .gt. 0) then
            v%max = v%max*f%scale + f%add
        endif
    endif

end subroutine mpp_get_valid
