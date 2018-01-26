function mpp_get_axis_id(axis) result(id)
    implicit none

    !Inputs/outputs
    type(axistype),intent(in) :: axis
    integer(kind=c_int) :: id

    call openmp_thread_trap()
    id = axis%id

end function mpp_get_axis_id

function mpp_get_field_id(field) result(id)
    implicit none

    !Inputs/outputs
    type(fieldtype),intent(in) :: field
    integer(kind=c_int) :: id

    call openmp_thread_trap()
    id = field%id

end function mpp_get_field_id

logical elemental function mpp_is_valid(x, v)
    implicit none

    !Inputs/outputs
    real(kind=c_double),intent(in) :: x
    type(validtype),intent(in) :: v

    if (v%is_range) then
        mpp_is_valid = (v%min <= x) .and. (x <= v%max)
    else
        mpp_is_valid = (x .ne. v%min)
    endif

end function mpp_is_valid

subroutine mpp_get_field_atts(field, &
                              name, &
                              units, &
                              longname, &
                              min, &
                              max, &
                              missing, &
                              ndim, &
                              siz, &
                              axes, &
                              atts, &
                              valid, &
                              scale, &
                              add, &
                              checksum)
    implicit none

    !Inputs/outputs
    type(fieldtype),intent(in) :: field
    character(len=*),intent(out),optional :: name
    character(len=*),intent(out),optional :: units
    character(len=*),intent(out),optional :: longname
    real(kind=c_double),intent(out),optional :: min
    real(kind=c_double),intent(out),optional :: max
    real(kind=c_double),intent(out),optional :: missing
    integer(kind=c_int),intent(out),optional :: ndim
    integer(kind=c_int),dimension(:),intent(out),optional :: siz
    type(axistype),dimension(:),intent(inout),optional :: axes
    type(atttype),dimension(:),intent(inout),optional :: atts
    type(validtype),intent(out),optional :: valid
    real(kind=c_double),intent(out),optional :: scale
    real(kind=c_double),intent(out),optional :: add
    integer(int64),dimension(:),intent(out),optional :: checksum

    !Local variables
    integer(kind=c_int) :: n
    integer(kind=c_int) :: m
    integer(kind=c_int) :: check_exist

    call openmp_thread_trap()
    if (present(name)) then
        name = field%name
    endif

    if (present(units)) then
        units = field%units
    endif
    if (present(longname)) then
        longname = field%longname
    endif

    if (present(min)) then
        min = field%min
    endif

    if (present(max)) then
        max = field%max
    endif

    if (present(missing)) then
        missing = field%missing
    endif

    if (present(ndim)) then
        ndim = field%ndim
    endif

    if (present(atts)) then
        atts = default_att
        n = size(atts,kind=c_int)
        m = size(field%Att,kind=c_int)
        if (n .lt. m) then
             call mpp_error(FATAL, &
                            "mpp_get_field_atts: attribute array not large" &
                                //" enough for field "//trim(field%name))
        endif
        do n = 1,m
            atts(n) = field%Att(n)
        enddo
    endif

    if (present(axes)) then
        axes = default_axis
        n = size(axes,kind=c_int)
        m = field%ndim
        if (n .lt. m) then
            call mpp_error(FATAL, &
                           "mpp_get_field_atts: axis array not large enough" &
                               //" for field "//trim(field%name))
        endif
        do n = 1,m
            axes(n) = field%axes(n)
        enddo
    endif

    if (present(siz)) then
        siz = -1
        n = size(siz,kind=c_int)
        m = field%ndim
        if (n .lt. m) then
            call mpp_error(FATAL, &
                           "mpp_get_field_atts: size array not large enough" &
                               //" for field "//trim(field%name))
        endif
        do n = 1,m
            siz(n) = field%size(n)
        enddo
    endif

    if (present(valid)) then
        call mpp_get_valid(field, &
                           valid)
    endif

    if (present(scale)) then
        scale = field%scale
    endif

    if (present(add)) then
        add = field%add
    endif

    if (present(checksum)) then
        checksum = 0
        check_exist = mpp_find_att(field%Att, &
                                   "checksum")
        if (check_exist .ge. 0) then
            checksum = field%checksum
        endif
    endif

end subroutine mpp_get_field_atts

subroutine mpp_get_axis_atts(axis, &
                             name, &
                             units, &
                             longname, &
                             cartesian, &
                             calendar, &
                             sense, &
                             len, &
                             natts, &
                             atts, &
                             compressed)
    implicit none

    !Inputs/outputs
    type(axistype),intent(in) :: axis
    character(len=*),intent(out),optional :: name
    character(len=*),intent(out),optional :: units
    character(len=*),intent(out),optional :: longname
    character(len=*),intent(out),optional :: cartesian
    character(len=*),intent(out),optional :: calendar
    integer(kind=c_int),intent(out),optional :: sense
    integer(kind=c_int),intent(out),optional :: len
    integer(kind=c_int),intent(out),optional :: natts
    type(atttype),dimension(:),intent(inout),optional :: atts
    character(len=*),intent(out),optional :: compressed

    !Local variables
    integer(kind=c_int) :: n
    integer(kind=c_int) :: m

    call openmp_thread_trap()
    if (present(name)) then
        name = axis%name
    endif

    if (present(units)) then
        units = axis%units
    endif

    if (present(longname)) then
        longname = axis%longname
    endif

    if (present(cartesian)) then
        cartesian = axis%cartesian
    endif

    if (present(compressed)) then
        compressed = axis%compressed
    endif

    if (present(calendar)) then
        calendar = axis%calendar
    endif

    if (present(sense)) then 
        sense = axis%sense
    endif

    if (present(len)) then
        len = axis%len
    endif

    if (present(atts)) then
        atts = default_att
        n = size(atts,kind=c_int)
        m = size(axis%Att,kind=c_int)
        if (n .lt. m) then
            call mpp_error(FATAL, &
                           "mpp_get_axis_atts: attribute array not large" &
                               //" enough for axis "//trim(axis%name))
        endif
        do n = 1,m
            atts(n) = axis%Att(n)
        enddo
    endif

    if (present(natts)) then
        natts = size(axis%Att)
    endif

end subroutine mpp_get_axis_atts

subroutine mpp_get_axis_data(axis, &
                             data)
    implicit none

    !Inputs/outputs
    type(axistype),intent(in) :: axis
    real(kind=c_double),dimension(:),intent(out) :: data
    integer(kind=c_int) :: i

    call openmp_thread_trap()
    if (size(data) .lt. axis%len) then
        call mpp_error(FATAL, &
                       "mpp_get_axis_data: data array not large enough" &
                           //" for axis "//trim(axis%name))
    endif

    if (allocated(axis%data) .and. allocated(axis%idata)) then
        call mpp_error(FATAL, &
                       "mpp_get_axis_data: the inputted axis cannot contain" &
                           //" both real and integer data.")
    endif

    if (allocated(axis%data)) then
        data(1:axis%len) = axis%data(1:axis%len)
    elseif (allocated(axis%idata)) then
        do i = 1,axis%len
            data(i) = real(axis%idata(i),kind=c_double)
        enddo
    else
        call mpp_error(FATAL, &
                       "mpp_get_axis_data: the inputted axis does not contain" &
                           //" any real or integer data.")
    endif

end subroutine mpp_get_axis_data

function mpp_get_field_index(fields, &
                             fieldname)
    implicit none

    !Inputs/outputs
    type(fieldtype),dimension(:),intent(in) :: fields
    character(len=*),intent(in) :: fieldname
    integer(kind=c_int) :: mpp_get_field_index

    !Local variables
    integer(kind=c_int) :: n

    call openmp_thread_trap()
    mpp_get_field_index = -1

    do n = 1,size(fields)
        if (lowercase(fields(n)%name) .eq. lowercase(fieldname)) then
            mpp_get_field_index = n
            exit
        endif
    enddo

end function mpp_get_field_index

function mpp_get_field_name(field)
    implicit none

    !Inputs/outputs
    type(fieldtype),intent(in) :: field
    character(len=len(field%name)) :: mpp_get_field_name

    call openmp_thread_trap()
    mpp_get_field_name = field%name

end function mpp_get_field_name

function mpp_get_att_length(att)
    implicit none

    !Inputs/outputs
    type(atttype),intent(in) :: att
    integer(kind=c_int) :: mpp_get_att_length

    call openmp_thread_trap()
    mpp_get_att_length = att%len

end function mpp_get_att_length

function mpp_get_att_type(att)
    implicit none

    !Inputs/outputs
    type(atttype),intent(in) :: att
    integer(kind=c_int) :: mpp_get_att_type

    call openmp_thread_trap()
    mpp_get_att_type = att%type

end function mpp_get_att_type

function mpp_get_att_name(att)
    implicit none

    !Inputs/outputs
    type(atttype),intent(in) :: att
    character(len=len(att%name)) :: mpp_get_att_name

    call openmp_thread_trap()
    mpp_get_att_name = att%name

end function mpp_get_att_name

function mpp_get_att_real(att)
    implicit none

    !Inputs/outputs
    type(atttype),intent(in) :: att
    real(kind=c_double),dimension(size(att%fatt)) :: mpp_get_att_real

    call openmp_thread_trap()
    mpp_get_att_real = att%fatt

end function mpp_get_att_real

function mpp_get_att_char(att)
    implicit none

    !Inputs/outputs
    type(atttype),intent(in) :: att
    character(len=att%len) :: mpp_get_att_char

    call openmp_thread_trap()
    mpp_get_att_char = att%catt

end function mpp_get_att_char

function mpp_get_att_real_scalar(att)
    implicit none

    !Inputs/outputs
    type(atttype),intent(in) :: att
    real(kind=c_double) :: mpp_get_att_real_scalar

    call openmp_thread_trap()
    mpp_get_att_real_scalar = att%fatt(1)

end function mpp_get_att_real_scalar

logical function mpp_is_dist_ioroot(ssize, &
                                    ioroot, &
                                    lsize)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: ssize
    integer(kind=c_int),intent(out),optional :: ioroot
    integer(kind=c_int),intent(out),optional :: lsize

    !Local variables
    integer(kind=c_int) :: rootpe
    integer(kind=c_int) :: d_lsize
    integer(kind=c_int) :: pe
    integer(kind=c_int) :: mypos
    integer(kind=c_int) :: d_ioroot
    integer(kind=c_int) :: npes
    integer(kind=c_int) :: maxpe
    integer(kind=c_int) :: last_ioroot

    call openmp_thread_trap()
    if (ssize .lt. 1) then
        call mpp_error(FATAL, &
                       "mpp_is_dist_ioroot: I/O stripe size < 1.")
    endif
    mpp_is_dist_ioroot = .false.
    rootpe = mpp_root_pe()
    d_lsize = ssize
    pe = mpp_pe()
    mypos = modulo(pe-rootpe,ssize)
    d_ioroot = pe - mypos
    npes = mpp_npes()
    maxpe = min(d_ioroot+ssize,npes+rootpe) - 1
    d_lsize = maxpe - d_ioroot + 1
    if (mod(npes,ssize) .eq. 1) then
        last_ioroot = (npes-1) - ssize
        if (pe .ge. last_ioroot) then
            d_ioroot = last_ioroot
            d_lsize = ssize + 1
        endif
    endif
    if (pe .eq. d_ioroot) then
        mpp_is_dist_ioroot = .true.
    endif
    if (present(ioroot)) then
        ioroot = d_ioroot
    endif
    if (present(lsize)) then
        lsize = d_lsize
    endif

end function mpp_is_dist_ioroot

function mpp_attribute_exist(field, &
                             name)
    implicit none

    !Inputs/outputs
    type(fieldtype),intent(in) :: field
    character(len=*),intent(in) :: name
    logical :: mpp_attribute_exist

    call openmp_thread_trap()
    if (field%natt .gt. 0) then
        mpp_attribute_exist = (mpp_find_att(field%Att,name) .gt. 0)
    else
        mpp_attribute_exist = .false.
    endif

end function mpp_attribute_exist

function mpp_io_clock_on()
    implicit none

    !Inputs/outputs
    logical :: mpp_io_clock_on

    call openmp_thread_trap()
    mpp_io_clock_on = io_clocks_on

end function mpp_io_clock_on

function mpp_get_default_calendar()
    implicit none

    !Inputs/outputs
    character(len=len(default_axis%calendar)) :: mpp_get_default_calendar

    call openmp_thread_trap()
    mpp_get_default_calendar = default_axis%calendar

end function mpp_get_default_calendar

function mpp_get_axis_bounds(axis, &
                             data, &
                             name)
    implicit none

    !Inputs/outputs
    type(axistype),intent(in) :: axis
    real(kind=c_double),dimension(:),intent(out) :: data
    character(len=*),optional,intent(out) :: name
    logical :: mpp_get_axis_bounds

    call openmp_thread_trap()
    if (size(data) .lt. axis%len+1) then
        call mpp_error(FATAL, &
                       "mpp_get_axis_bounds: data array not large enough" &
                           //" for axis "//trim(axis%name))
    endif

    if (.not. allocated(axis%data_bounds)) then
        mpp_get_axis_bounds = .false.
    else
        mpp_get_axis_bounds = .true.
        data(1:axis%len+1) = axis%data_bounds(:)
    endif

    if (present(name)) then
        name = trim(axis%name_bounds)
    endif

end function mpp_get_axis_bounds

function mpp_find_att(atts, &
                      name)
    implicit none

    !Inputs/outputs
    type(atttype),dimension(:),intent(in) :: atts
    character(len=*),intent(in) :: name
    integer(kind=c_int) :: mpp_find_att

    !Local variables
    integer(kind=c_int) :: i

    call openmp_thread_trap()
    mpp_find_att = -1
    do i = 1,size(atts)
        if (trim(name) .eq. trim(atts(i)%name)) then
            mpp_find_att = i
            exit
        endif
    enddo

end function mpp_find_att

subroutine mpp_dist_io_pelist(ssize, &
                              pelist)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: ssize
    integer(kind=c_int),dimension(:),allocatable,intent(out) :: pelist

    !Local variables
    integer(kind=c_int) :: i
    integer(kind=c_int) :: lsize
    integer(kind=c_int) :: ioroot
    logical :: is_ioroot

    call openmp_thread_trap()
    is_ioroot = .false.
    if (ssize .lt. 1) then
        call mpp_error(FATAL, &
                       "mpp_dist_io_pelist: I/O stripe size < 1.")
    endif

    is_ioroot = mpp_is_dist_ioroot(ssize, &
                                   ioroot=ioroot, &
                                   lsize=lsize)

    if (lsize .lt. 1) then
        call mpp_error(FATAL, &
                       "mpp_dist_io_pelist: size of pelist < 1.")
    endif

    allocate(pelist(lsize))
    do i = 1,lsize
        pelist(i) = ioroot + i - 1
    enddo

end subroutine mpp_dist_io_pelist

function mpp_get_maxunits() result(max_num)
    implicit none

    !Inputs/outputs
    integer(kind=c_int) :: max_num

    call openmp_thread_trap()
    max_num = maxunits

end function mpp_get_maxunits

logical function do_cf_compliance()
    call openmp_thread_trap()
    do_cf_compliance = cf_compliance
end function do_cf_compliance

subroutine mpp_modify_field_meta(field, &
                                 name, &
                                 units, &
                                 longname, &
                                 min, &
                                 max, &
                                 missing, &
                                 axes)
    implicit none

    !Inputs/outputs
    type(fieldtype),intent(inout) :: field
    character(len=*),intent(in),optional :: name
    character(len=*),intent(in),optional :: units
    character(len=*),intent(in),optional :: longname
    real(kind=c_double),intent(in),optional :: min
    real(kind=c_double),intent(in),optional :: max
    real(kind=c_double),intent(in),optional :: missing
    type(axistype),dimension(:),intent(inout),optional :: axes

    call openmp_thread_trap()
    if (present(name)) then
        field%name = trim(name)
    endif

    if (present(units)) then
        field%units = trim(units)
    endif

    if (present(longname)) then
        field%longname = trim(longname)
    endif

    if (present(min)) then
        field%min = min
    endif

    if (present(max)) then
        field%max = max
    endif

    if (present(missing)) then
        field%missing = missing
    endif

end subroutine mpp_modify_field_meta

subroutine mpp_modify_axis_meta(axis, &
                                name, &
                                units, &
                                longname, &
                                cartesian, &
                                data)
    implicit none

    !Inputs/outputs
    type(axistype),intent(inout) :: axis
    character(len=*),intent(in),optional :: name
    character(len=*),intent(in),optional :: units
    character(len=*),intent(in),optional :: longname
    character(len=*),intent(in),optional :: cartesian
    real(kind=c_double),dimension(:),intent(in),optional :: data

    call openmp_thread_trap()
    if (present(name)) then
        axis%name = trim(name)
    endif

    if (present(units)) then
        axis%units = trim(units)
    endif

    if (present(longname)) then
        axis%longname = trim(longname)
    endif

    if (present(cartesian)) then
        axis%cartesian = trim(cartesian)
    endif

    if (present(data)) then
        axis%len = size(data)
        if (allocated(axis%data)) then
            deallocate(axis%data)
        endif
        allocate(axis%data(axis%len))
        axis%data = data
    endif

end subroutine mpp_modify_axis_meta
