!Write variable metadata to a netCDF file.
subroutine mpp_write_meta_field(unit, &
                                field, &
                                axes, &
                                name, &
                                units, &
                                longname,&
                                min, &
                                max, &
                                missing, &
                                fill, &
                                scale, &
                                add, &
                                pack, &
                                time_method, &
                                standard_name, &
                                checksum)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(inout) :: field
    type(axistype),dimension(:),intent(in) :: axes
    character(len=*),intent(in) :: name
    character(len=*),intent(in) :: units
    character(len=*),intent(in) :: longname
    real(kind=c_double),intent(in),optional :: min
    real(kind=c_double),intent(in),optional :: max
    real(kind=c_double),intent(in),optional :: missing
    real(kind=c_double),intent(in),optional :: fill
    real(kind=c_double),intent(in),optional :: scale
    real(kind=c_double),intent(in),optional :: add
    integer(kind=c_int),intent(in),optional :: pack
    character(len=*),intent(in),optional :: time_method
    character(len=*),intent(in),optional :: standard_name
    integer(kind=int64),dimension(:),intent(in),optional :: checksum

    call openmp_thread_trap()
    call mpp_write_meta_field_(context, &
                               unit, &
                               field, &
                               axes, &
                               name, &
                               units, &
                               longname,&
                               min, &
                               max, &
                               missing, &
                               fill, &
                               scale, &
                               add, &
                               pack, &
                               time_method, &
                               standard_name, &
                               checksum)

end subroutine mpp_write_meta_field

subroutine mpp_write_meta_field_(context, &
                                 file_index, &
                                 field, &
                                 axes, &
                                 var_name, &
                                 units, &
                                 longname,&
                                 var_min, &
                                 var_max, &
                                 missing, &
                                 fill, &
                                 var_scale, &
                                 add, &
                                 var_pack, &
                                 time_method, &
                                 standard_name, &
                                 checksum)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(fieldtype),intent(inout) :: field
    type(axistype),dimension(:),intent(in) :: axes
    character(len=*),intent(in) :: var_name
    character(len=*),intent(in) :: units
    character(len=*),intent(in) :: longname
    real(kind=c_double),intent(in),optional :: var_min
    real(kind=c_double),intent(in),optional :: var_max
    real(kind=c_double),intent(in),optional :: missing
    real(kind=c_double),intent(in),optional :: fill
    real(kind=c_double),intent(in),optional :: var_scale
    real(kind=c_double),intent(in),optional :: add
    integer(kind=c_int),intent(in),optional :: var_pack
    character(len=*),intent(in),optional :: time_method
    character(len=*),intent(in),optional :: standard_name
    integer(kind=int64),dimension(:),intent(in),optional :: checksum

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: name_len
    character(kind=c_char,len=1),dimension(:),allocatable :: name_buf
    integer(kind=c_int) :: i
    logical(kind=c_bool) :: found_unlimited_dimension
    integer(kind=c_int),dimension(:),allocatable :: axis_id
    integer(kind=c_int) :: type_in_file
    integer(kind=c_int) :: variable_index
    real(kind=c_double) :: a
    real(kind=c_double) :: b
    character(len=64) :: checksum_char
    integer(kind=c_int) :: pack_size
    real(kind=c_double) :: add_val
    real(kind=c_double) :: scale_val
    integer(kind=c_int) :: ishift
    integer(kind=c_int) :: jshift
    logical(kind=c_bool) :: foundx
    logical(kind=c_bool) :: foundy
    integer(kind=c_int) :: num_axes

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    !Make sure that only one "X" and one "Y" axis was passed in, store
    !them (for all non-writing ranks), and use them
    !to determine the "2d domain position" of the field.
    foundx = .false._c_bool
    foundy = .false._c_bool

    if (.not. context%files(file_index)%is_writer) then
        if (allocated(field%axes)) then
                call mpp_error(WARNING, &
                               "mpp_write_meta_field_: the current field has" &
                                   //" already had its metadata defined.  It is" &
                                   //" being overwritten.")
                deallocate(field%axes)
        endif
        allocate(field%axes(2))
    endif

    do i = 1,size(axes)
        if (axes(i)%cartesian .eq. "X") then
            if (.not. foundx) then
                ishift = axes(i)%shift
                foundx = .true._c_bool
                if (.not. context%files(file_index)%is_writer) then
                    field%axes(1) = axes(i)
                endif
            else
                call mpp_error(FATAL, &
                               "mpp_write_meta_field_: the inputted" &
                                   //" axes array contains more than one" &
                                   //" x axis.")
            endif
        elseif (axes(i)%cartesian .eq. "Y") then
            if (.not. foundy) then
                jshift = axes(i)%shift
                foundy = .true._c_bool
                if (.not. context%files(file_index)%is_writer) then
                    field%axes(2) = axes(i)
                endif
            else
                call mpp_error(FATAL, &
                               "mpp_write_meta_field_: the inputted" &
                                   //" axes array contains more than one" &
                                   //" y axis.")
            endif
        endif
    enddo

    !Position is only needed for the 2d domain decomposed writes.
    if (foundx .and. foundy) then
        if (ishift .eq. 1 .and. jshift .eq. 1) then
            field%position = CORNER
        elseif (ishift .eq. 1) then
            field%position = EAST
        elseif (jshift .eq. 1) then
            field%position = NORTH
        else
            field%position = CENTER
        endif
    endif

    if (context%files(file_index)%is_writer) then

        !Issue a warning if fill and missing are both present and different.
        if (present(fill) .and. present(missing)) then
            if (fill .ne. missing) then
                call mpp_error(WARNING, &
                               "mpp_write_meta_field_: netCDF attributes fill" &
                                   //" value and missing value must be equal.")
            endif
        endif

        !Place the name in an interoperable buffer.
        name_len = len_trim(var_name)
        allocate(name_buf(name_len+1))
        do i = 1,name_len
            name_buf(i) = var_name(i:i)
        enddo
        name_buf(name_len+1) = c_null_char

        !Fill an array with dimension indices that where stored when the metadata
        !for each dimension was defined.  Make sure that only one unlimited
        !dimension was passed in.  Store the variable id of the unlimited
        !dimension, later when the unlimited axis data value will be updated.
        found_unlimited_dimension = .false._c_bool
        field%time_axis_index = -1
        num_axes = size(axes)
        allocate(axis_id(num_axes))
        if (allocated(field%axes)) then
                call mpp_error(WARNING, &
                               "mpp_write_meta_field_: the current field has" &
                                   //" already had its metadata defined.  It is" &
                                   //" being overwritten.")
                deallocate(field%axes)
        endif
        allocate(field%axes(num_axes))
        do i = 1,num_axes
            field%axes(i) = axes(i)

            !Since c reads the data in "row-based" order, flip the
            !dimension order.
            axis_id(num_axes+1-i) = axes(i)%did

            if (mpp_io_c_get_netcdf_dimension_is_unlimited(f, &
                                                           axis_id(num_axes+1-i))) then
                if (found_unlimited_dimension) then
                    call mpp_error(FATAL, &
                                   "mpp_write_meta_field_: netcdf variables" &
                                       //" with more than one unlimited" &
                                       //" are not currently supported.")
                endif
                found_unlimited_dimension = .true._c_bool
                field%time_axis_index = i
            endif
        enddo

        !Determine the type that the variable will be stored in the netcdf file
        !as.
        if (present(var_pack)) then
            select case (var_pack)
                case(0)
                    type_in_file = MPP_IO_C_NC_INT
                case(1)
                    type_in_file = MPP_IO_C_NC_DOUBLE
                case(2)
                    type_in_file = MPP_IO_C_NC_FLOAT
                case(4)
                    if (.not. present(var_scale) .or. .not. present(add)) then
                        call mpp_error(FATAL, &
                                       "mpp_write_meta_field_: scale and add" &
                                           //" must be supplied when pack = 4.")
                    endif
                    type_in_file = MPP_IO_C_NC_SHORT
                case(8)
                    if (.not. present(var_scale) .or. .not. present(add)) then
                        call mpp_error(FATAL, &
                                       "mpp_write_meta_field_: scale and add" &
                                           //" must be supplied when pack = 8.")
                    endif
                    type_in_file = MPP_IO_C_NC_BYTE
                case default
                    call mpp_error(FATAL, &
                                   "mpp_write_meta_field_: only legal packing" &
                                       //" values are 0,1,2,4,8.")
            end select
        else
            type_in_file = MPP_IO_C_NC_FLOAT
        endif

        !Write the variable metadata to the netcdf file.
        variable_index = mpp_io_c_write_netcdf_variable_metadata(f, &
                                                                 type_in_file, &
                                                                 name_buf, &
                                                                 axis_id, &
                                                                 size(axis_id,kind=c_int), &
                                                                 MPP_IO_C_MAX_ATTS_PER_VAR, &
                                                                 .true._c_bool)

        !Store the variable_index.
        field%id = variable_index
        deallocate(name_buf)
        deallocate(axis_id)

        !Write out variable attributes.
        call mpp_write_attribute_metadata(context, &
                                          file_index, &
                                          variable_index, &
                                          "long_name", &
                                          len_trim(longname,kind=c_size_t), &
                                          cval=longname)

        if (lowercase(trim(units)) .ne. "none" .or. .not. cf_compliance) then
            call mpp_write_attribute_metadata(context, &
                                              file_index, &
                                              variable_index, &
                                              "units", &
                                              len_trim(units,kind=c_size_t), &
                                              cval=units)
        endif

        if (present(var_pack)) then
            pack_size = var_pack
        else
            pack_size = 2
        endif

        if (present(add)) then
            add_val = add
        else
            add_val = 0.0
        endif

        if (present(var_scale)) then
            scale_val = var_scale
        else
            scale_val = 1.0
        endif

        if (present(var_min) .and. present(var_max)) then
            if (pack_size .eq. 1 .or. pack_size .eq. 2) then
                call mpp_write_attribute_metadata(context, &
                                                  file_index, &
                                                  variable_index, &
                                                  "valid_range", &
                                                  int(2,kind=c_size_t), &
                                                  rval=(/var_min,var_max/), &
                                                  pack_param=pack_size)
            else
                a = nint((var_min-add_val)/scale_val)
                b = nint((var_max-add_val)/scale_val)
                call mpp_write_attribute_metadata(context, &
                                                  file_index, &
                                                  variable_index, &
                                                  "valid_range", &
                                                  int(2,kind=c_size_t), &
                                                  rval=(/a,b/), &
                                                  pack_param=pack_size)
            endif
        elseif (present(var_min)) then
            if (pack_size .eq. 1 .or. pack_size .eq. 2) then
                call mpp_write_attribute_metadata(context, &
                                                  file_index, &
                                                  variable_index, &
                                                  "valid_min", &
                                                  int(1,kind=c_size_t), &
                                                  rval=(/var_min/), &
                                                  pack_param=pack_size)
            else
                a = nint((var_min-add_val)/scale_val)
                call mpp_write_attribute_metadata(context, &
                                                  file_index, &
                                                  variable_index, &
                                                  "valid_min", &
                                                  int(1,kind=c_size_t), &
                                                  rval=(/a/), &
                                                  pack_param=pack_size)
            endif
        elseif (present(var_max)) then
            if (pack_size .eq. 1 .or. pack_size .eq. 2) then
                call mpp_write_attribute_metadata(context, &
                                                  file_index, &
                                                  variable_index, &
                                                  "valid_max", &
                                                  int(1,kind=c_size_t), &
                                                  rval=(/var_max/), &
                                                  pack_param=pack_size)
            else
                a = nint((var_max-add_val)/scale_val)
                call mpp_write_attribute_metadata(context, &
                                                  file_index, &
                                                  variable_index, &
                                                  "valid_max", &
                                                  int(1,kind=c_size_t), &
                                                  rval=(/a/), &
                                                  pack_param=pack_size)
            endif
        endif

        if (present(missing)) then
            if (pack_size .eq. 1 .or. pack_size .eq. 2) then
                call mpp_write_attribute_metadata(context, &
                                                  file_index, &
                                                  variable_index, &
                                                  "missing_value", &
                                                  int(1,kind=c_size_t), &
                                                  rval=(/missing/), &
                                                  pack_param=pack_size)
            else
                a = nint((missing-add_val)/scale_val)
                call mpp_write_attribute_metadata(context, &
                                                  file_index, &
                                                  variable_index, &
                                                  "missing_value", &
                                                  int(1,kind=c_size_t), &
                                                  rval=(/a/), &
                                                  pack_param=pack_size)
            endif
        endif

        if (present(fill)) then
            if (pack_size .eq. 1 .or. pack_size .eq. 2) then
                call mpp_write_attribute_metadata(context, &
                                                  file_index, &
                                                  variable_index, &
                                                  "_FillValue", &
                                                  int(1,kind=c_size_t), &
                                                  rval=(/fill/), &
                                                  pack_param=pack_size)
            elseif (pack_size .eq. 0) then
                if (present(var_scale) .or. present(add)) then
                    call mpp_error(FATAL, &
                                   "mpp_write_meta_field: add ,scale not currently" &
                                       //" implemented for pack=0 int handling," &
                                       //" try reals instead.")
                else
                    call mpp_write_attribute_metadata(context, &
                                                      file_index, &
                                                      variable_index, &
                                                      "_FillValue", &
                                                      int(1,kind=c_size_t), &
                                                      ival=(/MPP_FILL_INT/), &
                                                      pack_param=pack_size)
                endif
            else
                a = nint((fill-add_val)/scale_val)
                call mpp_write_attribute_metadata(context, &
                                                  file_index, &
                                                  variable_index, &
                                                  "_FillValue", &
                                                  int(1,kind=c_size_t), &
                                                  rval=(/a/), &
                                                  pack_param=pack_size)
            endif
        endif

        if (pack_size .ne. 1 .and. pack_size .ne. 2) then
            call mpp_write_attribute_metadata(context, &
                                              file_index, &
                                              variable_index, &
                                              "packing", &
                                              int(1,kind=c_size_t), &
                                              ival=(/pack_size/))
            if (present(var_scale)) then
                call mpp_write_attribute_metadata(context, &
                                                  file_index, &
                                                  variable_index, &
                                                  "scale_factor", &
                                                  int(1,kind=c_size_t), &
                                                  rval=(/var_scale/))
            endif
            if (present(add)) then
                call mpp_write_attribute_metadata(context, &
                                                  file_index, &
                                                  variable_index, &
                                                  "add_offset", &
                                                  int(1,kind=c_size_t), &
                                                  rval=(/add/))
            endif
        endif

        if (present(checksum)) then
            write (checksum_char,'(Z16)') checksum(1)
            do i = 2,size(checksum)
                write (checksum_char,'(a,Z16)') trim(checksum_char)//",",checksum(i)
            enddo
            call mpp_write_attribute_metadata(context, &
                                              file_index, &
                                              variable_index, &
                                              "checksum", &
                                              len_trim(checksum_char,kind=c_size_t), &
                                              cval=checksum_char)
        endif

        if (present(time_method)) then
            call mpp_write_attribute_metadata(context, &
                                              file_index, &
                                              variable_index, &
                                              "cell_methods", &
                                              len_trim(time_method,kind=c_size_t)+6, &
                                              cval="time: "//trim(time_method))
        endif

        if (present(standard_name)) then
            call mpp_write_attribute_metadata(context, &
                                              file_index, &
                                              variable_index, &
                                              "standard_name", &
                                              len_trim(time_method,kind=c_size_t), &
                                              cval=standard_name)
        endif
    endif

end subroutine mpp_write_meta_field_
