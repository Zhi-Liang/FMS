!Write out data that has been decomposed across a domain.
subroutine mpp_write_2ddecomp_r2d(unit, &
                                  field, &
                                  domain, &
                                  data, &
                                  tstamp, &
                                  tile_count, &
                                  default_data)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    type(domain2d),intent(inout) :: domain
    real(kind=c_double),dimension(:,:),intent(inout) :: data
    real(kind=c_double),intent(in),optional :: tstamp
    integer(kind=c_int),intent(in),optional :: tile_count
    real(kind=c_double),intent(in),optional :: default_data

    call openmp_thread_trap()
    call mpp_write_2ddecomp_r2d_(context, &
                                 unit, &
                                 field, &
                                 domain, &
                                 data, &
                                 tstamp, &
                                 tile_count, &
                                 default_data)

end subroutine mpp_write_2ddecomp_r2d

subroutine mpp_write_2ddecomp_r3d(unit, &
                                  field, &
                                  domain, &
                                  data, &
                                  tstamp, &
                                  tile_count, &
                                  default_data)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    type(domain2d),intent(inout) :: domain
    real(kind=c_double),dimension(:,:,:),intent(inout) :: data
    real(kind=c_double),intent(in),optional :: tstamp
    integer(kind=c_int),intent(in),optional :: tile_count
    real(kind=c_double),intent(in),optional :: default_data

    call openmp_thread_trap()
    call mpp_write_2ddecomp_r3d_(context, &
                                 unit, &
                                 field, &
                                 domain, &
                                 data, &
                                 tstamp, &
                                 tile_count, &
                                 default_data)

end subroutine mpp_write_2ddecomp_r3d

subroutine mpp_write_2ddecomp_r4d(unit, &
                                  field, &
                                  domain, &
                                  data, &
                                  tstamp, &
                                  tile_count, &
                                  default_data)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    type(domain2d),intent(inout) :: domain
    real(kind=c_double),dimension(:,:,:,:),intent(inout) :: data
    real(kind=c_double),intent(in),optional :: tstamp
    integer(kind=c_int),intent(in),optional :: tile_count
    real(kind=c_double),intent(in),optional :: default_data

    call openmp_thread_trap()
    call mpp_write_2ddecomp_r4d_(context, &
                                 unit, &
                                 field, &
                                 domain, &
                                 data, &
                                 tstamp, &
                                 tile_count, &
                                 default_data)

end subroutine mpp_write_2ddecomp_r4d

subroutine mpp_write_2ddecomp_r2d_(context, &
                                   file_index, &
                                   field, &
                                   domain, &
                                   var_data, &
                                   unlim_dim_val, &
                                   tile_count, &
                                   default_data)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(fieldtype),intent(in) :: field
    type(domain2d),target,intent(inout) :: domain
    real(kind=c_double),dimension(:,:),intent(inout) :: var_data
    real(kind=c_double),intent(in),optional :: unlim_dim_val
    integer(kind=c_int),intent(in),optional :: tile_count
    real(kind=c_double),intent(in),optional :: default_data

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: dposition
    integer(kind=c_int) :: is
    integer(kind=c_int) :: ie
    integer(kind=c_int) :: js
    integer(kind=c_int) :: je
    integer(kind=c_int) :: isd
    integer(kind=c_int) :: ied
    integer(kind=c_int) :: jsd
    integer(kind=c_int) :: jed
    logical :: x_is_global
    logical :: y_is_global
    integer(kind=c_int) :: ism
    integer(kind=c_int) :: iem
    integer(kind=c_int) :: jsm
    integer(kind=c_int) :: jem
    logical(kind=c_bool) :: data_has_halos
    logical(kind=c_bool) :: halos_are_global
    integer(kind=c_int) :: isg
    integer(kind=c_int) :: ieg
    integer(kind=c_int) :: jsg
    integer(kind=c_int) :: jeg
    real(kind=c_double),dimension(:,:),allocatable :: gdata
    type(domain2d),pointer :: domain_ptr
    type(domain1d) :: xdomain
    type(domain1d) :: ydomain
    integer(kind=c_int) :: i
    logical(kind=c_bool) :: foundx
    logical(kind=c_bool) :: foundy
    real(kind=c_double),dimension(:,:),allocatable :: cdata

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    if (associated(context%files(file_index)%domain)) then

        !If there is a 2d domain stored in memory for the file, check that the
        !inputted domain matches the stored domain.
        call domain2d_check(context, &
                            file_index, &
                            domain)

    else

        !Check that the components of the inputted 2d domain match the 1d
        !domains pointed to by the "X" and "Y" axes of the inputted field.
        call mpp_get_domain_components(domain, &
                                       xdomain, &
                                       ydomain)

        !Make sure that the inputted field has a valid number of axes.
        if (context%files(file_index)%is_writer) then
            if (size(field%axes) .lt. 2 .or. size(field%axes) .gt. 3) then
                call mpp_error(FATAL, &
                               "mpp_write_2ddecomp_r2d_: the inputted field" &
                                   //" must have either 2 or 3 axes.")
            endif
        else
            if (size(field%axes) .ne. 2) then
                call mpp_error(FATAL, &
                               "mpp_write_2ddecomp_r2d_: the inputted field" &
                                   //" must have 2 axes.")
            endif
        endif

        !Make sure that components of the inputted 2d domain match the
        !domains pointed at by the axes of the inputted field.
        foundx = .false._c_bool
        foundy = .false._c_bool
        do i = 1,size(field%axes)
            if (field%axes(i)%cartesian .eq. "X") then
                if (.not. foundx) then
                    if (field%axes(i)%domain .ne. xdomain) then
                        call mpp_error(FATAL, &
                                       "mpp_write_2ddecomp_r2d_: the x component" &
                                           //" of the inputted 2d domain differs" &
                                           //" from the 1d domain associated with" &
                                           //" the x axis of the inputted field.")
                    endif
                    foundx = .true._c_bool
                else
                    call mpp_error(FATAL, &
                                   "mpp_write_2ddecomp_r2d_: the inputted field" &
                                       //" has more than one x axis.")
                endif
            elseif (field%axes(i)%cartesian .eq. "Y") then
                if (.not. foundy) then
                    if (field%axes(i)%domain .ne. ydomain) then
                        call mpp_error(FATAL, &
                                       "mpp_write_2ddecomp_r2d_: the y component" &
                                           //" of the inputted 2d domain differs" &
                                           //" from the 1d domain associated with" &
                                           //" the y axis of the inputted field.")
                    endif
                    foundy = .true._c_bool
                else
                    call mpp_error(FATAL, &
                                   "mpp_write_2ddecomp_r2d_: the inputted field" &
                                       //" has more than one y axis.")
                endif
            endif
        enddo
        if (.not. (foundx .and. foundy)) then
            call mpp_error(FATAL, &
                           "mpp_write_2ddecomp_r2d_: the inputted field" &
                               //" is missing either an x or y axis (or both).")
        endif
    endif

    !Get the 2d domain position of the field and different domain starting
    !and ending indices.
    dposition = field%position
    call mpp_get_compute_domain(domain, &
                                is, &
                                ie, &
                                js, &
                                je, &
                                tile_count=tile_count, &
                                position=dposition)
    call mpp_get_data_domain(domain, &
                             isd, &
                             ied, &
                             jsd, &
                             jed, &
                             x_is_global=x_is_global, &
                             y_is_global=y_is_global, &
                             tile_count=tile_count, &
                             position=dposition)
    call mpp_get_memory_domain(domain, &
                               ism, &
                               iem, &
                               jsm, &
                               jem, &
                               position=dposition)

    !Figure out if the inputted data contains halos.
    if (size(var_data,1) .eq. ie-is+1 .and. size(var_data,2) .eq. &
            je-js+1) then
        data_has_halos = .false._c_bool
    elseif (size(var_data,1) .eq. iem-ism+1 .and. size(var_data,2) .eq. &
                jem-jsm+1) then
        data_has_halos = .true._c_bool
    else
        write(error_unit,'(a,10i5)') "mpp_write_2ddecomp fails on field " &
                                         //trim(field%name) &
                                         //": is,ie,js,je, ism,iem,jsm,jem," &
                                         //" size(data,1), size(data,2)=", &
                                     is, &
                                     ie, &
                                     js, &
                                     je, &
                                     ism, &
                                     iem, &
                                     jsm, &
                                     jem, &
                                     size(var_data,1), &
                                     size(var_data,2)
        call mpp_error(FATAL, &
                       "mpp_write: data must be either on compute domain" &
                           //" or data domain.")
    endif

    !Figure out if the halos are global.
    halos_are_global = x_is_global .and. y_is_global

    if (context%files(file_index)%comm_required .or. &
            associated(context%files(file_index)%io_domain)) then

        !Here, communication between ranks is necessary because either a
        !domain update is needed, or the writing rank must write data it
        !does not own.

        if (halos_are_global) then

            !Perform the inter-rank communication.
            call mpp_update_domains(var_data, &
                                    domain, &
                                    position=dposition)

            if (context%files(file_index)%is_writer) then

                !Write the data to the file.
                call mpp_write_r2d(file_index, &
                                   field, &
                                   var_data, &
                                   unlim_dim_val)
            endif

        else
            domain_ptr => null()

            if (associated(context%files(file_index)%io_domain) .and. &
                    context%files(file_index)%data_in_multi_files) then
                domain_ptr => context%files(file_index)%io_domain
            else
                domain_ptr => domain
            endif

            !Get the field data from the domain pointed to by domain_ptr.
            call mpp_get_global_domain(domain_ptr, &
                                       isg, &
                                       ieg, &
                                       jsg, &
                                       jeg, &
                                       tile_count=tile_count, &
                                       position=dposition)

            if (context%files(file_index)%is_writer .or. .not. &
                    global_field_on_root_pe) then
                allocate(gdata(isg:ieg,jsg:jeg))
            else
                allocate(gdata(1,1))
            endif

            if (global_field_on_root_pe) then
                call mpp_global_field(domain_ptr, &
                                      var_data, &
                                      gdata, &
                                      position=dposition, &
                                      flags=XUPDATE+YUPDATE+GLOBAL_ROOT_ONLY, &
                                      default_data=default_data)
            else
                call mpp_global_field(domain_ptr, &
                                      var_data, &
                                      gdata, &
                                      position=dposition, &
                                      default_data=default_data)
            endif

            if (context%files(file_index)%is_writer) then

                !Write out the data.
                call mpp_write_r2d(file_index, &
                                   field, &
                                   gdata, &
                                   unlim_dim_val)
            endif
            deallocate(gdata)
            domain_ptr => null()
        endif

    else

        if (.not. context%files(file_index)%is_writer) then
            call mpp_error(FATAL, &
                           "mpp_write_2ddecomp_r2d_: this branch should" &
                               //" be reached.")
        endif

        !Here, no domain updated is needed and inter-rank communication is
        !not required because the writing rank is only writing data it owns.

        if (data_has_halos) then

            !First copy the data into a contiguous buffer and then write it
            !out.
            allocate(cdata(is:ie,js:je))
            cdata(:,:) = var_data(is-isd+1:ie-isd+1,js-jsd+1:je-jsd+1)
            call mpp_write_r2d(file_index, &
                               field, &
                               cdata, &
                               unlim_dim_val)
            deallocate(cdata)
        else

            !The data is already contiguous, so just write it out.
            call mpp_write_r2d(file_index, &
                               field, &
                               var_data, &
                               unlim_dim_val)
        endif
    endif

end subroutine mpp_write_2ddecomp_r2d_

subroutine mpp_write_2ddecomp_r3d_(context, &
                                   file_index, &
                                   field, &
                                   domain, &
                                   var_data, &
                                   unlim_dim_val, &
                                   tile_count, &
                                   default_data)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(fieldtype),intent(in) :: field
    type(domain2d),target,intent(inout) :: domain
    real(kind=c_double),dimension(:,:,:),intent(inout) :: var_data
    real(kind=c_double),intent(in),optional :: unlim_dim_val
    integer(kind=c_int),intent(in),optional :: tile_count
    real(kind=c_double),intent(in),optional :: default_data

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: dposition
    integer(kind=c_int) :: is
    integer(kind=c_int) :: ie
    integer(kind=c_int) :: js
    integer(kind=c_int) :: je
    integer(kind=c_int) :: isd
    integer(kind=c_int) :: ied
    integer(kind=c_int) :: jsd
    integer(kind=c_int) :: jed
    logical :: x_is_global
    logical :: y_is_global
    integer(kind=c_int) :: ism
    integer(kind=c_int) :: iem
    integer(kind=c_int) :: jsm
    integer(kind=c_int) :: jem
    logical(kind=c_bool) :: data_has_halos
    logical(kind=c_bool) :: halos_are_global
    integer(kind=c_int) :: isg
    integer(kind=c_int) :: ieg
    integer(kind=c_int) :: jsg
    integer(kind=c_int) :: jeg
    real(kind=c_double),dimension(:,:,:),allocatable :: gdata
    type(domain2d),pointer :: domain_ptr
    type(domain1d) :: xdomain
    type(domain1d) :: ydomain
    integer(kind=c_int) :: i
    logical(kind=c_bool) :: foundx
    logical(kind=c_bool) :: foundy
    real(kind=c_double),dimension(:,:,:),allocatable :: cdata

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    if (associated(context%files(file_index)%domain)) then

        !If there is a 2d domain stored in memory for the file, check that the
        !inputted domain matches the stored domain.
        call domain2d_check(context, &
                            file_index, &
                            domain)

    else

        !Check that the components of the inputted 2d domain match the 1d
        !domains pointed to by the "X" and "Y" axes of the inputted field.
        call mpp_get_domain_components(domain, &
                                       xdomain, &
                                       ydomain)

        !Make sure that the inputted field has a valid number of axes.
        if (context%files(file_index)%is_writer) then
            if (size(field%axes) .lt. 3 .or. size(field%axes) .gt. 4) then
                call mpp_error(FATAL, &
                               "mpp_write_2ddecomp_r3d_: the inputted field" &
                                   //" must have either 3 or 4 axes.")
            endif
        else
            if (size(field%axes) .ne. 2) then
                call mpp_error(FATAL, &
                               "mpp_write_2ddecomp_r3d_: the inputted field" &
                                   //" must have 2 axes.")
            endif
        endif

        !Make sure that components of the inputted 2d domain match the
        !domains pointed at by the axes of the inputted field.
        foundx = .false._c_bool
        foundy = .false._c_bool
        do i = 1,size(field%axes)
            if (field%axes(i)%cartesian .eq. "X") then
                if (.not. foundx) then
                    if (field%axes(i)%domain .ne. xdomain) then
                        call mpp_error(FATAL, &
                                       "mpp_write_2ddecomp_r3d_: the x component" &
                                           //" of the inputted 2d domain differs" &
                                           //" from the 1d domain associated with" &
                                           //" the x axis of the inputted field.")
                    endif
                    foundx = .true._c_bool
                else
                    call mpp_error(FATAL, &
                                   "mpp_write_2ddecomp_r3d_: the inputted field" &
                                       //" has more than one x axis.")
                endif
            elseif (field%axes(i)%cartesian .eq. "Y") then
                if (.not. foundy) then
                    if (field%axes(i)%domain .ne. ydomain) then
                        call mpp_error(FATAL, &
                                       "mpp_write_2ddecomp_r3d_: the y component" &
                                           //" of the inputted 2d domain differs" &
                                           //" from the 1d domain associated with" &
                                           //" the y axis of the inputted field.")
                    endif
                    foundy = .true._c_bool
                else
                    call mpp_error(FATAL, &
                                   "mpp_write_2ddecomp_r3d_: the inputted field" &
                                       //" has more than one y axis.")
                endif
            endif
        enddo
        if (.not. (foundx .and. foundy)) then
            call mpp_error(FATAL, &
                           "mpp_write_2ddecomp_r3d_: the inputted field" &
                               //" is missing either an x or y axis (or both).")
        endif
    endif

    !Get the 2d domain position of the field and different domain starting
    !and ending indices.
    dposition = field%position
    call mpp_get_compute_domain(domain, &
                                is, &
                                ie, &
                                js, &
                                je, &
                                tile_count=tile_count, &
                                position=dposition)
    call mpp_get_data_domain(domain, &
                             isd, &
                             ied, &
                             jsd, &
                             jed, &
                             x_is_global=x_is_global, &
                             y_is_global=y_is_global, &
                             tile_count=tile_count, &
                             position=dposition)
    call mpp_get_memory_domain(domain, &
                               ism, &
                               iem, &
                               jsm, &
                               jem, &
                               position=dposition)

    !Figure out if the inputted data contains halos.
    if (size(var_data,1) .eq. ie-is+1 .and. size(var_data,2) .eq. &
            je-js+1) then
        data_has_halos = .false._c_bool
    elseif (size(var_data,1) .eq. iem-ism+1 .and. size(var_data,2) .eq. &
                jem-jsm+1) then
        data_has_halos = .true._c_bool
    else
        write(error_unit,'(a,10i5)') "mpp_write_2ddecomp fails on field " &
                                         //trim(field%name) &
                                         //": is,ie,js,je, ism,iem,jsm,jem," &
                                         //" size(data,1), size(data,2)=", &
                                     is, &
                                     ie, &
                                     js, &
                                     je, &
                                     ism, &
                                     iem, &
                                     jsm, &
                                     jem, &
                                     size(var_data,1), &
                                     size(var_data,2)
        call mpp_error(FATAL, &
                       "mpp_write: data must be either on compute domain" &
                           //" or data domain.")
    endif

    !Figure out if the halos are global.
    halos_are_global = x_is_global .and. y_is_global

    if (context%files(file_index)%comm_required .or. &
            associated(context%files(file_index)%io_domain)) then

        !Here, communication between ranks is necessary because either a
        !domain update is needed, or the writing rank must write data it
        !does not own.

        if (halos_are_global) then

            !Perform the inter-rank communication.
            call mpp_update_domains(var_data, &
                                    domain, &
                                    position=dposition)

            if (context%files(file_index)%is_writer) then

                !Write the data to the file.
                call mpp_write_r3d(file_index, &
                                   field, &
                                   var_data, &
                                   unlim_dim_val)
            endif

        else
            domain_ptr => null()

            if (associated(context%files(file_index)%io_domain) .and. &
                    context%files(file_index)%data_in_multi_files) then
                domain_ptr => context%files(file_index)%io_domain
            else
                domain_ptr => domain
            endif

            !Get the field data from the domain pointed to by domain_ptr.
            call mpp_get_global_domain(domain_ptr, &
                                       isg, &
                                       ieg, &
                                       jsg, &
                                       jeg, &
                                       tile_count=tile_count, &
                                       position=dposition)

            if (context%files(file_index)%is_writer .or. .not. &
                    global_field_on_root_pe) then
                allocate(gdata(isg:ieg,jsg:jeg,size(var_data,3)))
            else
                allocate(gdata(1,1,1))
            endif

            if (global_field_on_root_pe) then
                call mpp_global_field(domain_ptr, &
                                      var_data, &
                                      gdata, &
                                      position=dposition, &
                                      flags=XUPDATE+YUPDATE+GLOBAL_ROOT_ONLY, &
                                      default_data=default_data)
            else
                call mpp_global_field(domain_ptr, &
                                      var_data, &
                                      gdata, &
                                      position=dposition, &
                                      default_data=default_data)
            endif

            if (context%files(file_index)%is_writer) then

                !Write out the data.
                call mpp_write_r3d(file_index, &
                                   field, &
                                   gdata, &
                                   unlim_dim_val)
            endif
            deallocate(gdata)
            domain_ptr => null()
        endif

    else

        if (.not. context%files(file_index)%is_writer) then
            call mpp_error(FATAL, &
                           "mpp_write_2ddecomp_r3d_: this branch should" &
                               //" be reached.")
        endif

        !Here, no domain updated is needed and inter-rank communication is
        !not required because the writing rank is only writing data it owns.

        if (data_has_halos) then

            !First copy the data into a contiguous buffer and then write it
            !out.
            allocate(cdata(is:ie,js:je,size(var_data,3)))
            cdata(:,:,:) = var_data(is-isd+1:ie-isd+1,js-jsd+1:je-jsd+1,:)
            call mpp_write_r3d(file_index, &
                               field, &
                               cdata, &
                               unlim_dim_val)
            deallocate(cdata)
        else

            !The data is already contiguous, so just write it out.
            call mpp_write_r3d(file_index, &
                               field, &
                               var_data, &
                               unlim_dim_val)
        endif
    endif

end subroutine mpp_write_2ddecomp_r3d_

subroutine mpp_write_2ddecomp_r4d_(context, &
                                   file_index, &
                                   field, &
                                   domain, &
                                   var_data, &
                                   unlim_dim_val, &
                                   tile_count, &
                                   default_data)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(fieldtype),intent(in) :: field
    type(domain2d),target,intent(inout) :: domain
    real(kind=c_double),dimension(:,:,:,:),intent(inout) :: var_data
    real(kind=c_double),intent(in),optional :: unlim_dim_val
    integer(kind=c_int),intent(in),optional :: tile_count
    real(kind=c_double),intent(in),optional :: default_data

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: dposition
    integer(kind=c_int) :: is
    integer(kind=c_int) :: ie
    integer(kind=c_int) :: js
    integer(kind=c_int) :: je
    integer(kind=c_int) :: isd
    integer(kind=c_int) :: ied
    integer(kind=c_int) :: jsd
    integer(kind=c_int) :: jed
    logical :: x_is_global
    logical :: y_is_global
    integer(kind=c_int) :: ism
    integer(kind=c_int) :: iem
    integer(kind=c_int) :: jsm
    integer(kind=c_int) :: jem
    logical(kind=c_bool) :: data_has_halos
    logical(kind=c_bool) :: halos_are_global
    integer(kind=c_int) :: isg
    integer(kind=c_int) :: ieg
    integer(kind=c_int) :: jsg
    integer(kind=c_int) :: jeg
    real(kind=c_double),dimension(:,:,:,:),allocatable :: gdata
    type(domain2d),pointer :: domain_ptr
    type(domain1d) :: xdomain
    type(domain1d) :: ydomain
    integer(kind=c_int) :: i
    logical(kind=c_bool) :: foundx
    logical(kind=c_bool) :: foundy
    real(kind=c_double),dimension(:,:,:,:),allocatable :: cdata

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    if (associated(context%files(file_index)%domain)) then

        !If there is a 2d domain stored in memory for the file, check that the
        !inputted domain matches the stored domain.
        call domain2d_check(context, &
                            file_index, &
                            domain)

    else

        !Check that the components of the inputted 2d domain match the 1d
        !domains pointed to by the "X" and "Y" axes of the inputted field.
        call mpp_get_domain_components(domain, &
                                       xdomain, &
                                       ydomain)

        !Make sure that the inputted field has a valid number of axes.
        if (context%files(file_index)%is_writer) then
            if (size(field%axes) .lt. 4 .or. size(field%axes) .gt. 5) then
                call mpp_error(FATAL, &
                               "mpp_write_2ddecomp_r4d_: the inputted field" &
                                   //" must have either 4 or 5 axes.")
            endif
        else
            if (size(field%axes) .ne. 2) then
                call mpp_error(FATAL, &
                               "mpp_write_2ddecomp_r4d_: the inputted field" &
                                   //" must have 2 axes.")
            endif
        endif

        !Make sure that components of the inputted 2d domain match the
        !domains pointed at by the axes of the inputted field.
        foundx = .false._c_bool
        foundy = .false._c_bool
        do i = 1,size(field%axes)
            if (field%axes(i)%cartesian .eq. "X") then
                if (.not. foundx) then
                    if (field%axes(i)%domain .ne. xdomain) then
                        call mpp_error(FATAL, &
                                       "mpp_write_2ddecomp_r4d_: the x component" &
                                           //" of the inputted 2d domain differs" &
                                           //" from the 1d domain associated with" &
                                           //" the x axis of the inputted field.")
                    endif
                    foundx = .true._c_bool
                else
                    call mpp_error(FATAL, &
                                   "mpp_write_2ddecomp_r4d_: the inputted field" &
                                       //" has more than one x axis.")
                endif
            elseif (field%axes(i)%cartesian .eq. "Y") then
                if (.not. foundy) then
                    if (field%axes(i)%domain .ne. ydomain) then
                        call mpp_error(FATAL, &
                                       "mpp_write_2ddecomp_r4d_: the y component" &
                                           //" of the inputted 2d domain differs" &
                                           //" from the 1d domain associated with" &
                                           //" the y axis of the inputted field.")
                    endif
                    foundy = .true._c_bool
                else
                    call mpp_error(FATAL, &
                                   "mpp_write_2ddecomp_r4d_: the inputted field" &
                                       //" has more than one y axis.")
                endif
            endif
        enddo
        if (.not. (foundx .and. foundy)) then
            call mpp_error(FATAL, &
                           "mpp_write_2ddecomp_r4d_: the inputted field" &
                               //" is missing either an x or y axis (or both).")
        endif
    endif

    !Get the 2d domain position of the field and different domain starting
    !and ending indices.
    dposition = field%position
    call mpp_get_compute_domain(domain, &
                                is, &
                                ie, &
                                js, &
                                je, &
                                tile_count=tile_count, &
                                position=dposition)
    call mpp_get_data_domain(domain, &
                             isd, &
                             ied, &
                             jsd, &
                             jed, &
                             x_is_global=x_is_global, &
                             y_is_global=y_is_global, &
                             tile_count=tile_count, &
                             position=dposition)
    call mpp_get_memory_domain(domain, &
                               ism, &
                               iem, &
                               jsm, &
                               jem, &
                               position=dposition)

    !Figure out if the inputted data contains halos.
    if (size(var_data,1) .eq. ie-is+1 .and. size(var_data,2) .eq. &
            je-js+1) then
        data_has_halos = .false._c_bool
    elseif (size(var_data,1) .eq. iem-ism+1 .and. size(var_data,2) .eq. &
                jem-jsm+1) then
        data_has_halos = .true._c_bool
    else
        write(error_unit,'(a,10i5)') "mpp_write_2ddecomp fails on field " &
                                         //trim(field%name) &
                                         //": is,ie,js,je, ism,iem,jsm,jem," &
                                         //" size(data,1), size(data,2)=", &
                                     is, &
                                     ie, &
                                     js, &
                                     je, &
                                     ism, &
                                     iem, &
                                     jsm, &
                                     jem, &
                                     size(var_data,1), &
                                     size(var_data,2)
        call mpp_error(FATAL, &
                       "mpp_write: data must be either on compute domain" &
                           //" or data domain.")
    endif

    !Figure out if the halos are global.
    halos_are_global = x_is_global .and. y_is_global

    if (context%files(file_index)%comm_required .or. &
            associated(context%files(file_index)%io_domain)) then

        !Here, communication between ranks is necessary because either a
        !domain update is needed, or the writing rank must write data it
        !does not own.

        if (halos_are_global) then

            !Perform the inter-rank communication.
            call mpp_update_domains(var_data, &
                                    domain, &
                                    position=dposition)

            if (context%files(file_index)%is_writer) then

                !Write the data to the file.
                call mpp_write_r4d(file_index, &
                                   field, &
                                   var_data, &
                                   unlim_dim_val)
            endif

        else
            domain_ptr => null()

            if (associated(context%files(file_index)%io_domain) .and. &
                    context%files(file_index)%data_in_multi_files) then
                domain_ptr => context%files(file_index)%io_domain
            else
                domain_ptr => domain
            endif

            !Get the field data from the domain pointed to by domain_ptr.
            call mpp_get_global_domain(domain_ptr, &
                                       isg, &
                                       ieg, &
                                       jsg, &
                                       jeg, &
                                       tile_count=tile_count, &
                                       position=dposition)

            if (context%files(file_index)%is_writer .or. .not. &
                    global_field_on_root_pe) then
                allocate(gdata(isg:ieg,jsg:jeg,size(var_data,3),size(var_data,4)))
            else
                allocate(gdata(1,1,1,1))
            endif

            if (global_field_on_root_pe) then
                call mpp_global_field(domain_ptr, &
                                      var_data, &
                                      gdata, &
                                      position=dposition, &
                                      flags=XUPDATE+YUPDATE+GLOBAL_ROOT_ONLY, &
                                      default_data=default_data)
            else
                call mpp_global_field(domain_ptr, &
                                      var_data, &
                                      gdata, &
                                      position=dposition, &
                                      default_data=default_data)
            endif

            if (context%files(file_index)%is_writer) then

                !Write out the data.
                call mpp_write_r4d(file_index, &
                                   field, &
                                   gdata, &
                                   unlim_dim_val)
            endif
            deallocate(gdata)
            domain_ptr => null()
        endif

    else

        if (.not. context%files(file_index)%is_writer) then
            call mpp_error(FATAL, &
                           "mpp_write_2ddecomp_r4d_: this branch should" &
                               //" be reached.")
        endif

        !Here, no domain updated is needed and inter-rank communication is
        !not required because the writing rank is only writing data it owns.

        if (data_has_halos) then

            !First copy the data into a contiguous buffer and then write it
            !out.
            allocate(cdata(is:ie,js:je,size(var_data,3),size(var_data,4)))
            cdata(:,:,:,:) = var_data(is-isd+1:ie-isd+1,js-jsd+1:je-jsd+1,:,:)
            call mpp_write_r4d(file_index, &
                               field, &
                               cdata, &
                               unlim_dim_val)
            deallocate(cdata)
        else

            !The data is already contiguous, so just write it out.
            call mpp_write_r4d(file_index, &
                               field, &
                               var_data, &
                               unlim_dim_val)
        endif
    endif

end subroutine mpp_write_2ddecomp_r4d_
