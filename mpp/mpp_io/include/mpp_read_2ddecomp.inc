!Read in data that has been decomposed across a domain.
subroutine mpp_read_2ddecomp_r2d(unit, &
                                 field, &
                                 domain, &
                                 data, &
                                 tindex, &
                                 tile_count)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    type(domain2D),intent(in) :: domain
    real(kind=c_double),dimension(:,:),intent(inout) :: data
    integer(kind=c_int),intent(in),optional :: tindex
    integer(kind=c_int),intent(in),optional :: tile_count

    call openmp_thread_trap()
    call mpp_read_2ddecomp_r2d_(context, &
                                unit, &
                                field, &
                                domain, &
                                data, &
                                tindex, &
                                tile_count)

end subroutine mpp_read_2ddecomp_r2d

subroutine mpp_read_2ddecomp_r3d(unit, &
                                 field, &
                                 domain, &
                                 data, &
                                 tindex, &
                                 tile_count)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    type(domain2D),intent(in) :: domain
    real(kind=c_double),dimension(:,:,:),intent(inout) :: data
    integer(kind=c_int),intent(in),optional :: tindex
    integer(kind=c_int),intent(in),optional :: tile_count

    call openmp_thread_trap()
    call mpp_read_2ddecomp_r3d_(context, &
                                unit, &
                                field, &
                                domain, &
                                data, &
                                tindex, &
                                tile_count)

end subroutine mpp_read_2ddecomp_r3d

subroutine mpp_read_2ddecomp_r4d(unit, &
                                 field, &
                                 domain, &
                                 data, &
                                 tindex, &
                                 tile_count)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    type(domain2D),intent(in) :: domain
    real(kind=c_double),dimension(:,:,:,:),intent(inout) :: data
    integer(kind=c_int),intent(in),optional :: tindex
    integer(kind=c_int),intent(in),optional :: tile_count

    call openmp_thread_trap()
    call mpp_read_2ddecomp_r4d_(context, &
                                unit, &
                                field, &
                                domain, &
                                data, &
                                tindex, &
                                tile_count)

end subroutine mpp_read_2ddecomp_r4d

subroutine mpp_read_2ddecomp_r2d_(context, &
                                  file_index, &
                                  field, &
                                  domain, &
                                  var_data, &
                                  unlim_dim_level, &
                                  tile_count)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(fieldtype),intent(in) :: field
    type(domain2D),intent(in) :: domain
    real(kind=c_double),dimension(:,:),intent(inout) :: var_data
    integer(kind=c_int),intent(in),optional :: unlim_dim_level
    integer(kind=c_int),intent(in),optional :: tile_count

    !Local variables
    integer(kind=c_int) :: f
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
    integer(kind=c_int) :: isg
    integer(kind=c_int) :: ieg
    integer(kind=c_int) :: jsg
    integer(kind=c_int) :: jeg
    integer(kind=c_int) :: dposition
    logical(kind=c_bool) :: data_has_halos
    logical(kind=c_bool) :: halos_are_global
    real(kind=c_double),dimension(:,:),allocatable :: gdata
    integer(kind=c_int) :: ioff
    integer(kind=c_int) :: joff
    integer(kind=c_int) :: j
    integer(kind=c_int) :: i
    real(kind=c_double),dimension(:,:),allocatable :: cdata
    integer(kind=c_int),dimension(:),allocatable :: corner_indices
    integer(kind=c_int),dimension(:),allocatable :: edge_lengths
    integer(kind=c_int) :: xs
    integer(kind=c_int) :: xe
    integer(kind=c_int) :: ys
    integer(kind=c_int) :: ye
    integer(kind=c_int) :: xsg
    integer(kind=c_int) :: xeg
    integer(kind=c_int) :: ysg
    integer(kind=c_int) :: yeg
    integer(kind=c_size_t),dimension(2) :: data_sizes
    type(domain2d),pointer :: io_domain

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)
    call domain2d_check(context, &
                        file_index, &
                        domain)

    !Make sure that an unlimited dimension level is passed in if the
    !variable depends on an unlimited dimension.
    if (.not. present(unlim_dim_level) .and. field%time_axis_index &
            .ne. -1) then
        call mpp_error(FATAL, &
                       "mpp_read_2ddecomp_r2d_: if the variable depends" &
                           //" on an unlimited dimension, then a level" &
                           //" at which the data will be read must also" &
                           //" be passed in.")
    endif

    !Get domain indices.
    call mpp_get_compute_domain(domain, &
                                is, &
                                ie, &
                                js, &
                                je, &
                                tile_count=tile_count)
    call mpp_get_data_domain(domain, &
                             isd, &
                             ied, &
                             jsd, &
                             jed, &
                             x_is_global=x_is_global, &
                             y_is_global=y_is_global, &
                             tile_count=tile_count)
    call mpp_get_memory_domain(domain, &
                               ism, &
                               iem, &
                               jsm, &
                               jem)
    call mpp_get_global_domain(domain, &
                               isg, &
                               ieg, &
                               jsg, &
                               jeg, &
                               tile_count=tile_count)

    !When the domain is symmetry, extra point is needed for some data on
    !x/y direction.
    dposition = CENTER
    if (mpp_domain_is_symmetry(domain)) then
        if (size(var_data,1) .eq. ie-is+1 .and. size(var_data,2) .eq. &
                je-js+1) then
            data_has_halos = .false.
        elseif (size(var_data,1) .eq. ie-is+2 .and. size(var_data,2) .eq. &
                    je-js+1) then
            data_has_halos = .false.
            dposition = EAST
            ie = ie + 1
        elseif (size(var_data,1) .eq. ie-is+1 .and. size(var_data,2) .eq. &
                    je-js+2) then
            dposition = NORTH
            data_has_halos = .false.
            je = je + 1
        elseif (size(var_data,1) .eq. ie-is+2 .and. size(var_data,2) .eq. &
                    je-js+2) then
            dposition = CORNER
            data_has_halos = .false.
            ie = ie + 1
            je = je + 1
        elseif (size(var_data,1) .eq. iem-ism+1 .and. size(var_data,2) .eq. &
                    jem-jsm+1) then
            data_has_halos = .true.
        elseif (size(var_data,1) .eq. iem-ism+2 .and. size(var_data,2) .eq. &
                    jem-jsm+1) then
            dposition = EAST
            data_has_halos = .true.
            ie = ie + 1
        elseif (size(var_data,1) .eq. iem-ism+1 .and. size(var_data,2) .eq. &
                    jem-jsm+2) then
            dposition = NORTH
            data_has_halos = .true.
            je = je + 1
        elseif (size(var_data,1) .eq. iem-ism+2 .and. size(var_data,2) .eq. &
                    jem-jsm+2) then
            dposition = CORNER
            data_has_halos = .true.
            ie = ie + 1
            je = je + 1
        else
            call mpp_error(FATAL, &
                           "mpp_read_2ddecomp_r2d_: when domain is symmetry," &
                               //" data must be either on compute domain or" &
                               //" data domain with the consideration of" &
                               //" shifting.")
        endif
    else
        if (size(var_data,1) .eq. ie-is+1 .and. size(var_data,2) .eq. &
                je-js+1) then
            data_has_halos = .false.
        elseif (size(var_data,1) .eq. iem-ism+1 .and. size(var_data,2) .eq. &
                    jem-jsm+1) then
            data_has_halos = .true.
        else
            call mpp_error(FATAL, &
                           "mpp_read_2ddecomp_r2d_: data must be either on" &
                               //" compute domain or data domain.")
        endif
    endif

    !Check if the halos are global.
    halos_are_global = x_is_global .and. y_is_global

    if (context%files(file_index)%comm_required) then

        !Here, communication between ranks is necessary because the reading
        !rank must read in data it does not own.

        if (halos_are_global) then
            if (context%files(file_index)%is_reader) then
                call mpp_read_r2d(file_index, &
                                  field, &
                                  var_data, &
                                  unlim_dim_level)
            endif
        else

            !Allocate a contiguous buffer.
            allocate(gdata(is:ie,js:je))

            !Read in the data.
            if (context%files(file_index)%is_reader) then
                call mpp_read_r2d(file_index, &
                                  field, &
                                  gdata, &
                                  unlim_dim_level)
            endif

            !Broadcast the data.
            if (allocated(context%files(file_index)%rank_list)) then
                call mpp_broadcast(gdata, &
                                   size(gdata), &
                                   context%files(file_index)%rank_list_root, &
                                   context%files(file_index)%rank_list)
            endif

            !Set dimension offsets.
            if (data_has_halos) then
                ioff = isd
                joff = jsd
            else
                ioff = is
                joff = js
            endif

            !Copy the read in data from the contiguous buffer.
            do j = js,je
                do i = is,ie
                    var_data(i-ioff+1,j-joff+1) = gdata(i-isg+1,j-jsg+1)
                enddo
            enddo
            deallocate(gdata)
        endif
    else

        if (.not. context%files(file_index)%is_reader) then
            call mpp_error(FATAL, &
                           "mpp_read_2ddecomp_r2d_: this branch should" &
                               //" be reached.")
        endif

        !Determine the corner indices and edge lengths for the read.
        allocate(corner_indices(size(field%axes)))
        allocate(edge_lengths(size(field%axes)))
        corner_indices = 1
        do i = 1,size(field%axes)
            edge_lengths(i) = field%size(i)
            if (i .eq. field%time_axis_index) then
                corner_indices(i) = unlim_dim_level
            endif
        enddo

        call mpp_get_compute_domain(domain, &
                                    xs, &
                                    xe, &
                                    ys, &
                                    ye, &
                                    tile_count=tile_count, &
                                    position=dposition)
        call mpp_get_global_domain(domain, &
                                   xsg, &
                                   xeg, &
                                   ysg, &
                                   yeg, &
                                   tile_count=tile_count, &
                                   position=dposition)
        edge_lengths(1) = xe - xs + 1
        edge_lengths(2) = ye - ys + 1

!       io_domain => mpp_get_io_domain(domain)
        io_domain => context%files(file_index)%io_domain
        if (associated(io_domain)) then
            call mpp_get_compute_domain(io_domain, &
                                        xs, &
                                        xe, &
                                        ys, &
                                        ye, &
                                        tile_count=tile_count, &
                                        position=dposition)
            call mpp_get_global_domain(io_domain, &
                                       xsg, &
                                       xeg, &
                                       ysg, &
                                       yeg, &
                                       tile_count=tile_count, &
                                       position=dposition)
            corner_indices(1) = xs - xsg + 1
            corner_indices(2) = ys - ysg + 1

        elseif (.not. context%files(file_index)%data_in_multi_files) then
            if (context%files(file_index)%num_ranks_at_init .gt. 1) then
                corner_indices(1) = xs - xsg + 1
                corner_indices(2) = ys - ysg + 1
            else
                if (xe-xs+1 .ne. xeg-xsg+1) then
                    corner_indices(1) = xs - xsg + 1
                    edge_lengths(1) = xe - xs + 1
                endif
                if (ye-ys+1 .ne. yeg-ysg+1) then
                    corner_indices(2) = ys - ysg + 1
                    edge_lengths(2) = ye - ys + 1
                endif
            endif
        endif

        if (data_has_halos) then
            allocate(cdata(is:ie,js:je))
            data_sizes = shape(cdata,kind=c_size_t)
            call mpp_read_r_array(context, &
                                  file_index, &
                                  field, &
                                  cdata, &
                                  data_sizes, &
                                  .false._c_bool, &
                                  unlim_dim_level, &
                                  corner_indices, &
                                  edge_lengths)
            var_data(is-isd+1:ie-isd+1,js-jsd+1:je-jsd+1) = cdata(:,:)
            deallocate(cdata)
        else
            data_sizes = shape(var_data,kind=c_size_t)
            call mpp_read_r_array(context, &
                                  file_index, &
                                  field, &
                                  var_data, &
                                  data_sizes, &
                                  .false._c_bool, &
                                  unlim_dim_level, &
                                  corner_indices, &
                                  edge_lengths)
        endif
        deallocate(corner_indices)
        deallocate(edge_lengths)
    endif

end subroutine mpp_read_2ddecomp_r2d_

subroutine mpp_read_2ddecomp_r3d_(context, &
                                  file_index, &
                                  field, &
                                  domain, &
                                  var_data, &
                                  unlim_dim_level, &
                                  tile_count)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(fieldtype),intent(in) :: field
    type(domain2D),intent(in) :: domain
    real(kind=c_double),dimension(:,:,:),intent(inout) :: var_data
    integer(kind=c_int),intent(in),optional :: unlim_dim_level
    integer(kind=c_int),intent(in),optional :: tile_count

    !Local variables
    integer(kind=c_int) :: f
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
    integer(kind=c_int) :: isg
    integer(kind=c_int) :: ieg
    integer(kind=c_int) :: jsg
    integer(kind=c_int) :: jeg
    integer(kind=c_int) :: dposition
    logical(kind=c_bool) :: data_has_halos
    logical(kind=c_bool) :: halos_are_global
    real(kind=c_double),dimension(:,:,:),allocatable :: gdata
    integer(kind=c_int) :: ioff
    integer(kind=c_int) :: joff
    integer(kind=c_int) :: k
    integer(kind=c_int) :: j
    integer(kind=c_int) :: i
    real(kind=c_double),dimension(:,:,:),allocatable :: cdata
    integer(kind=c_int),dimension(:),allocatable :: corner_indices
    integer(kind=c_int),dimension(:),allocatable :: edge_lengths
    integer(kind=c_int) :: xs
    integer(kind=c_int) :: xe
    integer(kind=c_int) :: ys
    integer(kind=c_int) :: ye
    integer(kind=c_int) :: xsg
    integer(kind=c_int) :: xeg
    integer(kind=c_int) :: ysg
    integer(kind=c_int) :: yeg
    integer(kind=c_size_t),dimension(3) :: data_sizes
    type(domain2d),pointer :: io_domain

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)
    call domain2d_check(context, &
                        file_index, &
                        domain)

    !Make sure that an unlimited dimension level is passed in if the
    !variable depends on an unlimited dimension.
    if (.not. present(unlim_dim_level) .and. field%time_axis_index &
            .ne. -1) then
        call mpp_error(FATAL, &
                       "mpp_read_2ddecomp_r3d_: if the variable depends" &
                           //" on an unlimited dimension, then a level" &
                           //" at which the data will be read must also" &
                           //" be passed in.")
    endif

    !Get domain indices.
    call mpp_get_compute_domain(domain, &
                                is, &
                                ie, &
                                js, &
                                je, &
                                tile_count=tile_count)
    call mpp_get_data_domain(domain, &
                             isd, &
                             ied, &
                             jsd, &
                             jed, &
                             x_is_global=x_is_global, &
                             y_is_global=y_is_global, &
                             tile_count=tile_count)
    call mpp_get_memory_domain(domain, &
                               ism, &
                               iem, &
                               jsm, &
                               jem)
    call mpp_get_global_domain(domain, &
                               isg, &
                               ieg, &
                               jsg, &
                               jeg, &
                               tile_count=tile_count)

    !When the domain is symmetry, extra point is needed for some data on
    !x/y direction.
    dposition = CENTER
    if (mpp_domain_is_symmetry(domain)) then
        if (size(var_data,1) .eq. ie-is+1 .and. size(var_data,2) .eq. &
                je-js+1) then
            data_has_halos = .false.
        elseif (size(var_data,1) .eq. ie-is+2 .and. size(var_data,2) .eq. &
                    je-js+1) then
            data_has_halos = .false.
            dposition = EAST
            ie = ie + 1
        elseif (size(var_data,1) .eq. ie-is+1 .and. size(var_data,2) .eq. &
                    je-js+2) then
            dposition = NORTH
            data_has_halos = .false.
            je = je + 1
        elseif (size(var_data,1) .eq. ie-is+2 .and. size(var_data,2) .eq. &
                    je-js+2) then
            dposition = CORNER
            data_has_halos = .false.
            ie = ie + 1
            je = je + 1
        elseif (size(var_data,1) .eq. iem-ism+1 .and. size(var_data,2) .eq. &
                    jem-jsm+1) then
            data_has_halos = .true.
        elseif (size(var_data,1) .eq. iem-ism+2 .and. size(var_data,2) .eq. &
                    jem-jsm+1) then
            dposition = EAST
            data_has_halos = .true.
            ie = ie + 1
        elseif (size(var_data,1) .eq. iem-ism+1 .and. size(var_data,2) .eq. &
                    jem-jsm+2) then
            dposition = NORTH
            data_has_halos = .true.
            je = je + 1
        elseif (size(var_data,1) .eq. iem-ism+2 .and. size(var_data,2) .eq. &
                    jem-jsm+2) then
            dposition = CORNER
            data_has_halos = .true.
            ie = ie + 1
            je = je + 1
        else
            call mpp_error(FATAL, &
                           "mpp_read_2ddecomp_r3d_: when domain is symmetry," &
                               //" data must be either on compute domain or" &
                               //" data domain with the consideration of" &
                               //" shifting.")
        endif
    else
        if (size(var_data,1) .eq. ie-is+1 .and. size(var_data,2) .eq. &
                je-js+1) then
            data_has_halos = .false.
        elseif (size(var_data,1) .eq. iem-ism+1 .and. size(var_data,2) .eq. &
                    jem-jsm+1) then
            data_has_halos = .true.
        else
            call mpp_error(FATAL, &
                           "mpp_read_2ddecomp_r3d_: data must be either on" &
                               //" compute domain or data domain.")
        endif
    endif

    !Check if the halos are global.
    halos_are_global = x_is_global .and. y_is_global

    if (context%files(file_index)%comm_required) then

        !Here, communication between ranks is necessary because the reading
        !rank must read in data it does not own.

        if (halos_are_global) then
            if (context%files(file_index)%is_reader) then
                call mpp_read_r3d(file_index, &
                                  field, &
                                  var_data, &
                                  unlim_dim_level)
            endif
        else

            !Allocate a contiguous buffer.
            allocate(gdata(is:ie,js:je,size(var_data,3)))

            !Read in the data.
            if (context%files(file_index)%is_reader) then
                call mpp_read_r3d(file_index, &
                                  field, &
                                  gdata, &
                                  unlim_dim_level)
            endif

            !Broadcast the data.
            if (allocated(context%files(file_index)%rank_list)) then
                call mpp_broadcast(gdata, &
                                   size(gdata), &
                                   context%files(file_index)%rank_list_root, &
                                   context%files(file_index)%rank_list)
            endif

            !Set dimension offsets.
            if (data_has_halos) then
                ioff = isd
                joff = jsd
            else
                ioff = is
                joff = js
            endif

            !Copy the read in data from the contiguous buffer.
            do k = 1,size(var_data,3)
                do j = js,je
                    do i = is,ie
                        var_data(i-ioff+1,j-joff+1,k) = gdata(i-isg+1,j-jsg+1,k)
                    enddo
                enddo
            enddo
            deallocate(gdata)
        endif
    else

        if (.not. context%files(file_index)%is_reader) then
            call mpp_error(FATAL, &
                           "mpp_read_2ddecomp_r3d_: this branch should" &
                               //" be reached.")
        endif

        !Determine the corner indices and edge lengths for the read.
        allocate(corner_indices(size(field%axes)))
        allocate(edge_lengths(size(field%axes)))
        corner_indices = 1
        do i = 1,size(field%axes)
            edge_lengths(i) = field%size(i)
            if (i .eq. field%time_axis_index) then
                corner_indices(i) = unlim_dim_level
            endif
        enddo

        call mpp_get_compute_domain(domain, &
                                    xs, &
                                    xe, &
                                    ys, &
                                    ye, &
                                    tile_count=tile_count, &
                                    position=dposition)
        call mpp_get_global_domain(domain, &
                                   xsg, &
                                   xeg, &
                                   ysg, &
                                   yeg, &
                                   tile_count=tile_count, &
                                   position=dposition)
        edge_lengths(1) = xe - xs + 1
        edge_lengths(2) = ye - ys + 1

!       io_domain => mpp_get_io_domain(domain)
        io_domain => context%files(file_index)%io_domain
        if (associated(io_domain)) then
            call mpp_get_compute_domain(io_domain, &
                                        xs, &
                                        xe, &
                                        ys, &
                                        ye, &
                                        tile_count=tile_count, &
                                        position=dposition)
            call mpp_get_global_domain(io_domain, &
                                       xsg, &
                                       xeg, &
                                       ysg, &
                                       yeg, &
                                       tile_count=tile_count, &
                                       position=dposition)
            corner_indices(1) = xs - xsg + 1
            corner_indices(2) = ys - ysg + 1

        elseif (.not. context%files(file_index)%data_in_multi_files) then
            if (context%files(file_index)%num_ranks_at_init .gt. 1) then
                corner_indices(1) = xs - xsg + 1
                corner_indices(2) = ys - ysg + 1
            else
                if (xe-xs+1 .ne. xeg-xsg+1) then
                    corner_indices(1) = xs - xsg + 1
                    edge_lengths(1) = xe - xs + 1
                endif
                if (ye-ys+1 .ne. yeg-ysg+1) then
                    corner_indices(2) = ys - ysg + 1
                    edge_lengths(2) = ye - ys + 1
                endif
            endif
        endif

        if (data_has_halos) then
            allocate(cdata(is:ie,js:je,size(var_data,3)))
            data_sizes = shape(cdata,kind=c_size_t)
            call mpp_read_r_array(context, &
                                  file_index, &
                                  field, &
                                  cdata, &
                                  data_sizes, &
                                  .false._c_bool, &
                                  unlim_dim_level, &
                                  corner_indices, &
                                  edge_lengths)
            var_data(is-isd+1:ie-isd+1,js-jsd+1:je-jsd+1,:) = cdata(:,:,:)
            deallocate(cdata)
        else
            data_sizes = shape(var_data,kind=c_size_t)
            call mpp_read_r_array(context, &
                                  file_index, &
                                  field, &
                                  var_data, &
                                  data_sizes, &
                                  .false._c_bool, &
                                  unlim_dim_level, &
                                  corner_indices, &
                                  edge_lengths)
        endif
        deallocate(corner_indices)
        deallocate(edge_lengths)
    endif

end subroutine mpp_read_2ddecomp_r3d_

subroutine mpp_read_2ddecomp_r4d_(context, &
                                  file_index, &
                                  field, &
                                  domain, &
                                  var_data, &
                                  unlim_dim_level, &
                                  tile_count)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(fieldtype),intent(in) :: field
    type(domain2D),intent(in) :: domain
    real(kind=c_double),dimension(:,:,:,:),intent(inout) :: var_data
    integer(kind=c_int),intent(in),optional :: unlim_dim_level
    integer(kind=c_int),intent(in),optional :: tile_count

    !Local variables
    integer(kind=c_int) :: f
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
    integer(kind=c_int) :: isg
    integer(kind=c_int) :: ieg
    integer(kind=c_int) :: jsg
    integer(kind=c_int) :: jeg
    integer(kind=c_int) :: dposition
    logical(kind=c_bool) :: data_has_halos
    logical(kind=c_bool) :: halos_are_global
    real(kind=c_double),dimension(:,:,:,:),allocatable :: gdata
    integer(kind=c_int) :: ioff
    integer(kind=c_int) :: joff
    integer(kind=c_int) :: p
    integer(kind=c_int) :: k
    integer(kind=c_int) :: j
    integer(kind=c_int) :: i
    real(kind=c_double),dimension(:,:,:,:),allocatable :: cdata
    integer(kind=c_int),dimension(:),allocatable :: corner_indices
    integer(kind=c_int),dimension(:),allocatable :: edge_lengths
    integer(kind=c_int) :: xs
    integer(kind=c_int) :: xe
    integer(kind=c_int) :: ys
    integer(kind=c_int) :: ye
    integer(kind=c_int) :: xsg
    integer(kind=c_int) :: xeg
    integer(kind=c_int) :: ysg
    integer(kind=c_int) :: yeg
    integer(kind=c_size_t),dimension(4) :: data_sizes
    type(domain2d),pointer :: io_domain

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)
    call domain2d_check(context, &
                        file_index, &
                        domain)

    !Make sure that an unlimited dimension level is passed in if the
    !variable depends on an unlimited dimension.
    if (.not. present(unlim_dim_level) .and. field%time_axis_index &
            .ne. -1) then
        call mpp_error(FATAL, &
                       "mpp_read_2ddecomp_r4d_: if the variable depends" &
                           //" on an unlimited dimension, then a level" &
                           //" at which the data will be read must also" &
                           //" be passed in.")
    endif

    !Get domain indices.
    call mpp_get_compute_domain(domain, &
                                is, &
                                ie, &
                                js, &
                                je, &
                                tile_count=tile_count)
    call mpp_get_data_domain(domain, &
                             isd, &
                             ied, &
                             jsd, &
                             jed, &
                             x_is_global=x_is_global, &
                             y_is_global=y_is_global, &
                             tile_count=tile_count)
    call mpp_get_memory_domain(domain, &
                               ism, &
                               iem, &
                               jsm, &
                               jem)
    call mpp_get_global_domain(domain, &
                               isg, &
                               ieg, &
                               jsg, &
                               jeg, &
                               tile_count=tile_count)

    !When the domain is symmetry, extra point is needed for some data on
    !x/y direction.
    dposition = CENTER
    if (mpp_domain_is_symmetry(domain)) then
        if (size(var_data,1) .eq. ie-is+1 .and. size(var_data,2) .eq. &
                je-js+1) then
            data_has_halos = .false.
        elseif (size(var_data,1) .eq. ie-is+2 .and. size(var_data,2) .eq. &
                    je-js+1) then
            data_has_halos = .false.
            dposition = EAST
            ie = ie + 1
        elseif (size(var_data,1) .eq. ie-is+1 .and. size(var_data,2) .eq. &
                    je-js+2) then
            dposition = NORTH
            data_has_halos = .false.
            je = je + 1
        elseif (size(var_data,1) .eq. ie-is+2 .and. size(var_data,2) .eq. &
                    je-js+2) then
            dposition = CORNER
            data_has_halos = .false.
            ie = ie + 1
            je = je + 1
        elseif (size(var_data,1) .eq. iem-ism+1 .and. size(var_data,2) .eq. &
                    jem-jsm+1) then
            data_has_halos = .true.
        elseif (size(var_data,1) .eq. iem-ism+2 .and. size(var_data,2) .eq. &
                    jem-jsm+1) then
            dposition = EAST
            data_has_halos = .true.
            ie = ie + 1
        elseif (size(var_data,1) .eq. iem-ism+1 .and. size(var_data,2) .eq. &
                    jem-jsm+2) then
            dposition = NORTH
            data_has_halos = .true.
            je = je + 1
        elseif (size(var_data,1) .eq. iem-ism+2 .and. size(var_data,2) .eq. &
                    jem-jsm+2) then
            dposition = CORNER
            data_has_halos = .true.
            ie = ie + 1
            je = je + 1
        else
            call mpp_error(FATAL, &
                           "mpp_read_2ddecomp_r4d_: when domain is symmetry," &
                               //" data must be either on compute domain or" &
                               //" data domain with the consideration of" &
                               //" shifting.")
        endif
    else
        if (size(var_data,1) .eq. ie-is+1 .and. size(var_data,2) .eq. &
                je-js+1) then
            data_has_halos = .false.
        elseif (size(var_data,1) .eq. iem-ism+1 .and. size(var_data,2) .eq. &
                    jem-jsm+1) then
            data_has_halos = .true.
        else
            call mpp_error(FATAL, &
                           "mpp_read_2ddecomp_r4d_: data must be either on" &
                               //" compute domain or data domain.")
        endif
    endif

    !Check if the halos are global.
    halos_are_global = x_is_global .and. y_is_global

    if (context%files(file_index)%comm_required) then

        !Here, communication between ranks is necessary because the reading
        !rank must read in data it does not own.

        if (halos_are_global) then
            if (context%files(file_index)%is_reader) then
                call mpp_read_r4d(file_index, &
                                  field, &
                                  var_data, &
                                  unlim_dim_level)
            endif
        else

            !Allocate a contiguous buffer.
            allocate(gdata(is:ie,js:je,size(var_data,3),size(var_data,4)))

            !Read in the data.
            if (context%files(file_index)%is_reader) then
                call mpp_read_r4d(file_index, &
                                  field, &
                                  gdata, &
                                  unlim_dim_level)
            endif

            !Broadcast the data.
            if (allocated(context%files(file_index)%rank_list)) then
                call mpp_broadcast(gdata, &
                                   size(gdata), &
                                   context%files(file_index)%rank_list_root, &
                                   context%files(file_index)%rank_list)
            endif

            !Set dimension offsets.
            if (data_has_halos) then
                ioff = isd
                joff = jsd
            else
                ioff = is
                joff = js
            endif

            !Copy the read in data from the contiguous buffer.
            do p = 1,size(var_data,4)
                do k = 1,size(var_data,3)
                    do j = js,je
                        do i = is,ie
                            var_data(i-ioff+1,j-joff+1,k,p) = gdata(i-isg+1,j-jsg+1,k,p)
                        enddo
                    enddo
                enddo
            enddo
            deallocate(gdata)
        endif
    else

        if (.not. context%files(file_index)%is_reader) then
            call mpp_error(FATAL, &
                           "mpp_read_2ddecomp_r4d_: this branch should" &
                               //" be reached.")
        endif

        !Determine the corner indices and edge lengths for the read.
        allocate(corner_indices(size(field%axes)))
        allocate(edge_lengths(size(field%axes)))
        corner_indices = 1
        do i = 1,size(field%axes)
            edge_lengths(i) = field%size(i)
            if (i .eq. field%time_axis_index) then
                corner_indices(i) = unlim_dim_level
            endif
        enddo

        call mpp_get_compute_domain(domain, &
                                    xs, &
                                    xe, &
                                    ys, &
                                    ye, &
                                    tile_count=tile_count, &
                                    position=dposition)
        call mpp_get_global_domain(domain, &
                                   xsg, &
                                   xeg, &
                                   ysg, &
                                   yeg, &
                                   tile_count=tile_count, &
                                   position=dposition)
        edge_lengths(1) = xe - xs + 1
        edge_lengths(2) = ye - ys + 1

!       io_domain => mpp_get_io_domain(domain)
        io_domain => context%files(file_index)%io_domain
        if (associated(io_domain)) then
            call mpp_get_compute_domain(io_domain, &
                                        xs, &
                                        xe, &
                                        ys, &
                                        ye, &
                                        tile_count=tile_count, &
                                        position=dposition)
            call mpp_get_global_domain(io_domain, &
                                       xsg, &
                                       xeg, &
                                       ysg, &
                                       yeg, &
                                       tile_count=tile_count, &
                                       position=dposition)
            corner_indices(1) = xs - xsg + 1
            corner_indices(2) = ys - ysg + 1

        elseif (.not. context%files(file_index)%data_in_multi_files) then
            if (context%files(file_index)%num_ranks_at_init .gt. 1) then
                corner_indices(1) = xs - xsg + 1
                corner_indices(2) = ys - ysg + 1
            else
                if (xe-xs+1 .ne. xeg-xsg+1) then
                    corner_indices(1) = xs - xsg + 1
                    edge_lengths(1) = xe - xs + 1
                endif
                if (ye-ys+1 .ne. yeg-ysg+1) then
                    corner_indices(2) = ys - ysg + 1
                    edge_lengths(2) = ye - ys + 1
                endif
            endif
        endif

        if (data_has_halos) then
            allocate(cdata(is:ie,js:je,size(var_data,3),size(var_data,4)))
            data_sizes = shape(cdata,kind=c_size_t)
            call mpp_read_r_array(context, &
                                  file_index, &
                                  field, &
                                  cdata, &
                                  data_sizes, &
                                  .false._c_bool, &
                                  unlim_dim_level, &
                                  corner_indices, &
                                  edge_lengths)
            var_data(is-isd+1:ie-isd+1,js-jsd+1:je-jsd+1,:,:) = cdata(:,:,:,:)
            deallocate(cdata)
        else
            data_sizes = shape(var_data,kind=c_size_t)
            call mpp_read_r_array(context, &
                                  file_index, &
                                  field, &
                                  var_data, &
                                  data_sizes, &
                                  .false._c_bool, &
                                  unlim_dim_level, &
                                  corner_indices, &
                                  edge_lengths)
        endif
        deallocate(corner_indices)
        deallocate(edge_lengths)
    endif

end subroutine mpp_read_2ddecomp_r4d_
