 !*****************************************************************************
 !Public facing mpp routines that can maybe be moved to fms.

 !-----------------------------------------------------------------------------
 !>Return a lowercase version of a string.
  function lowercase(cs) result(cs_lowercase)

   !<Inputs/Outputs
    character(len=*),intent(in)   :: cs
    character(len=len(cs)),target :: cs_lowercase

   !<Local variables
    integer(INT_KIND),parameter :: co=iachar('a')-iachar('A')
    integer(INT_KIND)           :: tlen
    integer(INT_KIND)           :: k
    character,pointer           :: ca

    tlen = len_trim(cs)
    if (tlen .le. 0) then
        cs_lowercase = cs
    else
        cs_lowercase = cs(1:tlen)
        do k = 1,tlen
            ca => cs_lowercase(k:k)
            if (ca .ge. 'A' .and. ca .le. 'Z') then
                ca = achar(ichar(ca)+co)
            endif
        enddo
    endif

  end function lowercase

 !-----------------------------------------------------------------------------
 !>Return an uppercase version of a string.
  function uppercase(cs) result(cs_uppercase)

   !<Inputs/Outputs
    character(len=*),intent(in)   :: cs
    character(len=len(cs)),target :: cs_uppercase

   !<Local variables
    integer(INT_KIND),parameter :: co=iachar('A')-iachar('a')
    integer(INT_KIND)           :: tlen
    integer(INT_KIND)           :: k
    character,pointer           :: ca

    tlen = len_trim(cs)
    if (tlen .le. 0) then
        cs_uppercase = cs
    else
        cs_uppercase = cs(1:tlen)
        do k = 1,tlen
            ca => cs_uppercase(k:k)
            if (ca .ge. 'a' .and. ca .le. 'z') then
                ca = achar(ichar(ca)+co)
            endif
        enddo
    endif

  end function uppercase

 !-----------------------------------------------------------------------------
 !>Get an available Fortran file unit. Currently not thread-safe!
  function get_unit() result(file_unit)

   !<Inputs/Outputs
    integer(INT_KIND) :: file_unit

   !<Local variables
    integer(INT_KIND),save :: i
    logical(INT_KIND)      :: l_open

    do i = 10,99
        inquire(unit=i,opened=l_open)
        if (.not. l_open) then
            exit
        endif
    enddo

    if (i .eq. 100) then
        call mpp_error(FATAL,'GET_UNIT: Unable to get an I/O unit')
    else
        file_unit = i
    endif

  end function get_unit

 !-----------------------------------------------------------------------------
 !>Read the contents of an ascii file into a character array.
  subroutine read_ascii_file(filename,length,content,pelist)

   !>Inputs/Outputs
    character(len=*),intent(in)                        :: filename
    integer(INT_KIND),intent(in)                       :: length
    character(len=*),dimension(:),intent(inout)        :: content
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist

   !>Local variables
    character(len=*),parameter :: version = '$Id$'
    character(len=*),parameter :: tagname = '$Name$'
    character(len=5)  :: text
    logical(INT_KIND) :: file_exist
    integer(INT_KIND) :: fstatus
    integer(INT_KIND) :: f_unit
    integer(INT_KIND) :: log_unit
    integer(INT_KIND) :: from_pe
    integer(INT_KIND) :: pnum_lines
    integer(INT_KIND) :: num_lines

    if (.not. read_ascii_file_on) then
        call mpp_error(FATAL,'READ_ASCII_FILE: get_ascii_file_num_lines ' &
                             //' needs to be called before calling' &
                             //' read_ascii_file')
    endif
    read_ascii_file_on = .false.        

    from_pe = mpp_root_pe()
    num_lines = size(content(:))

    if (mpp_pe() .eq. mpp_root_pe()) then
        log_unit = stdlog()
        write(log_unit,'(a)') '===========================================' &
                              //'============================='
        write(log_unit,'(a)') 'READ_ASCII_FILE: '//trim(version)//':'// &
                               trim(tagname)
        write(log_unit,'(a)') 'READ_ASCII_FILE: File: '//trim(filename)
        inquire(file=filename,exist=file_exist)
        if (file_exist) then
            f_unit = get_unit()
            open(unit=f_unit,file=filename,action='read',status='old', &
                 iostat=fstatus)
            if (fstatus .ne. 0) then
                write(unit=text,fmt='(i5)') fstatus
                call mpp_error(FATAL,'READ_ASCII_FILE: error opening file: ' &
                               //trim(FILENAME)//'.  (iostat = ' &
                               //trim(text)//')')
            else
                if (num_lines .gt. 0) then
                    content(:) = ' '
                    rewind(unit=f_unit,iostat=fstatus)
                    if (fstatus .ne. 0) then
                        write(unit=text,fmt='(i5)') fstatus
                        call mpp_error(FATAL,'READ_ASCII_FILE: unable to' &
                                             //' re-read file '&
                                             //trim(filename)//'. (iostat = ' &
                                             //trim(text)//').')
                    else
                        pnum_lines = 1
                        do
                            read(unit=f_unit,fmt='(a)',iostat=fstatus) content(pnum_lines)
                            if (fstatus .lt. 0) then
                                exit
                            endif
                            if (fstatus .gt. 0 ) then
                                write(unit=text,fmt='(i5)') pnum_lines
                                call mpp_error(FATAL,'READ_ASCII_FILE: error' &
                                                     //' reading line ' &
                                                     //trim(text)//' in file' &
                                                     //' '//trim(filename) &
                                                     //'.')
                            endif
                            if (pnum_lines .gt. num_lines) then
                                call mpp_error(FATAL,'READ_ASCII_FILE: number' &
                                                     //' of lines in file ' &
                                                     //trim(FILENAME)// &
                                                     ' is greater than '&
                                                     //' size(Content(:)). ')
                            endif
                            if (len_trim(Content(pnum_lines)) .eq. length) then
                                write(unit=text,fmt='(i5)') length
                                call mpp_error(FATAL,'READ_ASCII_FILE: Length' &
                                                     //' of output string (' &
                                                     //trim(text)//' is too' &
                                                     //' small. Increase the' &
                                                     //' length value.')
                            endif
                            pnum_lines = pnum_lines + 1
                        enddo
                        if (num_lines .ne. pnum_lines) then
                            call mpp_error(FATAL,'READ_ASCII_FILE: number of' &
                                                 //' lines in file ' &
                                                 //trim(filename)// &
                                                 ' does not equal to' &
                                                 //' size(content(:)) ' )
                        endif
                    endif
                endif
                close(unit=f_unit)
            endif
        else
            call mpp_error(FATAL,'READ_ASCII_FILE: File '//trim(FILENAME)// &
                                 ' does not exist.')
        endif
    endif

   !<Broadcast the character array
    call mpp_broadcast(content,size(content),from_pe,pelist=pelist)

  end subroutine read_ascii_file

 !-----------------------------------------------------------------------------
 !>Return the number of lines in an ascii file.
  function get_ascii_file_num_lines(filename,length,pelist) result(tot_num_lines)

   !>Inputs/Outputs
    character(len=*),intent(in)                        :: filename
    integer(INT_KIND),intent(in)                       :: length
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist
    integer(INT_KIND)                                  :: tot_num_lines

   !>Local variables.
    integer(INT_KIND)     :: num_lines
    character(len=length) :: str_tmp
    character(len=5)      :: text
    integer(INT_KIND)     :: fstatus
    integer(INT_KIND)     :: f_unit
    integer(INT_KIND)     :: from_pe
    logical(INT_KIND)     :: file_exist

    if (read_ascii_file_on) then
        call mpp_error(FATAL,'mpp_util.inc: get_ascii_file_num_lines is' &
                             //' called again before calling read_ascii_file')
    endif
    read_ascii_file_on = .true.

    from_pe = mpp_root_pe()
    tot_num_lines = -1
    num_lines = -1
    if (mpp_pe() .eq. mpp_root_pe()) then
        inquire(file=filename,exist=file_exist)
        if (file_exist) then
            f_unit = get_unit()
            open(unit=f_unit,file=filename,action='read',status='old', &
                 iostat=fstatus)

            if (fstatus .ne. 0) then
                write (unit=text,fmt='(i5)') fstatus
                call mpp_error(FATAL,'GET_ASCII_FILE_NUM_LINES: error' &
                                     //' opening file:'//trim(filename)// &
                                     '.  (iostat = '//trim(text)//')')
            else
                num_lines = 1
                do
                    read (unit=f_unit,fmt='(a)',iostat=fstatus) str_tmp
                    if (fstatus .lt. 0) then
                        exit
                    endif
                    if (fstatus .gt. 0) then
                        write (unit=text, fmt='(i5)') num_lines
                        call mpp_error(FATAL,'GET_ASCII_FILE_NUM_LINES: ' &
                                             //'error reading line ' &
                                             //trim(text)//' in file ' &
                                             //trim(filename)//'.')
                    endif
                    if (len_trim(str_tmp) == length) then
                        write(unit=text,fmt='(i5)') length
                        call mpp_error(FATAL,'GET_ASCII_FILE_NUM_LINES: '  &
                                             //' length of output string ' &
                                             //trim(text)//' is too small.' &
                                             //' Increase the length value.')
                    endif
                    num_lines = num_lines + 1
                enddo
                close(unit=f_unit)
            endif
        else
            call mpp_error(FATAL,'GET_ASCII_FILE_NUM_LINES: file ' &
                                  //trim(filename)//' does not exist.')
        endif
    endif

   !<Broadcast the number of lines
    call mpp_broadcast(num_lines,1,from_pe,pelist=pelist)
    tot_num_lines = num_lines

  end function get_ascii_file_num_lines

 !-----------------------------------------------------------------------------

 !*****************************************************************************
