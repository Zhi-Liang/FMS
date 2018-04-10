module cjson_array_string_mod

use cjson_error_mod,    only: cjson_error_mesg,FATAL,WARNING,NOTE
use cjson_utils_mod,    only: string_is_num,cjson_array_length,strcase,bounds_check
use iso_c_binding

implicit none


 interface
        function string_val (cjson, json, nml_name , var_name) result (svalue) bind(C, name="string_val")
           use iso_c_binding
           type (c_ptr),intent(in),value :: cjson !< The parsed json
           character(kind=c_char) :: json     !< The json string
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
           character(kind=c_char) :: var_name !< the name of the variable
           type (c_ptr):: svalue  !< The value of the variable (pointer)
         end function
         function string_array (cjson, json, nml_name , var_name, arind) result (s)  bind(C, name="string_array")
           use iso_c_binding
           type (c_ptr),intent(in),value :: cjson !< The parsed json
           character(kind=c_char) :: json     !< The json string
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
           character(kind=c_char) :: var_name !< the name of the variable
           integer  (kind=c_int), VALUE :: arind !< The index of the array that is being inquiried
           type (c_ptr):: s  !< The value of the variable (pointer)
         end function
         subroutine C_free (cptr) bind(C, name="free") !> C-free function
           use iso_c_binding
           type (c_ptr),intent(in),value :: cptr
         end subroutine
 end interface

contains
!> \author Tom Robinson thomas.robinson@noaa.gov
!> \date September 2016
!! STRING_2D is called when the variable is a two-dimensional string array.  
subroutine string_2d (cjson,json,nmlname,varname,nmlval,ilowB,jlowB,st_size)
use iso_c_binding
 character (*),target,intent(inout)        :: nmlval (ilowB:,jlowB:) !< The variable 
 character (*),intent(in)               :: json	        !< String containing the contents of the json file
        type(C_PTR),intent(in)          :: cjson        !< A C pointer containing the parsed json
 character (*),intent(in)           	:: nmlname      !< The name of the namelist that the variable is in
 character (*),intent(in)           	:: varname      !< The name of the variable of interest
 integer,intent(in)                     :: st_size      !< Size of a string 
 integer, intent(in)                    :: ilowB, jlowB !< lower bounds of array
! integer                                :: lowB1, lowB2 !< \param lowB1,lowB2 lower bounds of array
 integer                                :: ar_size      !> \param ar_size The size of the array


 character (len=st_size), pointer	:: var (:,:)
 character (len=st_size), pointer       :: sp

 type (C_PTR)                                            :: C_st = C_NULL_PTR
 character(len=1,kind=c_char),pointer,dimension(:)       :: F_st => null()
 character(len=:),allocatable                            :: data_string

        integer                         :: nmllen       !> \param nmllen The length of the nmlname string 
        integer                         :: varlen       !> \param varlen The length of the varname string
        integer                         :: nmlloc       !> \param nmlloc The location of the namelist in the json file
        logical                         :: onenml       !> \param onenml Makes sure the namelist is only listed once
        logical                         :: nmlbracket   !> \param nmlbracket Inside the nml array
        logical                         :: varbracket   !> \param varbracket Inside a variable array

        character(len=:),allocatable    :: varcheck     !> \param varcheck The part of the array call in ()s
        character(len=:),allocatable    :: jvname       !> \param jvname varname in the JSON file to match case 
        character(len=:),allocatable    :: jnname       !> \param jnname nmlname in the JSON file to match case 
        logical                         :: colon        !> \param colon Set to true if a : is found in varcheck
        logical                         :: comma        !> \param comma Set to true if a , is found in varcheck
        logical                         :: secondcolon  !> \param secondcolon Are there two colons?
        integer                         :: n1,com1,com2,col1,col2
        integer                         :: colloc1,colloc2,comloc
        integer                         :: begin1,begin2,end1,end2
        integer                         :: js           !> \param js The size of the array in the JSON
 integer :: i,ic,k,ii,jj

 ar_size = size(nmlval,dim=1)*size(nmlval,dim=2)
   var => nmlval !> Point to the 2D array

 nmllen=len(trim (nmlname))
 varlen=len(trim (varname))
 allocate (character(len=varlen):: jvname)
 allocate (character(len=nmllen):: jnname)
 onenml=.false.
 nmlbracket=.false.
 varbracket=.false.
 colon = .false.
 comma = .false.
 secondcolon = .false.

   nmlloop: do k=1,len(json)-(nmllen+1) !> Loop through the json
        IF (strcase(json(k:k+nmllen+1)) == '"'//strcase(trim(nmlname))//'"') then !> Find the NMLname
                jnname = json(k+1:k+nmllen)
            if (onenml) then
                call cjson_error_mesg("json_array",&
                   nmlname//" appears more than once.",fatal)
            else                
                onenml = .true.
                nmlloc = k+nmllen+1
            endif    
        ENDIF
   ENDDO nmlloop
 if (.not. onenml) call cjson_error_mesg("json_array",&
                   nmlname//" is not in the json file.",fatal)

 varloop: do k = nmlloc,len(json)-(len(varname)+2) !> Now search for varname
        if     (json(k:k) == "[" .and. .not. nmlbracket) then !> Begin searching the namelist array
                nmlbracket = .true.
        elseif (json(k:k) == "[" .and. nmlbracket) then !> Inside of a variable array - signal not to search 
                varbracket = .true.
        elseif (json(k:k) == "]" .and. varbracket) then !> Ending the variable array - search again
                varbracket = .false.
        elseif (json(k:k) == "]" .and. .not. varbracket) then !> The end of the namelist - exit
                exit
        elseif (strcase(json( k:k+len(varname) )) == '"'//strcase(trim (varname)) .AND. & 
                .not.varbracket .AND. nmlbracket) then !> When the variable array name is found
                ! !!!!!!!!!!!!!!!!!!!!!!!!!!!  NORMAL  !!!!!!!!!!!!!!!!!!!!! !
                jvname=json(k+1:k+len(varname))
!write (6,*) "K"//varname//"=",k,lowb
                if (json( k+len(varname)+1 : k+len(varname)+1 ) == '"') then !> When its just a normal array
                        call cjson_array_length (json,nmlname,varname,size(nmlval) ,js)
                        if (size(nmlval) > js) then
                           call cjson_error_mesg ("cjson_array_string_mod:string_2d",&
                            "The size of the JSON array must be at least as big as the size of the actual array "//&
                            "for the two dimensional string "//nmlname//"{"//varname//"}.",FATAL)
                        endif
                        ic=0
                        do jj=lbound(nmlval,dim=2),ubound(nmlval,dim=2)
                        do ii=lbound(nmlval,dim=1),ubound(nmlval,dim=1)
                                allocate (character(len=st_size) :: data_string) !> Allocate the data string
                                do i=1,st_size
                                 data_string(i:i)=" " !> Initialize the data string to be empty
                                enddo
                                C_st = string_array(cjson,json//C_NULL_CHAR,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,ic) !> Get the string
                                call c_f_pointer ( C_St,F_St , [st_size] ) !> Convert the string to a Fortran pointer
                                do i=1,st_size !> Loop through the sring characters
                                  if (F_St(i) == c_null_char) exit  !> End the loop on the null character
                                  data_string(i:i) = f_st(i) !> Put the individual characters into the data string
                                enddo
                                nmlval(ii,jj)(1:st_size) = data_string(1:st_size) !> Put the value of the string into var

                                nullify (sp)
                                deallocate (data_string)
                                nullify (F_st)
                                CALL C_free (C_St)
                                C_St = C_NULL_PTR

                                ic=ic+1
                        enddo;enddo

                elseif (json( k+len(varname)+1 : k+len(varname)+1 ) == '(') then !> When its part of an array
                        do i=k+len(varname)+2,len(json)
                                if (json(i-1:i)==')"') then 
                                 varcheck = json(k+len(varname)+2:i-2) !> Find whats in the parenthesis
                                 exit
                                elseif (json(i:i)=='}') then !> This shouldnt happen, so call a Fatal
                                  varcheck = json(k+len(varname)+2:i-2)
                                   call cjson_error_mesg("json_array",&
                                    'SYNTAX ERROR - "'//nmlname//'":[{"'//varname//varcheck// &
                                     "}] is missing a )",fatal)
                                endif
                        enddo
                        if (i>=len(json))call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//"}] is missing a )",fatal)
                        varcheckloop: do i=1,len(varcheck) !> Loop through varcheck to see if there are colons or commas
                                if (varcheck(i:i) == ",") then
                                  comma=.true. ; n1 = i ; comloc = i
                                elseif (varcheck(i:i) == ":" .AND. colon)then
                                  secondcolon=.true. ; colloc2 = i
                                elseif (varcheck(i:i) == ":")then
                                  colon=.true. ; n1 = i ; colloc1 = i
                                endif
                        enddo varcheckloop
                                if (.not.colon .AND. .not.comma) then !> For a single value
                                    call cjson_error_mesg("cjson_array_string_mod:string_2d", &
                          "The variable is a 2D array, but"//varname//'('//varcheck//') is 1D',&
                           FATAL)
                                elseif (colon .AND. .not.comma) then !> For an array
                                    call cjson_error_mesg("cjson_array_string_mod:string_2d", &
                          "The variable is a 2D array, but"//varname//'('//varcheck//') is 1D',&
                           FATAL)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                elseif (.not.colon .AND. comma) then !> For a single value in a 2d Array
                                    if(.not. string_is_num( varcheck(1:n1-1) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(1:n1-1)//" is not a number",FATAL)
                                    endif
                                    if(.not. string_is_num( varcheck(n1+1:len(varcheck)) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(n1+1:len(varcheck))//" is not a number",FATAL)
                                    endif
                                    read(varcheck(1:n1-1),'(I8)')com1 !> Convert the range strings to integers
                                    read(varcheck(n1+1:len(varcheck)),'(I8)')com2
                                    call bounds_check (lbound(nmlval,dim=1),ubound(nmlval,dim=1),varname,&
                                        com1=com1,com2=com2,lowb2=lbound(nmlval,dim=2),upb2=ubound(nmlval,dim=2))
                                allocate (character(len=st_size) :: data_string) !> Allocate the data string
                                do i=1,st_size
                                 data_string(i:i)=" " !> Initialize the data string to be empty
                                enddo
                                C_st = string_val(cjson,json//C_NULL_CHAR,nmlname//C_NULL_CHAR,&
                                        varname//'('//varcheck//')'//C_NULL_CHAR) !> Get the string
                                call c_f_pointer ( C_St,F_St , [st_size] ) !> Convert the string to a Fortran pointer
                                do i=1,st_size !> Loop through the sring characters
                                  if (F_St(i) == c_null_char) exit  !> End the loop on the null character
                                  data_string(i:i) = f_st(i) !> Put the individual characters into the data string
                                enddo
                                nmlval(com1,com2)(1:st_size) = data_string(1:st_size) !> Put the value of the string into var

                                nullify (sp)
                                deallocate (data_string)
                                nullify (F_st)
                                CALL C_free (C_St)
                                C_St = C_NULL_PTR

                                ic=ic+1

                                   comma=.false.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                elseif (colon .and. comma) then !> FOr a range of values in a 2D array

                                  if (secondcolon) then
                                    if(.not. string_is_num( varcheck(1:colloc1-1) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(1:colloc1-1)//" is not a number",FATAL)
                                     elseif (.not. string_is_num( varcheck(colloc1+1:comloc-1) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(colloc1+1:comloc-1)//" is not a number",FATAL)
                                     elseif (.not. string_is_num( varcheck(comloc+1:colloc2-1) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(comloc+1:colloc2-1)//" is not a number",FATAL)
                                     elseif (.not. string_is_num( varcheck(colloc2+1:len(varcheck)) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(colloc2+1:len(varcheck))//" is not a number",FATAL)
                                    endif !> String to number checking
                                    read(varcheck        (1:colloc1-1),'(I8)')    col1
                                    read(varcheck(colloc1+1:comloc-1),'(I8)')     com1 !> Convert the range strings to integers
                                    read(varcheck (comloc+1:colloc2-1),'(I8)')    com2
                                    read(varcheck(colloc2+1:len(varcheck)),'(I8)')col2 !> Convert the range strings to integers
                                    call bounds_check (lbound(nmlval,dim=1),ubound(nmlval,dim=1),varname,lba=col1,uba=com1)
                                    call bounds_check (lbound(nmlval,dim=2),ubound(nmlval,dim=2),varname,lba=com2,uba=col2)
                             CALL cjson_array_length (json,nmlname,varname//'('//varcheck//")",(col2-com2+1)+(com1-col1+1),js)
                                    if (js .NE. (col2-com2+1)+(com1-col1+1)) CALL cjson_error_mesg("json_array",&
                                    "The 2D array "//nmlname//'[{'//varname//'('//varcheck//")} does not match the size of the"//&
                                    " array in the JSON.",FATAL)
                                     ic=0
                                    do jj=com2,col2
                                    do ii=col1,com1

                                        allocate (character(len=st_size) :: data_string) !> Allocate the data string
                                        do i=1,st_size
                                         data_string(i:i)=" " !> Initialize the data string to be empty
                                        enddo
                                        C_st = string_array(cjson,json//C_NULL_CHAR,nmlname//C_NULL_CHAR,&
                                               varname//'('//varcheck//')'//C_NULL_CHAR,ic) !> Get the string
                                        call c_f_pointer ( C_St,F_St , [st_size] ) !> Convert the string to a Fortran pointer
                                        do i=1,st_size !> Loop through the sring characters
                                          if (F_St(i) == c_null_char) exit  !> End the loop on the null character
                                          data_string(i:i) = f_st(i) !> Put the individual characters into the data string
                                        enddo
                                        nmlval(ii,jj)(1:st_size) = data_string(1:st_size) !> Put the value of the string into var

                                        nullify (sp)
                                        deallocate (data_string)
                                        nullify (F_st)
                                        CALL C_free (C_St)
                                        C_St = C_NULL_PTR
 
                                        ic=ic+1
                                    enddo
                                    enddo





                                  elseif (comloc < colloc1) then !> When the array is in dim=2 of the variable
                                    if(.not. string_is_num( varcheck(1:comloc-1) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(1:comloc-1)//" is not a number",FATAL)
                                     elseif (.not. string_is_num( varcheck(comloc+1:colloc1-1) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(comloc+1:colloc1-1)//" is not a number",FATAL)
                                     elseif (.not. string_is_num( varcheck(colloc1+1:len(varcheck)) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(colloc1+1:len(varcheck))//" is not a number",FATAL)
                                    endif !> String to number checking
                                    read(varcheck(1:comloc-1),'(I8)')com1
                                    read(varcheck(comloc+1:colloc1-1),'(I8)')col1 !> Convert the range strings to integers
                                    read(varcheck(colloc1+1:len(varcheck)),'(I8)')col2
                                     call bounds_check (lbound(nmlval,dim=1),ubound(nmlval,dim=1),varname,single=com1)
                                     call bounds_check (lbound(nmlval,dim=2),ubound(nmlval,dim=2),varname,lba=col1,uba=col2)
                                    if (col2<col1) then
                                        call cjson_error_mesg ("json_array", "The array limits in "//&
                                        nmlname//'[{'//varname//'('//varcheck//")} are descending.",FATAL)
                                    endif
                       CALL cjson_array_length (json,nmlname,varname//'('//varcheck//")",col2-col1+1,js)
                        begin2=col1
                        if (js >= col2-col1+1) then
                            end2 = col2
                        elseif (js < size(var)) then 
                            end2 = col1+js-1
                        endif
                                     ic=0
                                    do jj=begin2,end2

                                        allocate (character(len=st_size) :: data_string) !> Allocate the data string
                                        do i=1,st_size
                                         data_string(i:i)=" " !> Initialize the data string to be empty
                                        enddo
                                        C_st = string_array(cjson,json//C_NULL_CHAR,nmlname//C_NULL_CHAR,&
                                               varname//'('//varcheck//')'//C_NULL_CHAR,ic) !> Get the string
                                        call c_f_pointer ( C_St,F_St , [st_size] ) !> Convert the string to a Fortran pointer
                                        do i=1,st_size !> Loop through the sring characters
                                          if (F_St(i) == c_null_char) exit  !> End the loop on the null character
                                          data_string(i:i) = f_st(i) !> Put the individual characters into the data string
                                        enddo
                                        nmlval(com1,jj)(1:st_size) = data_string(1:st_size) !> Put the value of the string into var

                                        nullify (sp)
                                        deallocate (data_string)
                                        nullify (F_st)
                                        CALL C_free (C_St)
                                        C_St = C_NULL_PTR

                                        ic=ic+1
                                    enddo

                                  elseif (colloc1 < comloc) then !> When the array is in dim=1 of the variable
                                    if(.not. string_is_num( varcheck(1:colloc1-1) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(1:colloc1-1)//" is not a number",FATAL)
                                     elseif (.not. string_is_num( varcheck(colloc1+1:comloc-1) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(colloc1+1:comloc-1)//" is not a number",FATAL)
                                     elseif (.not. string_is_num( varcheck(comloc+1:len(varcheck)) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(comloc+1:len(varcheck))//" is not a number",FATAL)
                                    endif !> String to number checking
                                    read(varcheck(1:colloc1-1),'(I8)')col1
                                    read(varcheck(colloc1+1:comloc-1),'(I8)')col2 !> Convert the range strings to integers
                                    read(varcheck(comloc+1:len(varcheck)),'(I8)')com2
                                     call bounds_check (lbound(nmlval,dim=1),ubound(nmlval,dim=1),varname,lba=col1,uba=col2)
                                     call bounds_check (lbound(nmlval,dim=2),ubound(nmlval,dim=2),varname,single=com2)
                                    if (col2<col1) then
                                        call cjson_error_mesg ("json_array", "The array limits in "//&
                                        nmlname//'[{'//varname//'('//varcheck//")} are descending.",FATAL)
                                    endif
                       CALL cjson_array_length (json,nmlname,varname//'('//varcheck//")",col2-col1+1,js)
                        begin1=col1
                        if (js >= col2-col1+1) then
                            end1 = col2
                        elseif (js < size(var)) then 
                            end1 = col1+js-1
                        endif
                                     ic=0
                                    do ii=begin1,end1
                                        allocate (character(len=st_size) :: data_string) !> Allocate the data string
                                        do i=1,st_size
                                         data_string(i:i)=" " !> Initialize the data string to be empty
                                        enddo
                                        C_st = string_array(cjson,json//C_NULL_CHAR,nmlname//C_NULL_CHAR,&
                                               varname//'('//varcheck//')'//C_NULL_CHAR,ic) !> Get the string
                                        call c_f_pointer ( C_St,F_St , [st_size] ) !> Convert the string to a Fortran pointer
                                        do i=1,st_size !> Loop through the sring characters
                                          if (F_St(i) == c_null_char) exit  !> End the loop on the null character
                                          data_string(i:i) = f_st(i) !> Put the individual characters into the data string
                                        enddo
                                        nmlval(ii,com2)(1:st_size) = data_string(1:st_size) !> Put the value of the string into var

                                        nullify (sp)
                                        deallocate (data_string)
                                        nullify (F_st)
                                        CALL C_free (C_St)
                                        C_St = C_NULL_PTR


                                        ic=ic+1
                                    enddo
                                  endif
                                colon=.false.
                                comma=.false.
                                secondcolon=.false.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                                endif !(.not.colon .AND. comma)
                                        
                endif !(json( k+len(varname)+1 : k+len(varname)+1 ) == '(')
        endif !(json(k:k) == "[" .and. .not. nmlbracket)
 enddo varloop
 !> Memory cleanup
 if (allocated(varcheck)) deallocate(varcheck)
end subroutine string_2d

end module cjson_array_string_mod
