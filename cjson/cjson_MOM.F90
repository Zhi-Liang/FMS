module cjson_MOM_mod
           use iso_c_binding 
 use mpp_mod,           only: FATAL, WARNING, NOTE
 use mpp_mod,           only: mpp_sync, mpp_root_pe, mpp_pe
 use fms_mod,           only: fms_init, error_mesg   
 
implicit none
 include 'mpif.h'

 interface
         type(C_PTR) function parseJson (json) result (cjson) bind(C, name="parseJson")
           use iso_c_binding
           character(kind=c_char) :: json     !< The json string
         end function
         type(C_PTR) function cJSON_CreateObject () result (cobj) bind(C, name="cJSON_CreateObject")
           use iso_c_binding
         end function
         type (c_ptr) function cJSON_CreateNumber(n) result (item) bind(C, name="cJSON_CreateNumber")
           use iso_c_binding
                real (kind=c_double),intent(in),value        :: n            !< The input number     
         end function
         type (c_ptr) function cJSON_CreateString (s) result (item) bind(C, name="cJSON_CreateString")
           use iso_c_binding
                character(kind=c_char)          :: s            !< The input string     
         end function
         type (c_ptr) function cJSON_CreateTrue () result (item) bind(C, name="cJSON_CreateTrue")
           use iso_c_binding
         end function
         type (c_ptr) function cJSON_CreateFalse () result (item) bind(C, name="cJSON_CreateFalse")
           use iso_c_binding
         end function
!!!!!!!!!!!!!!!!!!!
         subroutine cJSON_AddItemToObject (cpt_at_to, obj_name, cpt_addin)      bind(C, name="cJSON_AddItemToObject")
           use iso_c_binding
                type (C_PTR),intent(in),value   :: cpt_at_to     !< The pointer the level above (root for nmls, nml for var, var for values)
                type (C_PTR),intent(in),value   :: cpt_addin     !< THe pointer to what is being added in
                character(kind=c_char)          :: obj_name      !< The name of the object
         end subroutine

         function mom_json_file (cjson) result (json) bind(C, name="mom_json_file") !> Writes the MOM_input file 
           use iso_c_binding
                type (C_PTR),intent(in),value   :: cjson        !< The json pointer
                type (C_PTR)                    :: json         !< THe json string
         end function

 end interface
contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date June 2016
!! MOMjson_init is a parser that takes in MOM_input as the input file and converts it to 
!! MOM_input.json in JSON format using cJSON.  The two outputs of the routine are mom_input which 
!! is a string of the contents of the MOM_input file and mom_json which is a string of the contents
!! of the MOM_input.json file.  
!!
!! The parsing is done on == , = , and ! (That is at least 6 spaces from the end of a line).  Lines
!! that contain none of these symbols are treated as comments by the parser and are included as a 
!! description key.  This includes blank lines.  
!! When a == is found, the parser strips off the "! ==" , "==" , and "module", and creates a new 
!! object with the name of the module.  This is used like a namelist name.  At the top, the parser
!! adds MOM_global which will be the object containing any variables that do not have a containing
!! module in the MOM_input file.
!! If a single = is found, the parser will get the 
!! value of the variable, and will also read the units within the [].  If no units are given, the 
!! parser will include a blank string for the units.  If a second single = is found on a line, the
!! parser assumes that whatever is to the right of that equals sign is the default, and will 
!! include the default value.  Lines that have ! for comments are included as "description" and the
!! ! is removed.    
!!
!! The input to json is done as follows
!! \verbatim
!! -------------- MOM_input ---------------
!! /* Top comments */
!! VAR = 3              ![UNIT] default = 0
!!                      ! A comment
!! !COMVAR = "string"   !
!!                      ! This variable is commented
!!                      ! out
!! !=== module MOM_module ==!
!! BOOLVAR = True       ! default = False
!!                      ! Some other Comment
!! REALVAR = 10.1       ! [m]
!!                      ! A distance
!! 
!! ----------- MOM_input.json -------------
!! {
!!      "MOM_global": {
!!              "description":"/* Top comments */",
!!              "VAR" : {
!!                      "value": 3,
!!                      "units": "UNIT",
!!                      "default": 0,
!!                      "description": "A comment"
!!              },
!!              "!COMVAR" : {
!!                      "value": "string",
!!                      "units": "",
!!                      "description": "This variable is commented",
!!                      "description": "out",
!!              }
!!      },
!!      "MOM_module": {
!!              "BOOLVAR": {
!!                      "value": true,
!!                      "units": "",
!!                      "default": false,
!!                      "description": "Some other Comment"
!!              },
!!              "REALVAR": {
!!                      "value": 10.1,
!!                      "units": "m",
!!                      "description": "A distance"
!!              }
!!      }
!! }
!! \endverbatim
!! Override files are done in numerical order from 0-99, with MOM_override being the first, 
!! MOM_override0 overriding MOM_override, and MOM_override99 overriding all other variables.
!! New variables are put in the MOM_global object with a note where they came from.  Variables with
!! a #override are given new values.  In the above example if MOM_override2 had
!! \code 
!! NEWVAR = 10
!! #override REALVAR = -1.1
!! \endcode
!! Then MOM_input.json would look like:
!! \verbatim
!!      "MOM_global": {
!!              "NEWVAR":{ "value" : 10, "from" : "MOM_override2"},
!!              "description":"/* Top comments */",
!!              "VAR" : {
!!                      "value": 3,
!!                      "units": "UNIT",
!!                      "default": 0,
!!                      "description": "A comment"
!!              },
!!              "!COMVAR" : {
!!                      "value": "string",
!!                      "units": "",
!!                      "description": "This variable is commented",
!!                      "description": "out",
!!              }
!!      },
!!      "MOM_module": {
!!              "BOOLVAR": {
!!                      "value": true,
!!                      "units": "",
!!                      "default": false,
!!                      "description": "Some other Comment"
!!              },
!!              "REALVAR": {
!!                      "value": 1 , "from" : "MOM_override2",
!!                      "value": 10.1,
!!                      "units": "m",
!!                      "description": "A distance"
!!              }
!!      }
!! }
!! \endverbatim 
                     
subroutine MOMjson_init (mom_input,mom_json,mom_cjson)
           use iso_c_binding
 integer,parameter                              :: size_json = 1000000
 integer,parameter                              :: num_override_files = 99
 character (len=:), allocatable , intent (out)  :: mom_input     !< MOM_input string
 character (*)                  , intent (out)  :: mom_json     !< MOM_input converted to mom_input 
 type (C_PTR)                   , intent (out)  :: mom_cjson    !< A C pointer to the parsed json
 character (len=:), allocatable                 :: value        !> \param value A string holding the value of the variable
 integer                                        :: iunit ,i
 integer                                        :: ii,jj,kk,ll, nn
 integer                                        :: begin_line
 integer                                        :: end_line
 character (len=:),allocatable                  :: get_len      !> \param get_len Used to get the length of the list
 integer                                        :: list_len     !> \param list_len The length of the list
 character (len=15) :: fname
 character (len=150),dimension(:),allocatable     :: lines
 character (len=:),allocatable                  :: LINE,newline
 integer                                        :: line_size
 real                                           :: rval         !> \param rval The value of the variable converted from a string
! character (len=:), allocatable                 :: sandbox      !> \param sandbox This utility string is used to hold a string
 type (C_PTR)                                   :: cjson        !> \param cjson The json string c pointer (root)
 type (C_PTR)                                   :: cnml         !> \param cnml c pointer to the "namelist" level
 type (C_PTR)                                   :: cvar         !> \param cvar c poitner to the "variable" level  
 logical                                        :: nmllevel     !> \param nmllevel This is true when at the nml level (not variable level)
 logical                                        :: fileexist 
 character(len=1, kind=c_char), dimension (:), pointer :: cJSON_full_pointer => null ()
 type (C_PTR)                                   :: C_String_ptr !> \param C_String_ptr a C-string pointer
 character*10                                   :: overnum      !> \param overnum The number of the override file
 character (len=:), allocatable                 :: MOM_override        !> \param MOM_override The name of the override file
 character (len=:), allocatable                 :: MOM_override_data
 character (len=:), allocatable                 :: override_var
 integer                                        :: MOM_global_loc

call fms_init()
 fname="MOM_input"
 iunit=29
!> Allocate the very large temporary character string
 allocate (character (len=1000000) :: get_len)
!> Open the mom_input file
 open (iunit,file=trim(fname),status="old",access="stream")
!> Get the length of the mom_input file to pass to the C routines for allocating the string when reading
  read (iunit,iostat=i) get_len  
  inquire (unit=iunit,pos=list_len) !> Find the endpoint in the mom_input file using inquire
  close (iunit) !> Close the mom_input file
 deallocate (get_len) !> Deallocate the temporary string because it is not neccessary

 if (allocated(mom_input) ) then !> If mom_input already exists, overwrite the old one
        CALL error_mesg('MOMjson_init', "The mom_input had been previously initialized.  Overwriting previous initialization.",WARNING)
        deallocate(mom_input)
 endif

 allocate (character (len=list_len) :: mom_input) !> Allocate the mom_input string with the proper length
iunit=29
open (iunit,file=trim(fname),status="old",access="stream")
 read (iunit,iostat=i) mom_input  !> Read the mom_input string
 close (iunit) !> Close the mom_input file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> Get a string of each lines for parsing
end_line=1
line_size=0
jj=1

 do ii=1,list_len       
   if (mom_input(ii:ii) == char(10)) then
        end_line = ii
        jj=jj+1
   endif
  begin_line=end_line
 enddo
 !> Allocate the string array of lines then get the lines
 allocate ( lines (jj-1) )
 jj=1
 begin_line=1
 do ii=1,list_len        
   if (mom_input(ii:ii) == char(10)) then
        end_line = ii
        lines(jj) = mom_input(begin_line:end_line)
        jj=jj+1
   endif
  begin_line=end_line
 enddo
!> Initialize the json with MOM_global object
 cjson = cJSON_CreateObject ()
 cnml = C_NULL_PTR
 cnml = cJSON_CreateObject ()
 call cJSON_AddItemToObject(cjson, "MOM_global"//C_NULL_CHAR ,cnml)
 nmllevel = .true.
!> Parse the lines on the =.  If 2 = are present, then the second is default
 do ii=1,size(lines)
   ll = len( trim(lines(ii)) )
   allocate (character (len=ll) :: line)
   line=trim( lines(ii) ) 
   lineloop: do jj = 1,len( trim(line) )
        if (len(line) < 4) then
                exit lineloop
!!!!!!!!
        elseif     (line(jj:jj+1) == "==") then !> Look for the == which will be the pseudo-namelist
                cnml = C_NULL_PTR
                cnml = cJSON_CreateObject ()
                call cJSON_AddItemToObject(cjson, trim( nmln(line,jj) )//C_NULL_CHAR ,cnml)
                nmllevel = .true.
                exit lineloop
!!!!!!!!
        elseif (lines(ii)(jj:jj) == "=") then !> look for equals sign
                cvar = C_NULL_PTR
                cvar = cJSON_CreateObject () !> Create a new variable object 
                nmllevel = .false.
                call cJSON_AddItemToObject(cnml, trim( varn(line,jj) )//C_NULL_CHAR ,cvar) !> Add variable to cnml
                 if     (whatkindofvalue(trim(valuen(line,jj) )) == 1 )then !> True
                   call cJSON_AddItemToObject(cvar, "value"//C_NULL_CHAR,cJSON_CreateTrue()     )
                 elseif (whatkindofvalue(trim(valuen(line,jj) )) == 2 )then !> False
                   call cJSON_AddItemToObject(cvar, "value"//C_NULL_CHAR,cJSON_CreateFalse()    )
                 elseif (whatkindofvalue(trim(valuen(line,jj) )) == 3 )then !> String with double quotes
                   call cJSON_AddItemToObject(cvar, "value"//C_NULL_CHAR, &
                        cJSON_CreateString( singleTOdoublequotes(valuen(line,jj))//C_NULL_CHAR) )
                 elseif (whatkindofvalue(trim(valuen(line,jj) )) == 4 )then !> String with single quotes
                   call cJSON_AddItemToObject(cvar, "value"//C_NULL_CHAR, &
                        cJSON_CreateString( singleTOdoublequotes(valuen(line,jj))//C_NULL_CHAR) )
                 elseif (whatkindofvalue(trim(valuen(line,jj) )) == 5 )then !> Number
                   value = valuen(line,jj)
                   read( value,* ) rval !> Convert the string to a number
                   deallocate(value)
                   call cJSON_AddItemToObject(cvar, "value"//C_NULL_CHAR,cJSON_CreateNumber(dble(rval)))
                 elseif (whatkindofvalue(trim(valuen(line,jj) )) == 6 )then
                        CALL error_mesg('MOMjson_init',"The value of "//varn(line,jj)//" is not supported." // line ,FATAL)
                 endif
                 !> Add Units
                 call cJSON_AddItemToObject(cvar, "units"//C_NULL_CHAR , cJSON_CreateString( unitsn(line,jj)//C_NULL_CHAR ) ) !> Add units to variable
                 !> Check for default values by looking for another = 
                 do kk=jj+1, len( trim(line) )    
                        if ( lines(ii)(kk:kk) == "=") then !> When a default is found, add the default value to the variable object
                                if     (whatkindofvalue(trim(valuen(line,kk) )) == 1 )then !> True
                                  call cJSON_AddItemToObject(cvar, "dafault"//C_NULL_CHAR,cJSON_CreateTrue()     )
                                elseif (whatkindofvalue(trim(valuen(line,kk) )) == 2 )then !> False
                                  call cJSON_AddItemToObject(cvar, "default"//C_NULL_CHAR,cJSON_CreateFalse()    )
                                elseif (whatkindofvalue(trim(valuen(line,kk) )) == 3 )then !> String with double quotes
                                  call cJSON_AddItemToObject(cvar, "default"//C_NULL_CHAR, &
                                        cJSON_CreateString( singleTOdoublequotes(valuen(line,kk))//C_NULL_CHAR) )
                                elseif (whatkindofvalue(trim(valuen(line,kk) )) == 4 )then !> String with single quotes
                                  call cJSON_AddItemToObject(cvar, "default"//C_NULL_CHAR, &
                                       cJSON_CreateString( singleTOdoublequotes(valuen(line,kk))//C_NULL_CHAR) )
                                elseif (whatkindofvalue(trim(valuen(line,kk) )) == 5 )then !> Number
                                  value = valuen(line,kk)
                                  read( value,* ) rval !> Convert the string to a number
                                  deallocate(value)
                                  call cJSON_AddItemToObject(cvar, "default"//C_NULL_CHAR,cJSON_CreateNumber(dble(rval)))
                                elseif (whatkindofvalue(trim(valuen(line,kk) )) == 6 )then
                                       CALL error_mesg('MOMjson_init',"FATAL: JSON_MOM - The value of "//varn(line,jj)//" is not supported." // line ,FATAL)
                                endif
                          exit lineloop !> Anything after the value is ignored
                        endif
        !!!!!!!!
               enddo
!!!!!!!!
        elseif (line(jj:jj) == "!" ) then
           if (line(jj+3:jj+3) .ne. "=" .and. jj > 5) then !> If there is a comment, write it as description
                if (nmllevel) then
                     call  cJSON_AddItemToObject(cnml,"description"//c_null_char, &
                                cJSON_CreateString(trim(line(jj+1:))//c_null_char) )
                else
                     call  cJSON_AddItemToObject(cvar,"description"//c_null_char, &
                                cJSON_CreateString(trim(line(jj+1:))//c_null_char) )
                endif   
                exit lineloop        
          endif
        elseif (jj == len(line)-4 ) then !> If the line has not = or !, then include it as description
                if (nmllevel) then
                     call  cJSON_AddItemToObject(cnml,"description"//c_null_char,cJSON_CreateString(trim(line(2:))//c_null_char) )
                else
                     call  cJSON_AddItemToObject(cvar,"description"//c_null_char,cJSON_CreateString(trim(line(2:))//c_null_char) )
                endif   
                exit lineloop
        endif
   enddo lineloop
   deallocate(line)
 enddo
deallocate (lines)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 C_String_ptr = mom_json_file(cjson)

mom_json=" "
 CALL C_f_pointer(C_String_ptr,cJSON_full_pointer,[size_json])
    do ii=1 ,size_json
           nn = ii
           if ( cJSON_full_pointer (ii) == c_null_char) exit  !> End the loop on the null character
           mom_json(ii:ii) = cJSON_full_pointer (ii) !> Put each value of the string array into the correct character location
    enddo
    MOM_global_find: do ii=1 ,size_json
        if (mom_json(ii:ii+11) == '"MOM_global"') then
                do jj = ii+11 , ii+21
                   if (mom_json(jj:jj) == '{') then
                        MOM_global_loc = jj
                        exit MOM_global_find
                   endif
                enddo
        endif
    enddo MOM_global_find
!> Loop through all possible files
 override_loop: do ii = -1,99
     iunit=ii+100
        if (ii == -1) then !> Search for MOM_override when ii = -1
           inquire (file="MOM_override",exist=fileexist)
           MOM_override="MOM_override"
        else
           write (overnum,'(I0)') ii
           inquire (file="MOM_override"//overnum,exist=fileexist) !> Look for numbered files in order  
                MOM_override="MOM_override"//overnum
        endif

        if (fileexist) then
!> OPEN THE FILE
                !> Allocate the very large temporary character string
                allocate (character (len=1000000) :: get_len)
                !> Open the mom_input file
                open (iunit,file=trim(MOM_override),status="old",access="stream")
        !> Get the length of the mom_input file to pass to the C routines for allocating the string when reading
                read (iunit,iostat=i) get_len  
                 inquire (unit=iunit,pos=list_len) !> Find the endpoint in the mom_input file using inquire
                close (iunit) !> Close the mom_input file
                deallocate (get_len) !> Deallocate the temporary string because it is not neccessary

                if (allocated(mom_override_data) ) then !> If mom_input already exists, overwrite the old one
                        deallocate(mom_override_data)
                endif
                allocate (character (len=list_len) :: mom_override_data) !> Allocate the mom_input string with the proper length
                iunit=29
                open (iunit,file=trim(MOM_override),status="old",access="stream")
                 read (iunit,iostat=i) mom_override_data  !> Read the mom_input string
                close (iunit) !> Close the mom_input file
                
!> Check the override file for variables
                override_var=""
                nn=1
                do jj=1,list_len       
                   if (mom_override_data(jj:jj) == char(10)) then !> Parse line by line  
                    line=mom_override_data(nn:jj) 
                    nn = jj !> nn is the end of the line here
                    do kk=1,len(line)
!> GET THE NAME OF THE VARIABLE AND THE VALUE
                        if (line (kk:kk) == "=") then !AA
                                mom_json=mom_json(1:MOM_global_loc+1)// char(10)//char(9)//char(9)//    &
                                        ' "'//varn(line,kk)//'": { "value" : '//valuen(line,kk)//       &
                                        ', "from": "' //trim(MOM_override) //'" },'//                   &
                                        mom_json(MOM_global_loc +1:)
                                exit ! Exits the do kk=1,len(line)
!> CHECK FOR AN OVERRIDE     
                        elseif (line(KK:KK) == "#") then !AA
                          if (KK+9 < len(line)) then !AAb
                           if (line(KK:KK+9)=="#override ") then !AAc
                                newline=line(KK+9:len(line))
                                do ll = 1,len(newline)  !a
                                    if (newline(ll:ll) == "=") then!b
                                       do nn=MOM_global_loc,len(trim(mom_json)) !c
                                            if (varn(newline,ll) == mom_json(nn-len( varn(newline,ll) )+1 : nn)) then !d
                                                do i=nn,len(trim(mom_json)) !e
                                                        if (mom_json(i:i+6) == '"value"') then !f
                                                           mom_json = mom_json(1:i-1)//                 &
                                                               '"value": '// valuen(newline,ll)//       &
                                                               ', "from":"'//trim(MOM_override)//'",'// &        
                                                                char(10)//char(13)//char(9)//char(9)//&
                                                                char(9)//mom_json(i:)
                                                           exit
                                                        endif !f
                                                enddo !e
                                            endif !d
                                       enddo !c
                                    endif !b
                                enddo !a
                                exit ! Exits the do kk=1,len(line)
                          endif !AAc
                         endif !AAb
                        endif !AA
                     enddo ! kk=1,len(line)
                    endif ! (mom_override_data(jj:jj) == char(10)) 

                enddo ! jj=1,list_len

        endif ! if (fileexist) 


        
        if ( allocated(MOM_override)      ) deallocate (MOM_override)
        if ( allocated(MOM_override_data) ) deallocate (MOM_override_data)
 enddo override_loop
IF(mpp_pe() == mpp_root_pe() ) THEN
 iunit = 29
 open (iunit,file="MOM_input.json",status="Unknown")
 write (29,*) trim(mom_json)
 close (iunit)
ENDIF !MPP_PE() ENDIF
 call mpp_sync()
 mom_cjson = parseJson (mom_json)
 CALL error_mesg('MOMjson_init',"JSON - MOM_input converted to MOM_input.json and parsed",NOTE)

end subroutine MOMjson_init
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date June 2016
!! The function returns an integer flag based on what kind of value the string value has in it.
!! \verbatim
!! Number Returned | Type of variable
!!      1          |    True
!!      2          |    False
!!      3          |    String with ""
!!      4          |    String with ''
!!      5          |    Number
!!      6          |    Error
!! \endverbatim
integer function whatkindofvalue (value)
 character(*),intent(in)        :: value !< The value of the variable in question
 if    (trim(value) == "True" .or. trim(value) == "true" .or. trim(value) == ".true." &
         .or. trim(value) == "TRUE" .or. trim(value) == ".TRUE.") then    
           whatkindofvalue = 1  
 elseif (trim(value) == "False" .or. trim(value) == "false" .or. trim(value) == ".false." &
         .or. trim(value) == "FALSE" .or. trim(value) == ".FALSE.") then    
           whatkindofvalue = 2
 elseif (value(1:1) == '"' .and. & 
         value( len(value):len(value) ) == '"') then    
           whatkindofvalue = 3
 elseif (value(1:1) == "'" .and. &
           value( len(value):len(value) ) == "'") then    
           whatkindofvalue = 4
 elseif (value(1:1) == "0" .or. &
          value(1:1) == "1" .or. &
          value(1:1) == "2" .or. &
          value(1:1) == "3" .or. &
          value(1:1) == "4" .or. &
          value(1:1) == "5" .or. &
          value(1:1) == "6" .or. &
          value(1:1) == "7" .or. &
          value(1:1) == "8" .or. &
          value(1:1) == "9" .or. &
          value(1:1) == "." .or. &
          value(1:1) == "-" ) then
            whatkindofvalue = 5
 else
            whatkindofvalue = 6
 endif
end function whatkindofvalue
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date June 2016
!! Strips quotes off of strings for writing to json
function singleTOdoublequotes (string) result (newstring)
 character (*)    ,intent(in)   :: string
 character (len=:), allocatable :: newstring
 newstring =  string(2:len(string)-1) 
end function singleTOdoublequotes 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date June 2016
!! Finds the "module" string, removes it, and returns a string of just the module name
function nmln (line,jj) result (varname)
  character (*) ,intent(in)     :: line
  character (len=:),allocatable :: varname
  integer       ,intent(in)     :: jj !< Location of the =
  integer                       :: kk, ll
        top: do kk=jj+1,len( trim(line))
                if (line(kk:kk+6) == "module") then
                   do ll = kk+6, len(line)
                      if (line(ll:ll) == "=") then
                        varname = trim( line(kk+7:ll-1) )
                        exit top
                      endif
                   enddo   
                endif
        enddo top

end function nmln
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date June 2016
!! Returns a string of everything to the left of an = on a line.  It is assumed that this will be 
!! the variable name
function varn (line,jj) result (varname)
  character (*) ,intent(in)     :: line
  character (len=:),allocatable :: varname
  integer       ,intent(in)     :: jj !< Location of the =
  varname = trim( line(2:jj-1) )
end function varn
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date June 2016
!! Finds a [ and returns a string of whatever is inside the square brackets [string]
function unitsn (line,jj) result (varname)
  character (*) ,intent(in)     :: line
  character (len=:),allocatable :: varname
  integer       ,intent(in)     :: jj !< Location of the =
  integer       :: kk, ll
        top: do kk=jj+1,len( trim(line))
                if (line(kk:kk) == "[") then
                   do ll = kk, len(line)
                      if (line(ll:ll) == "]") then
                        varname = trim( line(kk+1:ll-1) )
                        exit top
                      endif
                   enddo   
                endif
         varname =""
        enddo top
end function unitsn
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date June 2016
!! Returns the value of the variable in string form.  It seaches for double or single quotes OR an
!! exclaimation point (' " !).  If a quote is found, it matches that quote to get the string.  If 
!! there is no quote, it finds the ! and returns everything between the = and !.
function valuen (line,jj) result (varname)
  character (*) ,intent(in)     :: line
  character (len=:),allocatable :: varname
  integer       ,intent(in)     :: jj !< Location of the =
  integer                       :: kk, ll
        top: do kk=jj+1,len( trim(line))
                if (line(kk:kk) == "'" .or. line(kk:kk) == '"') then
                   do ll = kk+1, len(line)
                      if (line(ll:ll) == line(kk:kk)) then
                        varname = line(kk:ll)
                        exit top
                      endif
                   enddo
                elseif (line(kk:kk) == "!" .or. kk .eq. len(trim(line)) ) then
                      do ll = jj+1, kk
                        if (line(ll:ll) .ne. " ") then 
                         varname = trim ( line(ll:kk-1) )
                        exit top
                        endif
                      enddo
                endif
        enddo top

end function valuen


end module
