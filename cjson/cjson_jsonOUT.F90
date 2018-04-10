module cjson_jsonout_mod
!> This module is designed to output all variables that are called using the subroutine JSON_ARGS 
!! in a standard compliant JSON format, and so that the arrays are in one array instead of the 
!! partials that the cjson_*.F90 files accept.  The output JSON file will be set up in order of 
!! calls to JSON_ARGS, then in order of scalars, arrays, and 2D arrays.  Within that, output is 
!! written in the numerical order that they are given to the JSON_ARGS subroutine.  The module uses
!! ISO_C_BINDING, cjson_error_mod, and mpif.h.
!!
!!
!! Here is a small example.
!! Input JSON:
!! \code 
!! { "GROUP1": { "var10":1, "array2":[2,2], "array2(1)":20 },
!!   "GROUP2": { "scalar":{"value":300 , "units":"cm}, "array":{"value":[4,5],"units="none"} } }
!! \endcode
!! The calls to JSON_ARGS
!! \code
!!   CALL JSON_ARGS("GROUP2",json,cjson, &
!!   var20 = scalar     , varname20="scalar"  , &
!!   var2 = unitsscalar , varname2="scalar"   , objname2="units", &
!!   array1= array      , arrayname1 = "array", &
!!   var1 = unitsarray  , varname1="array" , objname1="units")
!!
!!   CALL JSON_ARGS("GROUP1",json,cjson, &
!!   var10 = var10      , varname10="var10"  , &
!!   array2= array2     , arrayname2 = "array2")
!! \endcode
!! The output json will look like: 
!! \code
!! {
!!   "GROUP2": {
!!        "array":  {
!!                  "units": "none"
!!                  },
!!        "scalar": {
!!                  "units": "cm"
!!                  },
!!        "scalar": {
!!                  "value": 3
!!                  },
!!        "array":  {
!!                  "value": [4,5]
!!                  }
!!             },
!!   "GROUP1": {
!!        "var10":  {
!!                  "value": 1
!!                  },
!!        "array2": {
!!                  "value": [20,2]
!!                  }
!!             }
!! }
!! \endcode
!!  There are a few things to note here.  
!! 1. scalar was converted to meters, BUT the units still say cm.  The units will always be the 
!!    original units
!! 2. Although array units is listed last in the JSON_ARGS call, it shows up first in the output 
!!    json.  That's because it is a scalar and has the lowest number.  Scalars first in numerical 
!!    order, followed by arrays and then 2D arrays.
!! 3. GROUP2 is before GROUP1 because the call to JSON_ARGS for GROUP2 occurs before GROUP1
!! 4. array2(1) was replaced, and the output shows the array as it is stored and used by the 
!!    calling module.
!! 5. All of the variables have the key "value" to show their value.  If any other attribute name is 
!!    used, then the variable is listed again with the key being the attribute name (ie. units).
!!
!! The output json pointer is generated automatically when JSON_ARGS is called (OUTPUT_JSON), BUT 
!! the output is not automatically generated.  The output routine must be called in order to 
!! actually write the output to the output.json file.  OUTPUT_JSON should not be changed by any 
!! other modules other than this one.    
use cjson_error_mod,     only:cjson_error_mesg, NOTE, WARNING, FATAL
use ISO_C_BINDING
implicit none
include 'mpif.h'
PUBLIC
 type (C_PTR)  :: OUTPUT_JSON  !< This is the cpointer to the output json.  This is added to.
 type (C_PTR)  :: OBJ_PTR      !< A pointer used for a given namelist.  This can be changed for each JSON object
 logical,public:: jsonout_initialized !< Set to true if OTUPUT_JSON has been initialized
 
 interface
          !> Parses a json string
         type(C_PTR) function parseJson (json) result (cjson) bind(C, name="parseJson")
           use iso_c_binding
           character(kind=c_char) :: json     !< The json string
         end function
          !> Creates an object for a cjson file
         type(C_PTR) function cJSON_CreateObject () result (cobj) bind(C, name="cJSON_CreateObject")
           use iso_c_binding
         end function
          !> Creates a number using a double to use with cJSON
         type (c_ptr) function cJSON_CreateNumber(n) result (item) bind(C, name="cJSON_CreateNumber")
           use iso_c_binding
                real (kind=c_double),intent(in),value        :: n            !< The input number     
         end function
          !> Creates a string to use with cJSON
         type (c_ptr) function cJSON_CreateString (s) result (item) bind(C, name="cJSON_CreateString")
           use iso_c_binding
                character(kind=c_char)          :: s            !< The input string     
         end function
          !> Creates a true to use with cJSON
         type (c_ptr) function cJSON_CreateTrue () result (item) bind(C, name="cJSON_CreateTrue")
           use iso_c_binding
         end function
          !> Creates a false to use with cJSON
         type (c_ptr) function cJSON_CreateFalse () result (item) bind(C, name="cJSON_CreateFalse")
           use iso_c_binding
         end function
          !> Creates a numerical array to use with cJSON
         type (c_ptr) function cJSON_CreateDoubleArray(numbers,icount) result (item) bind(C, name="cJSON_CreateDoubleArray")
           use iso_c_binding
                real (kind=c_double),intent(in),dimension(*)       :: numbers !< The values in the array
                integer (kind=c_int),intent(in),value        :: icount  !< The number of items in the array 
         end function
          !> Creates an array to use with cJSON
         type (c_ptr) function cJSON_CreateArray() result (array) bind(C, name="cJSON_CreateArray")
           use iso_c_binding
         end function
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!       
          !> Used to add an item to an array (logical or string)
         subroutine cJSON_AddItemToArray(array,item)    bind(C, name="cJSON_AddItemToArray")
           use iso_c_binding
                type (C_PTR),intent(in),value   :: array    !< Pointer created from  cJSON_CreateArray
                type (C_PTR),intent(in),value   :: item     !< Created pointer to what is being added
         end subroutine
          !> Adds an item to a JSON object.  Used to add namelist object to full JSON or adding a 
          !! variable to a namelist object, or adding a key to a variable.
         subroutine cJSON_AddItemToObject (cpt_at_to, obj_name, cpt_addin)      bind(C, name="cJSON_AddItemToObject")
           use iso_c_binding
                type (C_PTR),intent(in),value   :: cpt_at_to     !< The pointer the level above (root for nmls, nml for var, var for values)
                type (C_PTR),intent(in),value   :: cpt_addin     !< THe pointer to what is being added in
                character(kind=c_char)          :: obj_name      !< The name of the object
         end subroutine
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!       
          !> Writes the json pointer OUTPUT_JSON to a file
         integer function write_json_file (cjson) bind(C, name="write_json_file") !> Writes the MOM_input file 
           use iso_c_binding
                type (C_PTR),intent(in),value   :: cjson        !< The json pointer
         end function
 end interface
!> Interface used for calling programs
 interface cjson_write
     module procedure cjson_write_json_scalar
     module procedure cjson_write_json_array
     module procedure cjson_write_2D
 end interface

CONTAINS
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date July 2016
!! Initializes the JSON object that all of the variables in the current call will be written to.  
!! This is similar to a namelist 
subroutine cjson_write_json_object (nmlname)
 character(*)            :: nmlname !< The name of the JSON object (namelist-like)
 
 if (.not. jsonout_initialized) then !> Check if the output JSON has been initialized
     OUTPUT_JSON = cJSON_CreateObject ()
     jsonout_initialized = .true.
 endif
 OBJ_PTR = C_NULL_PTR
 OBJ_PTR = cJSON_CreateObject () !> Create a new JSON object
 call cJSON_AddItemToObject(OUTPUT_JSON , trim(nmlname )//C_NULL_CHAR , OBJ_PTR) !> Add the "namelist" to the JSON
end subroutine cjson_write_json_object 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date July 2016
!! The variable and variable name are input in order to write out the JSON.  The subroutine uses 
!! the public OBJ_PTR pointer for the namelist object.  Each time this subroutine is called, it 
!! will create a new variable object and key.  The key is whatever attribute was requested from 
!! the wrapper (ie. "units"), or "value" if no attibute is given.  
!! Scalar values are written out at the key.  
!! If someone used this call:
!! \code
!! CALL JSON_ARGS("NMLNAME",json,cjson, &
!!   var1=VARNAME , varname1="VARNAME", &
!!   var2=units   , varname2="VARNAME", objname2="units")
!! \endcode
!! Where varname = 6m, then the output format looks like this:
!! \code
!! {
!!   "NMLNAME": {
!!        "VARNAME":     {
!!             "value": 6
!!                       },
!!        "VARNAME":     {
!!             "units": "m"
!!                       }
!!             }
!! }
!! \endcode
!! The output is written in numerical order (scalars, arrays, then 2D arrays)
subroutine cjson_write_json_scalar (nmlval,varname,attribute,st_size)

 class (*),target,intent(IN)       :: nmlval      !< The input variable
 class (*),pointer                 :: var         !> \param var A pointer to the variable
 character(*),intent(IN)           :: varname     !< The name of the variable
 integer,optional,intent(IN)       :: st_size     !< The size of the input string
 character(*),optional,intent(IN)  :: attribute   !< An included attribute being requested (units, default, etc)
 character (len=5)                 :: value = "value"
 character (len=:),allocatable     :: key         !> \param key The actual name of the key
 type (c_ptr)                      :: cvar        !> \param cvar A C pointer for the variable
 integer,pointer                   :: ip => null() 
 real   ,pointer                   :: rp => null() 
 logical,pointer                   :: lp => null() 
 character,pointer                 :: sp => null() 
 type (c_ptr)                      :: sptr        !> \param sptr A C pointer to the JSON formatted string
 var => nmlval !> Point var to the variable
 cvar = C_NULL_PTR
 cvar = cJSON_CreateObject () !> Create a new variable object 
 call cJSON_AddItemToObject(OBJ_PTR,trim(varname)//C_NULL_CHAR,cvar) !> Add the variable to the namelist object
 if (present (attribute)) then !> Set the key to attribute if it's present, otherwise use value
     key = attribute
 else
     key = value
 endif
 select type (var) !> Use pointers for each type
  type is (integer) !> Numbers are added as doubles
     ip => var
     call cJSON_AddItemToObject(cvar, key//C_NULL_CHAR, cJSON_CreateNumber( dble(ip) ) )
     ip => null ()
  type is (real) ! Numbers are added as doubles
     rp => var
     call cJSON_AddItemToObject(cvar, key//C_NULL_CHAR, cJSON_CreateNumber( dble(rp) ) )
     rp => null ()
  type is (logical) !> Add true or false for logicals
     lp => var
     if (lp)       call cJSON_AddItemToObject(cvar, key//C_NULL_CHAR,cJSON_CreateTrue()  )
     if (.not. lp) call cJSON_AddItemToObject(cvar, key//C_NULL_CHAR,cJSON_CreateFalse() )
     lp => null ()
  type is (character(*)) !> Add a string using the size of the string and a string pointer
    if (.not. present(st_size) ) call cjson_error_mesg ('cjson_write_json_scalar',&
       "The call for "//varname//                                                 &
       " must include the st_size as an input when calling JSON_ARGS",            &
       FATAL )
     sp => var
     call cjson_write_json_stringScalar (sp,st_size,sptr)
     call cJSON_AddItemToObject(cvar, key//C_NULL_CHAR,&
          sptr )
     sp => null ()
 end select

!> Reset the pointers
 cvar = C_NULL_PTR
end subroutine cjson_write_json_scalar
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date July 2016
!! The variable and variable name are input in order to write out the JSON.  The subroutine uses 
!! the public OBJ_PTR pointer for the namelist object.  Each time this subroutine is called, it 
!! will create a new variable object and key.  The key is whatever attribute was requested from 
!! the wrapper (ie. "units"), or "value" if no attibute is given.  
!! Array values are written out at the key.  
!! If someone used this call:
!! \code
!! CALL JSON_ARGS("NMLNAME",json,cjson,     &
!!   array1=VARNAME , arrayname1="VARNAME", &
!!   var2=units     , varname2="VARNAME", objname2="units")
!! \endcode
!! Where varname = {6,7,8} m, then the output format looks like this:
!! \code
!! {
!!   "NMLNAME": {
!!        "VARNAME":     {
!!             "units": "m"
!!                       }
!!        "VARNAME":     {
!!             "value": [6,7,8]
!!                       },
!!             }
!! }
!! \endcode
!! The output is written in numerical order starting with scalars, then arrays, and lastly 2D arrays.  
subroutine cjson_write_json_array (nmlval,varname,attribute,st_size)
use iso_c_binding
 class (*), target,intent(in)      :: nmlval(:)   !< The variable
 class (*), pointer                :: var(:)      !> \param var A pointer to the variable
 character(*),intent(in)           :: varname     !< The name of the variable in the JSON file
 character(*),optional,intent(IN)  :: attribute   !< An included attribute being requested (units, default, etc)
 character (len=5)                 :: value = "value"
 character (len=:),allocatable     :: key         !> \param key The actual name of the key
 type (c_ptr)                      :: cvar        !> \param cvar A C pointer created to hold the variable information
 type (c_ptr)                      :: arrayptr    !> \param arrayptr A C pointer created to hold an array
 integer,pointer                   :: ip(:) => null()
 real   ,pointer                   :: rp(:) => null()
 logical,pointer                   :: lp(:) => null()
 character,pointer                 :: sp(:) => null()
 integer                           :: i           !> \param i Used for looping
 integer,intent(in),optional       :: st_size     !< The size of an input string
 type (c_ptr)                      :: cTorF       !> \param cTorF A C pointer created to hold either true or false

 var => nmlval
 cvar = C_NULL_PTR
 arrayptr = C_NULL_PTR
 cTorF = C_NULL_PTR

 cvar = cJSON_CreateObject () !> Create a new variable object 
 call cJSON_AddItemToObject(OBJ_PTR,trim(varname)//C_NULL_CHAR,cvar) !> Add the variable to the object
 select type (var)
  type is (integer) !> Numebrs are written as arrays of doubles
     ip => var
     call cJSON_AddItemToObject(cvar, value//C_NULL_CHAR, cJSON_CreateDoubleArray( dble(ip), size(ip) ) )
  type is (real)
     rp => var
     call cJSON_AddItemToObject(cvar, value//C_NULL_CHAR, cJSON_CreateDoubleArray( dble(rp), size(rp) ) )
  type is (logical) !> Logcals require a more manual approach
     lp => var
      arrayptr = cJSON_CreateArray() !> Crate an array for logicals
     do i = 1,size(var)
          if (lp(i))       cTorF = cJSON_CreateTrue () !> Determine if it is .true. or .false.
          if (.not. lp(i)) cTorF = cJSON_CreateFalse()
          CALL cJSON_AddItemToArray(arrayptr,cTorF) !> Add it to the array
     enddo
     call cJSON_AddItemToObject(cvar, value//C_NULL_CHAR, arrayptr) !> Add the array to the variable
  type is (character(*))
    if (.not. present(st_size) ) call cjson_error_mesg ('cjson_write_json_array',&
       "The call for "//varname//                                                & 
       " must include the st_size as an input when calling JSON_ARGS",           &
       FATAL )
     call cjson_write_json_stringArray (var,st_size,arrayptr) !> Handle strings with their own routine 
     call cJSON_AddItemToObject(cvar, value//C_NULL_CHAR, arrayptr) !> Put the string array pointer into the variable
 end select

 !> Reset all pointers
 arrayptr = C_NULL_PTR
 cvar = C_NULL_PTR
 cTorF = C_NULL_PTR

end subroutine cjson_write_json_array
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date July 2016
!! Change a 2D array to a 1D array pointer and send it to the cjson_write_json_array routine
SUBROUTINE cjson_write_2D (nmlval,varname,attribute,st_size)
use iso_c_binding
 class (*), target,intent(in)      :: nmlval(:,:) !< The variable
 class (*), pointer                :: var(:,:)    !> \param var A pointer to the variable
 character(*),intent(in)           :: varname     !< The name of the variable 
 character(*),intent(in),optional  :: attribute   !< An included attribute request (such as "units")
 integer,pointer                    :: ii(:,:) => null()
 real   ,pointer                   :: rr(:,:) => null()
 logical,pointer                   :: ll(:,:) => null()
 character,pointer                 :: ss(:,:) => null()
 integer,pointer                   :: ip(:) => null()
 real   ,pointer                   :: rp(:) => null()
 logical,pointer                   :: lp(:) => null()
 character,pointer                 :: sp(:) => null()
 integer,intent(in),optional       :: st_size     !< The size of the input string
 integer                           :: ar_size     !> \param ar_size The size of the array

  var => nmlval
  ar_size = size(var)

 select type (var) !> Select the intrinsic type of the namelist variable
 
   type is (integer) !> Copy the 2D array into a temporary 1D array
    ii => var !> Use a 2D array pointer
    call c_f_pointer( C_LOC(ii(1,1)), ip, [ar_size]) !> Point a 1D array pointer to the 2D pointer
    call cjson_write_json_array(ip,trim(varname),attribute) !> Call the write subroutine
   type is (real) ! 
    rr => var
    call c_f_pointer( C_LOC(rr(1,1)), rp, [ar_size])
    call cjson_write_json_array(rp,trim(varname),attribute)
   type is (logical) !
    ll => var
    call c_f_pointer( C_LOC(ll(1,1)), lp, [ar_size])
    call cjson_write_json_array(lp,trim(varname),attribute)
   type is (character(*)) !
    ss => var
    call c_f_pointer( C_LOC(ss(1,1)), sp, [ar_size])
    call cjson_write_json_array(sp,trim(varname),attribute=attribute,st_size=st_size)
 end select

end subroutine cjson_write_2D


!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date July 2016
!! Subroutine for handling string arrays.  This pulls out individual array members and adds them one
!! by one because string array passing from Fortran to C is unreliable.  The individual strings are 
!! added to the array pointer, then the array pointer is sent back to the calling function
subroutine cjson_write_json_stringScalar (var,st_size,sptr)
 integer,intent(in)                :: st_size     !< The size of the input string var
 character(len=st_size),intent(in) :: var         !< The input string variable
 integer                           :: i           !> \param i Used for looping
 type (c_ptr),intent(out)          :: sptr        !> \param sptr A C pointer for a string
 sptr    = C_NULL_PTR
     sptr = cJSON_CreateString (trim(var(1:st_size))//C_NULL_CHAR) !> Get the cJSON string pointer
end subroutine cjson_write_json_stringScalar
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date July 2016
!! Subroutine for handling string arrays.  This pulls out individual array members and adds them one
!! by one because string array passing from Fortran to C is unreliable.  The individual strings are 
!! added to the array pointer, then the array pointer is sent back to the calling function
subroutine cjson_write_json_stringArray (var,st_size,arrayptr)
 integer,intent(in)                :: st_size     !< The size of the input string var
 character(len=st_size),intent(in) :: var(:)      !< The input string variable
 integer                           :: i           !> \param i Used for looping
 type (c_ptr),intent(out)          :: arrayptr    !< The pointer holding the string array 
 type (c_ptr)                      :: sptr        !> \param sptr A C pointer for a string
 arrayptr = cJSON_CreateArray() !> Initialize the array
 sptr = C_NULL_PTR
 do i=1,size(var) !> Loop through the array
     sptr = cJSON_CreateString (trim(var(i)(1:st_size))//C_NULL_CHAR) !> Get the cJSON string pointer
     CALL cJSON_AddItemToArray(arrayptr,sptr) !> Add the string to the array
     sptr = C_NULL_PTR    !> Reset the pointer
 enddo
end subroutine cjson_write_json_stringArray


!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date July 2016
!! Uses the PUBLIC variable OUTPUT_JSON and writes the output.  Calls the C routine write_json_file
!! in write_json.c to do the actual writing.  The writting is done by the processor with rank 0.
subroutine cjson_write_json_out
 logical            :: is_mpi_on   !> \param is_mpi_on Check if MPI is initialized
 integer            :: ierr        !> \param ierr An MPI output error check
 integer            :: rank        !> \param rank The processor MPI rank
 integer            :: CP          !> \param CP An integer returned from the 
                                   !!        function write_json_file (1=success)

 call MPI_INITIALIZED (is_mpi_on,ierr) !> Check if MPI is initialized
! if (.not. is_mpi_on) CALL MPI_INIT(ierr) !> If its not, then initialize it
 if (is_mpi_on) then
   call MPI_COMM_RANK (MPI_COMM_WORLD, rank, ierr) !> Get the processor ranks
   if (ierr .ne. mpi_success) call cjson_error_mesg("cjson_write_json_out","Error getting PE numbers",FATAL)
   if (rank == 0) then !> <DO ONLY ON ONE PROCESSOR>
    CP = write_json_file (OUTPUT_JSON) !> Call the write function
    if (CP .ne. 1) call cjson_error_mesg("cjson_write_json_out","Error writing the JSON output.",FATAL) 
   endif  !> </DO ONLY ON ONE PROCESSOR>
 else
  call cjson_error_mesg("cjson_write_json_out","Writing of JSON only supported with MPI",WARNING)
 endif
! call MPI_BARRIER (MPI_COMM_WORLD,ierr) !> Get all the processors together
! if (.not. is_mpi_on) CALL MPI_FINALIZE(ierr) !> If MPI wasn't on before, turn it back off
 

end subroutine cjson_write_json_out




end module cjson_jsonout_mod

