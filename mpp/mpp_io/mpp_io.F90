module mpp_io_mod

    use,intrinsic :: iso_fortran_env, only: error_unit, &
                                            input_unit, &
                                            int8, &
                                            int64, &
                                            output_unit

    use iso_c_binding, only: c_associated, &
                             c_bool, &
                             c_char, &
                             c_double, &
                             c_f_pointer, &
                             c_float, &
                             c_int, &
                             c_int32_t, &
                             c_loc, &
                             c_null_char, &
                             c_null_ptr, &
                             c_ptr, &
                             c_short, &
                             c_size_t

    use mpp_mod
    use mpp_io_c
    use mpp_domains_mod

    implicit none
    private

    !--------------------------------------------------------------------------
    !Public parameters.
    public :: MPP_NETCDF
    public :: MPP_ASCII
    public :: MPP_HDF5
    public :: MPP_WRONLY
    public :: MPP_RDONLY
    public :: MPP_APPEND
    public :: MPP_OVERWR
    public :: MPP_MULTI
    public :: MPP_SINGLE

    !Possibly not needed.
    public :: MPP_DELETE
    public :: MPP_IEEE32     !Possibly not needed.
    public :: MPP_NATIVE     !Possibly not needed.
    public :: MPP_SEQUENTIAL !Possibly not needed.
    public :: MPP_DIRECT     !Possibly not needed.
    public :: MPP_COLLECT    !Possibly not needed.
    public :: FILE_TYPE_USED !Possibly not needed.
    public :: MAX_FILE_SIZE  !Possibly not needed.

    !--------------------------------------------------------------------------
    !Public routines/interfaces
    public :: mpp_io_init
    public :: mpp_io_exit
    public :: mpp_open
    public :: mpp_close
    public :: mpp_write_meta
    public :: mpp_write
    public :: mpp_write_compressed
    public :: mpp_io_unstructured_write
    public :: mpp_write_unlimited_axis
    public :: mpp_read_meta
    public :: mpp_read
    public :: mpp_read_compressed
    public :: mpp_io_unstructured_read

    !New routines
    public :: mpp_ascii_fread
    public :: mpp_get_file_unit
    public :: destroy_atttype
    public :: destroy_axistype
    public :: destroy_fieldtype

    !--------------------------------------------------------------------------
    !Legacy-only features
    public :: mpp_get_id
    public :: mpp_is_valid
    public :: mpp_get_atts
    public :: mpp_get_axis_data
    public :: mpp_get_field_index
    public :: mpp_get_field_name
    public :: mpp_get_att_length
    public :: mpp_get_att_type
    public :: mpp_get_att_name
    public :: mpp_get_att_real
    public :: mpp_get_att_char
    public :: mpp_get_att_real_scalar
    public :: mpp_is_dist_ioroot
    public :: mpp_attribute_exist
    public :: mpp_io_clock_on
    public :: mpp_get_default_calendar
    public :: mpp_get_axis_bounds
    public :: mpp_get_maxunits
    public :: do_cf_compliance
    public :: mpp_modify_meta
    public :: mpp_get_info
    public :: mpp_def_dim
    public :: mpp_get_dimension_length
    public :: mpp_get_file_name
    public :: mpp_file_is_opened
    public :: mpp_get_axes
    public :: mpp_get_fields
    public :: mpp_get_times
    public :: mpp_get_axis_by_name
    public :: mpp_get_time_axis
    public :: mpp_get_tavg_info
    public :: mpp_get_att_value
    public :: mpp_flush

    !Not working
    public :: mpp_read_distributed_ascii

    !Deprecated.
    public :: mpp_get_iospec
    public :: mpp_get_ncid
    public :: mpp_io_set_stack_size
    public :: netcdf_err
    public :: mpp_copy_meta
    public :: mpp_set_unit_range
    public :: mpp_get_unit_range
    public :: mpp_get_axis_index
    public :: mpp_get_axis_length
    public :: mpp_get_recdimid
    public :: mpp_write_axis_data

    public :: atttype
    public :: axistype
    public :: validtype
    public :: fieldtype
    public :: filetype
    public :: default_field
    public :: default_axis
    public :: default_att

    type :: atttype
        private
        integer(kind=c_int) :: type
        integer(kind=c_int) :: len
        character(len=128) :: name
        character(len=1280) :: catt
        real(kind=c_double),dimension(:),allocatable :: fatt
    end type atttype

    type :: axistype
        private
        character(len=128) :: name
        character(len=128) :: name_bounds
        character(len=128) :: units
        character(len=256) :: longname
        character(len=8) :: cartesian
        character(len=256) :: compressed
        character(len=24) :: calendar
        integer(kind=c_int) :: sense
        integer(kind=c_int) :: len
        type(domain1D),pointer :: domain
        real(kind=c_double),dimension(:),allocatable :: data
        real(kind=c_double),dimension(:),allocatable :: data_bounds
        integer(kind=c_int),dimension(:),allocatable :: idata
        integer(kind=c_int) :: id
        integer(kind=c_int) :: did
        integer(kind=c_int) :: type
        integer(kind=c_int) :: natt
        integer(kind=c_int) :: shift
        type(atttype),dimension(:),allocatable :: Att
    end type axistype

    type :: validtype
        private
        logical(kind=c_bool) :: is_range
        real(kind=c_double) :: min
        real(kind=c_double) :: max
    end type validtype

    type :: fieldtype
        private
        character(len=128) :: name
        character(len=128) :: units
        character(len=256) :: longname
        character(len=128) :: standard_name
        real(kind=c_double) :: min
        real(kind=c_double) :: max
        real(kind=c_double) :: missing
        real(kind=c_double) :: fill
        real(kind=c_double) :: scale
        real(kind=c_double) :: add
        integer(kind=c_int) :: pack
        integer(kind=int64),dimension(3) :: checksum
        type(axistype),dimension(:),allocatable :: axes
        integer(kind=c_int),dimension(:),allocatable :: size
        integer(kind=c_int) :: time_axis_index
        integer(kind=c_int) :: id
        integer(kind=c_int) :: type
        integer(kind=c_int) :: natt
        integer(kind=c_int) :: ndim
        type(atttype),dimension(:),allocatable :: Att
        integer(kind=c_int) :: position
    end type fieldtype

    type :: filetype
        private
        character(len=256) :: name
        integer(kind=c_int) :: action
        integer(kind=c_int) :: format
        integer(kind=c_int) :: access
        integer(kind=c_int) :: threading
        integer(kind=c_int) :: fileset
        integer(kind=c_int) :: record
        integer(kind=c_int) :: ncid
        logical(kind=c_bool) :: opened
        logical(kind=c_bool) :: intialized
        logical(kind=c_bool) :: nohdrs
        integer(kind=c_int) :: time_level
        real(kind=c_double) :: time
        logical(kind=c_bool) :: valid
        logical(kind=c_bool) :: write_on_this_pe
        logical(kind=c_bool) :: read_on_this_pe
        logical(kind=c_bool) :: io_domain_exist
        integer(kind=c_int) :: id
        integer(kind=c_int) :: recdimid
        real(kind=c_double),dimension(:),allocatable :: time_values
        integer(kind=c_int) :: ndim
        integer(kind=c_int) :: nvar
        integer(kind=c_int) :: natt
        type(axistype),dimension(:),allocatable :: axis
        type(fieldtype),dimension(:),allocatable :: var
        type(atttype),dimension(:),allocatable :: att
        type(domain2D),dimension(:),allocatable :: domain
        type(domainUG),dimension(:),allocatable :: domain_ug
    end type filetype

    interface mpp_get_id
        module procedure mpp_get_axis_id
        module procedure mpp_get_field_id
    end interface mpp_get_id

    interface mpp_get_atts
        module procedure mpp_get_global_atts
        module procedure mpp_get_field_atts
        module procedure mpp_get_axis_atts
    end interface mpp_get_atts

    interface mpp_modify_meta
        module procedure mpp_modify_field_meta
        module procedure mpp_modify_axis_meta
    end interface mpp_modify_meta

    interface mpp_def_dim
        module procedure mpp_def_dim_nodata
        module procedure mpp_def_dim_int
        module procedure mpp_def_dim_real
    end interface mpp_def_dim

    !Deprecated
    interface mpp_copy_meta
        module procedure mpp_copy_meta_axis
        module procedure mpp_copy_meta_field
        module procedure mpp_copy_meta_global
    end interface mpp_copy_meta

    !--------------------------------------------------------------------------
    !Public Interfaces
    interface mpp_write_meta
        module procedure mpp_write_meta_global
        module procedure mpp_write_meta_global_scalar_r
        module procedure mpp_write_meta_global_scalar_i
        module procedure mpp_write_meta_var
        module procedure mpp_write_meta_scalar_r
        module procedure mpp_write_meta_scalar_i
        module procedure mpp_write_meta_axis_i1d
        module procedure mpp_write_meta_axis_r1d
        module procedure mpp_write_meta_axis_unlimited
        module procedure mpp_write_meta_field
    end interface mpp_write_meta

    interface mpp_write
        module procedure mpp_write_axis
        module procedure mpp_write_r0d
        module procedure mpp_write_r1d
        module procedure mpp_write_r2d
        module procedure mpp_write_r3d
        module procedure mpp_write_r4d
        module procedure mpp_write_2ddecomp_r2d
        module procedure mpp_write_2ddecomp_r3d
        module procedure mpp_write_2ddecomp_r4d
    end interface mpp_write

    interface mpp_write_compressed
        module procedure mpp_write_compressed_r1d
        module procedure mpp_write_compressed_r2d
        module procedure mpp_write_compressed_r3d
    end interface mpp_write_compressed

    interface mpp_io_unstructured_write
        module procedure mpp_io_unstructured_write_r1d
        module procedure mpp_io_unstructured_write_r2d
        module procedure mpp_io_unstructured_write_r3d
        module procedure mpp_io_unstructured_write_r4d
    end interface mpp_io_unstructured_write

    interface mpp_write_unlimited_axis
        module procedure mpp_write_unlimited_axis_r1d
    end interface

    interface mpp_read
        module procedure mpp_read_r0d
        module procedure mpp_read_r1d
        module procedure mpp_read_r2d
        module procedure mpp_read_r3d
        module procedure mpp_read_r4d
        module procedure mpp_read_region_r2d
        module procedure mpp_read_region_r3d
        module procedure mpp_read_text
        module procedure mpp_read_2ddecomp_r2d
        module procedure mpp_read_2ddecomp_r3d
        module procedure mpp_read_2ddecomp_r4d
    end interface

    interface mpp_read_compressed
        module procedure mpp_read_compressed_r1d
        module procedure mpp_read_compressed_r2d
        module procedure mpp_read_compressed_r3d
    end interface

    interface mpp_io_unstructured_read
        module procedure mpp_io_unstructured_read_r1d
        module procedure mpp_io_unstructured_read_r2d
        module procedure mpp_io_unstructured_read_r3d
    end interface

    interface mpp_read_distributed_ascii
        module procedure mpp_read_distributed_ascii_r1d
        module procedure mpp_read_distributed_ascii_i1d
        module procedure mpp_read_distributed_ascii_a1d
    end interface

    interface mpp_get_att_value
        module procedure mpp_get_field_att_text
    end interface

    interface mpp_file_is_opened
        module procedure mpp_file_is_opened_i
        module procedure mpp_file_is_opened_a
    end interface

    interface mpp_ascii_fread
        module procedure mpp_ascii_fread_i4_1d
        module procedure mpp_ascii_fread_r8_3d
        module procedure mpp_ascii_fread_r8_5d
    end interface

    !--------------------------------------------------------------------------
    !Parameters.
    integer(kind=c_int),parameter :: MPP_IO_MAX_NUM_NETCDF_FILES = 1024
    integer(kind=c_int),parameter :: MPP_IO_MAX_NUM_REGULAR_FILES = 1024
    integer(kind=c_int),parameter :: MPP_IO_MAX_NUM_HDF5_FILES = 4
    integer(kind=c_int),parameter :: MPP_IO_DEBUG = 1
    integer(kind=c_int),parameter :: MPP_IO_VERBOSE = 2
    integer(kind=c_int),parameter :: MPP_IO_MAX_NUM_FILES = 3072
    integer(kind=c_int),parameter :: MPP_NETCDF = 203
    integer(kind=c_int),parameter :: MPP_ASCII = 200
    integer(kind=c_int),parameter :: MPP_HDF5 = 204
    integer(kind=c_int),parameter :: MPP_IO_MAX_FILENAME_LEN = 512
    integer(kind=c_int),parameter :: MPP_RDONLY = 101
    integer(kind=c_int),parameter :: MPP_WRONLY = 100
    integer(kind=c_int),parameter :: MPP_APPEND = 102
    integer(kind=c_int),parameter :: MPP_OVERWR = 103
    integer(kind=c_int),parameter :: MPP_MULTI = 401
    integer(kind=c_int),parameter :: MPP_SINGLE = 400

    integer(kind=c_int),parameter :: GLOBAL_ROOT_ONLY = 2**9

    !Probably not needed.
    integer(kind=c_int),parameter :: MPP_DELETE = 501
    integer(kind=c_int),parameter :: MPP_IEEE32 = 201     !Possibly not needed.
    integer(kind=c_int),parameter :: MPP_NATIVE = 202     !Possibly not needed.
    integer(kind=c_int),parameter :: MPP_SEQUENTIAL = 300 !Possibly not needed.
    integer(kind=c_int),parameter :: MPP_DIRECT = 301     !Possibly not needed.
    integer(kind=c_int),parameter :: MPP_COLLECT = 502    !Possibly not needed.
    integer(kind=int64),parameter :: MAX_FILE_SIZE = 2147483647
    integer(kind=c_int) :: FILE_TYPE_USED

    !--------------------------------------------------------------------------
    !Legacy module variables.
    integer(kind=c_int) :: unit_begin = -1
    integer(kind=c_int) :: unit_end = -1
    logical(kind=c_bool) :: units_set = .false._c_bool
    integer(kind=c_int) :: maxunits = 1024
    type(atttype) :: default_att
    type(axistype) :: default_axis
    type(fieldtype) :: default_field

    !--------------------------------------------------------------------------
    !Namelist variables.
    integer(kind=c_int) :: header_buffer_val = 16384
    logical(kind=c_bool) :: global_field_on_root_pe = .true._c_bool
    logical(kind=c_bool):: io_clocks_on = .false._c_bool
    integer(kind=c_int) :: shuffle = 0
    integer(kind=c_int) :: deflate = 0
    integer(kind=c_int) :: deflate_level = -1
    logical(kind=c_bool) :: cf_compliance = .false._c_bool

    namelist /mpp_io_nml/ header_buffer_val, &
                          global_field_on_root_pe, &
                          io_clocks_on, &
                          shuffle, &
                          deflate, &
                          deflate_level, &
                          cf_compliance

    !--------------------------------------------------------------------------
    !Private derived type definitions.
    type netcdf_var_type
        integer(kind=c_int) :: var_index
        integer(kind=c_int),dimension(:),allocatable :: att_indices
    end type

    type parallel_io_type
        logical(kind=c_bool) :: in_use
        integer(kind=c_int) :: c_file_index
        integer(kind=c_int) :: file_type
        integer(kind=c_int),dimension(:),allocatable :: rank_list
        integer(kind=c_int) :: rank_list_root
        type(domain2D) :: domain
        type(domain2D) :: io_domain
        type(domainUG) :: domain_UG
        type(domainUG) :: io_domain_UG
        logical(kind=c_bool) :: is_writer
        logical(kind=c_bool) :: is_reader
        logical(kind=c_bool) :: data_in_multi_files
        logical(kind=c_bool) :: comm_required
        integer(kind=c_int) :: num_ranks_at_init
        integer(kind=c_int),dimension(:),allocatable :: global_att_indices
        integer(kind=c_int),dimension(:),allocatable :: dim_indices
        type(netcdf_var_type),dimension(:),allocatable :: vars
    end type

    type mpp_io_context_type
        type(parallel_io_type),dimension(:),allocatable :: files
        integer(kind=c_int) :: cur_num_files
    end type

    !--------------------------------------------------------------------------
    !Private module variables.
    logical(kind=c_bool) :: module_is_initialized = .false._c_bool
    type(mpp_io_context_type) :: context

    contains

#include "mpp_io_private_utils.inc"
#include "mpp_io_public_utils.inc"
#include "mpp_io_context_utils.inc"
#include "mpp_io_init.inc"
#include "mpp_io_exit.inc"
#include "mpp_open.inc"
#include "mpp_close.inc"
#include "mpp_write_meta_atts.inc"
#include "mpp_write_meta_axis.inc"
#include "mpp_write_meta_field.inc"
#include "mpp_write_axis.inc"
#include "mpp_write.inc"
#include "mpp_write_2ddecomp.inc"
#include "mpp_write_compressed.inc"
#include "mpp_io_unstructured_write.inc"
#include "mpp_write_unlimited_axis.inc"
#include "mpp_read_meta.inc"
#include "mpp_read.inc"
#include "mpp_read_compressed.inc"
#include "mpp_io_unstructured_read.inc"
#include "mpp_read_region.inc"
#include "mpp_read_2ddecomp.inc"

#include "mpp_get_global_atts.inc"
#include "mpp_get_info.inc"
#include "mpp_def_dim.inc"
#include "mpp_get_dimension_length.inc"
#include "mpp_get_file_name.inc"
#include "mpp_file_is_opened.inc"
#include "mpp_io_legacy_utils.inc"
#include "mpp_get_fields.inc"
#include "mpp_get_times.inc"
#include "mpp_get_axes.inc"
#include "mpp_get_axis_by_name.inc"
#include "mpp_get_field_att_text.inc"
#include "mpp_get_time_axis.inc"
#include "mpp_get_tavg_info.inc"
#include "mpp_flush.inc"

#include "mpp_io_deprecated_utils.inc"
#include "mpp_get_valid.inc"

!Not working
#include "mpp_read_distributed_ascii.inc"

end module mpp_io_mod
