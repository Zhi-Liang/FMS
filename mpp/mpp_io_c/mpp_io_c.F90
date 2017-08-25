module mpp_io_c
    use iso_c_binding, only: c_int, &
                             c_size_t
    implicit none
    private

#include "cmpp_io_macros.h"

    public :: MPP_IO_C_INDEX_NOT_FOUND
    public :: MPP_IO_C_GLOBAL_ATT
    public :: MPP_IO_C_KEEP_FILE
    public :: MPP_IO_C_MAX_ATTS_PER_VAR
    public :: MPP_IO_C_MAX_DIMS_PER_FILE
    public :: MPP_IO_C_MAX_DIMS_PER_VAR
    public :: MPP_IO_C_MAX_GLOBAL_ATTS_PER_FILE
    public :: MPP_IO_C_MAX_METADATA_BYTES_PER_FILE
    public :: MPP_IO_C_MAX_VARS_PER_FILE
    public :: MPP_IO_C_NC_BYTE
    public :: MPP_IO_C_NC_CHAR
    public :: MPP_IO_C_NC_DOUBLE
    public :: MPP_IO_C_NC_FLOAT
    public :: MPP_IO_C_NC_INT
    public :: MPP_IO_C_NC_SHORT
    public :: MPP_IO_C_NC_UNLIMITED
    public :: MPP_IO_C_CHAR
    public :: MPP_IO_C_INT
    public :: MPP_IO_C_FLOAT
    public :: MPP_IO_C_DOUBLE

    public :: mpp_io_c_strlen
    public :: mpp_io_c_init
    public :: mpp_io_c_finalize

    !Regular files.
    public :: mpp_io_c_open_regular_file
    public :: mpp_io_c_close_regular_file
    public :: mpp_io_c_flush_regular_file
    public :: mpp_io_c_get_regular_file_name
    public :: mpp_io_c_get_regular_file_is_open

    !Netcdf files.
    public :: mpp_io_c_open_netcdf_file
    public :: mpp_io_c_close_netcdf_file
    public :: mpp_io_c_flush_netcdf_file
    public :: mpp_io_c_fread_regular_file
    public :: mpp_io_c_read_netcdf_attribute
    public :: mpp_io_c_write_netcdf_attribute
    public :: mpp_io_c_free_netcdf_attribute
    public :: mpp_io_c_get_netcdf_attribute_index
    public :: mpp_io_c_get_netcdf_attribute_name
    public :: mpp_io_c_get_netcdf_attribute_type_in_file
    public :: mpp_io_c_get_netcdf_attribute_type_in_mem
    public :: mpp_io_c_get_netcdf_attribute_num_values
    public :: mpp_io_c_get_netcdf_attribute_values
    public :: mpp_io_c_read_netcdf_dimension
    public :: mpp_io_c_write_netcdf_dimension
    public :: mpp_io_c_advance_netcdf_dimension_level
    public :: mpp_io_c_free_netcdf_dimension
    public :: mpp_io_c_get_netcdf_dimension_index
    public :: mpp_io_c_get_netcdf_dimension_name
    public :: mpp_io_c_get_netcdf_dimension_length
    public :: mpp_io_c_get_netcdf_dimension_is_unlimited
    public :: mpp_io_c_get_netcdf_dimension_current_level
    public :: mpp_io_c_get_netcdf_dimension_id
    public :: mpp_io_c_read_netcdf_variable_metadata
    public :: mpp_io_c_write_netcdf_variable_metadata
    public :: mpp_io_c_read_netcdf_variable_data
    public :: mpp_io_c_write_netcdf_variable_data
    public :: mpp_io_c_buffer_netcdf_variable_data
    public :: mpp_io_c_write_buffered_netcdf_variable_data
    public :: mpp_io_c_free_netcdf_variable
    public :: mpp_io_c_get_netcdf_variable_index
    public :: mpp_io_c_get_netcdf_variable_is_data_buffered
    public :: mpp_io_c_get_netcdf_variable_max_num_attributes
    public :: mpp_io_c_get_netcdf_variable_name
    public :: mpp_io_c_get_netcdf_variable_type_in_file
    public :: mpp_io_c_get_netcdf_variable_num_dimensions
    public :: mpp_io_c_get_netcdf_variable_num_attributes
    public :: mpp_io_c_get_netcdf_variable_id
    public :: mpp_io_c_get_netcdf_variable_buffered_data
    public :: mpp_io_c_get_netcdf_variable_corner_indices
    public :: mpp_io_c_get_netcdf_variable_edge_lengths
    public :: mpp_io_c_get_netcdf_variable_type_in_mem
    public :: mpp_io_c_get_netcdf_file_name
    public :: mpp_io_c_get_netcdf_file_is_open
    public :: mpp_io_c_nc_inq
    public :: mpp_io_c_get_file_index

    !Utility routines.
    interface
        function mpp_io_c_strlen(ptr) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_ptr, &
                                     c_size_t
            implicit none
            type(c_ptr),value,intent(in) :: ptr
            integer(kind=c_size_t) :: res
        end function mpp_io_c_strlen
    end interface

    !Library state routines.
    interface
        subroutine mpp_io_c_init(max_num_netcdf_files, &
                                 max_num_regular_files, &
                                 max_num_hdf5_files, &
                                 debug_flag, &
                                 verbose_flag, &
                                 header_buffer_val, &
                                 shuffle, &
                                 deflate, &
                                 deflate_level) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_bool
            implicit none
            integer(kind=c_int),value,intent(in) :: max_num_netcdf_files
            integer(kind=c_int),value,intent(in) :: max_num_regular_files
            integer(kind=c_int),value,intent(in) :: max_num_hdf5_files
            logical(kind=c_bool),value,intent(in) :: debug_flag
            logical(kind=c_bool),value,intent(in) :: verbose_flag
            integer(kind=c_int),value,intent(in) :: header_buffer_val
            integer(kind=c_int),value,intent(in) :: shuffle
            integer(kind=c_int),value,intent(in) :: deflate
            integer(kind=c_int),value,intent(in) :: deflate_level
        end subroutine mpp_io_c_init
    end interface

    interface
        subroutine mpp_io_c_finalize() &
            bind(c)
            implicit none
        end subroutine mpp_io_c_finalize
    end interface

    !Regular file routines.
    interface
        function mpp_io_c_open_regular_file(fname, &
                                            f_action) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_char, &
                                     c_int
            implicit none
            character(kind=c_char,len=1),dimension(*),intent(in) :: fname
            integer(kind=c_int),value,intent(in) :: f_action
            integer(kind=c_int) :: res
        end function mpp_io_c_open_regular_file
    end interface

    interface
        subroutine mpp_io_c_close_regular_file(file_index, &
                                               action, &
                                               do_warn) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_bool
            implicit none
            integer(kind=c_int),intent(inout) :: file_index
            integer(kind=c_int),value,intent(in) :: action
            logical(kind=c_bool),value,intent(in) :: do_warn
        end subroutine mpp_io_c_close_regular_file
    end interface

    interface
        subroutine mpp_io_c_flush_regular_file(file_index) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
        end subroutine mpp_io_c_flush_regular_file
    end interface

    interface
        subroutine mpp_io_c_fread_regular_file(file_index, &
                                               num_vals, &
                                               data_type, &
                                               val_max_width, &
                                               delimiter, &
                                               data_buf, &
                                               last_read) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_char, &
                                     c_ptr, &
                                     c_bool
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: num_vals
            integer(kind=c_int),value,intent(in) :: data_type
            integer(kind=c_int),value,intent(in) :: val_max_width
            character(kind=c_char,len=1),value,intent(in) :: delimiter
            type(c_ptr) :: data_buf
            logical(kind=c_bool),value,intent(in) :: last_read
        end subroutine mpp_io_c_fread_regular_file
    end interface

    interface
        function mpp_io_c_get_regular_file_name(file_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_ptr
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            type(c_ptr) :: res
        end function mpp_io_c_get_regular_file_name
    end interface

    interface mpp_io_c_get_regular_file_is_open
        function mpp_io_c_get_regular_file_is_open_by_index(file_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_bool
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            logical(kind=c_bool) :: res
        end function mpp_io_c_get_regular_file_is_open_by_index

        function mpp_io_c_get_regular_file_is_open_by_name(file_name) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_char, &
                                     c_bool
            implicit none
            character(kind=c_char,len=1),dimension(*),intent(in) :: file_name
            logical(kind=c_bool) :: res
        end function mpp_io_c_get_regular_file_is_open_by_name
    end interface

    !Netcdf file routines.
    interface
        function mpp_io_c_open_netcdf_file(fname, &
                                           f_action, &
                                           max_num_global_attributes, &
                                           max_num_dimensions, &
                                           max_num_variables, &
                                           max_num_attributes_per_variable, &
                                           max_num_dimensions_per_variable, &
                                           max_metadata_num_bytes) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_char, &
                                     c_int, &
                                     c_size_t
            implicit none
            character(kind=c_char,len=1),dimension(*),intent(in) :: fname
            integer(kind=c_int),value,intent(in) :: f_action
            integer(kind=c_int),value,intent(in) :: max_num_global_attributes
            integer(kind=c_int),value,intent(in) :: max_num_dimensions
            integer(kind=c_int),value,intent(in) :: max_num_variables
            integer(kind=c_int),value,intent(in) :: max_num_attributes_per_variable
            integer(kind=c_int),value,intent(in) :: max_num_dimensions_per_variable
            integer(kind=c_size_t),value,intent(in) :: max_metadata_num_bytes
            integer(kind=c_int) :: res
        end function mpp_io_c_open_netcdf_file
    end interface

    interface
        subroutine mpp_io_c_close_netcdf_file(file_index, &
                                              action, &
                                              do_warn) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_bool
            implicit none
            integer(kind=c_int),intent(inout) :: file_index
            integer(kind=c_int),value,intent(in) :: action
            logical(kind=c_bool),value,intent(in) :: do_warn
        end subroutine mpp_io_c_close_netcdf_file
    end interface

    interface
        subroutine mpp_io_c_flush_netcdf_file(file_index) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
        end subroutine mpp_io_c_flush_netcdf_file
    end interface

    interface
        function mpp_io_c_read_netcdf_attribute(file_index, &
                                                variable_index, &
                                                netcdf_attribute_id) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            integer(kind=c_int),value,intent(in) :: netcdf_attribute_id
            integer(kind=c_int) :: res
        end function mpp_io_c_read_netcdf_attribute
    end interface

    interface
        function mpp_io_c_write_netcdf_attribute(file_index, &
                                                 variable_index, &
                                                 type_in_file, &
                                                 num_values, &
                                                 name, &
                                                 values, &
                                                 type_in_mem, &
                                                 buffer_values) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_size_t, &
                                     c_char, &
                                     c_ptr, &
                                     c_bool
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            integer(kind=c_int),value,intent(in) :: type_in_file
            integer(kind=c_size_t),value,intent(in) :: num_values
            character(kind=c_char,len=1),dimension(*),intent(in) :: name
            type(c_ptr),value,intent(in) :: values
            integer(kind=c_int),value,intent(in) :: type_in_mem
            logical(kind=c_bool),value,intent(in) :: buffer_values
            integer(kind=c_int) :: res
        end function mpp_io_c_write_netcdf_attribute
    end interface

    interface
        subroutine mpp_io_c_free_netcdf_attribute(file_index, &
                                                  variable_index, &
                                                  attribute_index) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            integer(kind=c_int),intent(inout) :: attribute_index
        end subroutine mpp_io_c_free_netcdf_attribute
    end interface

    interface
        function mpp_io_c_get_netcdf_attribute_index(file_index, &
                                                     variable_index, &
                                                     att_name) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_ptr
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            type(c_ptr),value,intent(in) :: att_name
            integer(kind=c_int) :: res
        end function mpp_io_c_get_netcdf_attribute_index
    end interface

    interface
        function mpp_io_c_get_netcdf_attribute_name(file_index, &
                                                    variable_index, &
                                                    attribute_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_ptr
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            integer(kind=c_int),value,intent(in) :: attribute_index
            type(c_ptr) :: res
        end function mpp_io_c_get_netcdf_attribute_name
    end interface

    interface
        function mpp_io_c_get_netcdf_attribute_type_in_file(file_index, &
                                                            variable_index, &
                                                            attribute_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            integer(kind=c_int),value,intent(in) :: attribute_index
            integer(c_int) :: res
        end function mpp_io_c_get_netcdf_attribute_type_in_file
    end interface

    interface
        function mpp_io_c_get_netcdf_attribute_type_in_mem(file_index, &
                                                           variable_index, &
                                                           attribute_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            integer(kind=c_int),value,intent(in) :: attribute_index
            integer(c_int) :: res
        end function mpp_io_c_get_netcdf_attribute_type_in_mem
    end interface

    interface
        function mpp_io_c_get_netcdf_attribute_num_values(file_index, &
                                                          variable_index, &
                                                          attribute_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_size_t
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            integer(kind=c_int),value,intent(in) :: attribute_index
            integer(kind=c_size_t) :: res
        end function mpp_io_c_get_netcdf_attribute_num_values
    end interface

    interface
        function mpp_io_c_get_netcdf_attribute_values(file_index, &
                                                      variable_index, &
                                                      attribute_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_ptr
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            integer(kind=c_int),value,intent(in) :: attribute_index
            type(c_ptr) :: res
        end function mpp_io_c_get_netcdf_attribute_values
    end interface

    interface
        function mpp_io_c_read_netcdf_dimension(file_index, &
                                                netcdf_dimension_id) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: netcdf_dimension_id
            integer(c_int) :: res
        end function mpp_io_c_read_netcdf_dimension
    end interface

    interface
        function mpp_io_c_write_netcdf_dimension(file_index, &
                                                 name, &
                                                 length, &
                                                 buffer_values) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_char, &
                                     c_size_t, &
                                     c_bool
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            character(kind=c_char,len=1),dimension(*),intent(in) :: name
            integer(kind=c_size_t),value,intent(in) :: length
            logical(kind=c_bool),value,intent(in) :: buffer_values
            integer(c_int) :: res
        end function mpp_io_c_write_netcdf_dimension
    end interface

    interface
        subroutine mpp_io_c_advance_netcdf_dimension_level(file_index, &
                                                           dimension_index) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: dimension_index
        end subroutine mpp_io_c_advance_netcdf_dimension_level
    end interface

    interface
        subroutine mpp_io_c_free_netcdf_dimension(file_index, &
                                                  dimension_index) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),intent(inout) :: dimension_index
        end subroutine mpp_io_c_free_netcdf_dimension
    end interface

    interface mpp_io_c_get_netcdf_dimension_index
        function mpp_io_c_get_netcdf_dimension_index_global_p(file_index, &
                                                              dim_name_ptr) &
            result(res) &
            bind(c,name="mpp_io_c_get_netcdf_dimension_index_global")
            use iso_c_binding, only: c_int, &
                                     c_ptr
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            type(c_ptr),value,intent(in) :: dim_name_ptr
            integer(kind=c_int) :: res
        end function mpp_io_c_get_netcdf_dimension_index_global_p

        function mpp_io_c_get_netcdf_dimension_index_variable(file_index, &
                                                              variable_index, &
                                                              dimension_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            integer(kind=c_int),value,intent(in) :: dimension_index
            integer(kind=c_int) :: res
        end function mpp_io_c_get_netcdf_dimension_index_variable
    end interface

    interface
        function mpp_io_c_get_netcdf_dimension_name(file_index, &
                                                    dimension_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_ptr
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: dimension_index
            type(c_ptr) :: res
        end function mpp_io_c_get_netcdf_dimension_name
    end interface

    interface
        function mpp_io_c_get_netcdf_dimension_length(file_index, &
                                                      dimension_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_size_t
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: dimension_index
            integer(kind=c_size_t) :: res
        end function mpp_io_c_get_netcdf_dimension_length
    end interface

    interface
        function mpp_io_c_get_netcdf_dimension_is_unlimited(file_index, &
                                                            dimension_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_bool
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: dimension_index
            logical(kind=c_bool) :: res
        end function mpp_io_c_get_netcdf_dimension_is_unlimited
    end interface

    interface
        function mpp_io_c_get_netcdf_dimension_current_level(file_index, &
                                                             dimension_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: dimension_index
            integer(kind=c_int) :: res
        end function mpp_io_c_get_netcdf_dimension_current_level
    end interface

    interface
        function mpp_io_c_get_netcdf_dimension_id(file_index, &
                                                  dimension_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: dimension_index
            integer(kind=c_int) :: res
        end function mpp_io_c_get_netcdf_dimension_id
    end interface

    interface
        function mpp_io_c_read_netcdf_variable_metadata(file_index, &
                                                        netcdf_variable_id) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: netcdf_variable_id
            integer(kind=c_int) :: res
        end function mpp_io_c_read_netcdf_variable_metadata
    end interface

    interface
        function mpp_io_c_write_netcdf_variable_metadata(file_index, &
                                                         type_in_file, &
                                                         name, &
                                                         dimension_indices, &
                                                         num_dimensions, &
                                                         max_num_attributes, &
                                                         buffer_metadata) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_char, &
                                     c_bool
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: type_in_file
            character(kind=c_char,len=1),dimension(*),intent(in) :: name
            integer(kind=c_int),dimension(*),intent(in) :: dimension_indices
            integer(kind=c_int),value,intent(in) :: num_dimensions
            integer(kind=c_int),value,intent(in) :: max_num_attributes
            logical(kind=c_bool),value,intent(in) :: buffer_metadata
            integer(kind=c_int) :: res
        end function mpp_io_c_write_netcdf_variable_metadata
    end interface

    interface
        subroutine mpp_io_c_read_netcdf_variable_data(file_index, &
                                                      variable_index, &
                                                      corner_indices, &
                                                      edge_lengths, &
                                                      data, &
                                                      type_in_mem) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_size_t, &
                                     c_ptr
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            integer(kind=c_size_t),dimension(*),intent(in) :: corner_indices
            integer(kind=c_size_t),dimension(*),intent(in) :: edge_lengths
            type(c_ptr),intent(inout) :: data
            integer(kind=c_int),value,intent(in) :: type_in_mem
        end subroutine mpp_io_c_read_netcdf_variable_data
    end interface

    interface
        subroutine mpp_io_c_write_netcdf_variable_data(file_index, &
                                                       variable_index, &
                                                       corner_indices, &
                                                       edge_lengths, &
                                                       data, &
                                                       type_in_mem) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_size_t, &
                                     c_ptr
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            integer(kind=c_size_t),dimension(*),intent(in) :: corner_indices
            integer(kind=c_size_t),dimension(*),intent(in) :: edge_lengths
            type(c_ptr),value,intent(in) :: data
            integer(kind=c_int),value,intent(in) :: type_in_mem
        end subroutine mpp_io_c_write_netcdf_variable_data
    end interface

    interface
        subroutine mpp_io_c_buffer_netcdf_variable_data(file_index, &
                                                        variable_index, &
                                                        corner_indices, &
                                                        edge_lengths, &
                                                        data, &
                                                        type_in_mem, &
                                                        overwrite) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_size_t, &
                                     c_ptr, &
                                     c_bool
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            integer(kind=c_size_t),dimension(*),intent(in) :: corner_indices
            integer(kind=c_size_t),dimension(*),intent(in) :: edge_lengths
            type(c_ptr),value,intent(in) :: data
            integer(kind=c_int),value,intent(in) :: type_in_mem
            logical(kind=c_bool),value,intent(in) :: overwrite
        end subroutine mpp_io_c_buffer_netcdf_variable_data
    end interface

    interface
        subroutine mpp_io_c_write_buffered_netcdf_variable_data(file_index, &
                                                                variable_index, &
                                                                free_data) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_bool
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            logical(kind=c_bool),value,intent(in) :: free_data
        end subroutine mpp_io_c_write_buffered_netcdf_variable_data
    end interface

    interface
        subroutine mpp_io_c_free_netcdf_variable(file_index, &
                                                 variable_index) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),intent(inout) :: variable_index
        end subroutine mpp_io_c_free_netcdf_variable
    end interface

    interface
        function mpp_io_c_get_netcdf_variable_index(file_index, &
                                                    var_name_ptr) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_ptr
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            type(c_ptr),value,intent(in) :: var_name_ptr
            integer(kind=c_int) :: res
        end function mpp_io_c_get_netcdf_variable_index
    end interface

    interface
        function mpp_io_c_get_netcdf_variable_is_data_buffered(file_index, &
                                                               variable_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_bool
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            logical(kind=c_bool) :: res
        end function mpp_io_c_get_netcdf_variable_is_data_buffered
    end interface

    interface
        function mpp_io_c_get_netcdf_variable_max_num_attributes(file_index, &
                                                                 variable_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            integer(kind=c_int) :: res
        end function mpp_io_c_get_netcdf_variable_max_num_attributes
    end interface

    interface
        function mpp_io_c_get_netcdf_variable_name(file_index, &
                                                   variable_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_ptr
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            type(c_ptr) :: res
        end function mpp_io_c_get_netcdf_variable_name
    end interface

    interface
        function mpp_io_c_get_netcdf_variable_type_in_file(file_index, &
                                                           variable_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            integer(kind=c_int) :: res
        end function mpp_io_c_get_netcdf_variable_type_in_file
    end interface

    interface
        function mpp_io_c_get_netcdf_variable_num_dimensions(file_index, &
                                                             variable_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            integer(kind=c_int) :: res
        end function mpp_io_c_get_netcdf_variable_num_dimensions
    end interface

    interface
        function mpp_io_c_get_netcdf_variable_num_attributes(file_index, &
                                                             variable_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            integer(kind=c_int) :: res
        end function mpp_io_c_get_netcdf_variable_num_attributes
    end interface

    interface
        function mpp_io_c_get_netcdf_variable_id(file_index, &
                                                 variable_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            integer(kind=c_int) :: res
        end function mpp_io_c_get_netcdf_variable_id
    end interface

    interface
        function mpp_io_c_get_netcdf_variable_buffered_data(file_index, &
                                                            variable_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_ptr
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            type(c_ptr) :: res
        end function mpp_io_c_get_netcdf_variable_buffered_data
    end interface

    interface
        function mpp_io_c_get_netcdf_variable_corner_indices(file_index, &
                                                             variable_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_ptr
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            type(c_ptr) :: res
        end function mpp_io_c_get_netcdf_variable_corner_indices
    end interface

    interface
        function mpp_io_c_get_netcdf_variable_edge_lengths(file_index, &
                                                           variable_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_ptr
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            type(c_ptr) :: res
        end function mpp_io_c_get_netcdf_variable_edge_lengths
    end interface

    interface
        function mpp_io_c_get_netcdf_variable_type_in_mem(file_index, &
                                                          variable_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),value,intent(in) :: variable_index
            integer(kind=c_int) :: res
        end function mpp_io_c_get_netcdf_variable_type_in_mem
    end interface

    interface
        function mpp_io_c_get_netcdf_file_name(file_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_ptr
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            type(c_ptr) :: res
        end function mpp_io_c_get_netcdf_file_name
    end interface

    interface mpp_io_c_get_netcdf_file_is_open
        function mpp_io_c_get_netcdf_file_is_open_by_index(file_index) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_int, &
                                     c_bool
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            logical(kind=c_bool) :: res
        end function mpp_io_c_get_netcdf_file_is_open_by_index

        function mpp_io_c_get_netcdf_file_is_open_by_name(file_name) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_char, &
                                     c_bool
            implicit none
            character(kind=c_char,len=1),dimension(*),intent(in) :: file_name
            logical(kind=c_bool) :: res
        end function mpp_io_c_get_netcdf_file_is_open_by_name
    end interface

    interface
        subroutine mpp_io_c_nc_inq(file_index, &
                                   num_global_atts, &
                                   num_dims, &
                                   num_vars) &
            bind(c)
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int),value,intent(in) :: file_index
            integer(kind=c_int),intent(out) :: num_global_atts
            integer(kind=c_int),intent(out) :: num_dims
            integer(kind=c_int),intent(out) :: num_vars
        end subroutine mpp_io_c_nc_inq
    end interface

    interface
        function mpp_io_c_get_file_index(file_name, &
                                         file_type) &
            result(res) &
            bind(c)
            use iso_c_binding, only: c_char, &
                                     c_int
            implicit none
            character(kind=c_char,len=1),dimension(*),intent(in) :: file_name
            integer(kind=c_int),value,intent(in) :: file_type
            integer(kind=c_int) :: res
        end function mpp_io_c_get_file_index
    end interface

    !Parameters.
    integer(kind=c_int),parameter :: MPP_IO_C_INDEX_NOT_FOUND = CMPP_IO_INDEX_NOT_FOUND
    integer(kind=c_int),parameter :: MPP_IO_C_GLOBAL_ATT = CMPP_IO_GLOBAL_ATT
    integer(kind=c_int),parameter :: MPP_IO_C_KEEP_FILE = CMPP_KEEP_FILE
    integer(kind=c_int),parameter :: MPP_IO_C_MAX_ATTS_PER_VAR = CMPP_IO_MAX_ATTS_PER_VAR
    integer(kind=c_int),parameter :: MPP_IO_C_MAX_DIMS_PER_FILE = CMPP_IO_MAX_DIMS_PER_FILE
    integer(kind=c_int),parameter :: MPP_IO_C_MAX_DIMS_PER_VAR = CMPP_IO_MAX_DIMS_PER_VAR
    integer(kind=c_int),parameter :: MPP_IO_C_MAX_GLOBAL_ATTS_PER_FILE = CMPP_IO_MAX_GLOBAL_ATTS_PER_FILE
    integer(kind=c_size_t),parameter :: MPP_IO_C_MAX_METADATA_BYTES_PER_FILE = CMPP_IO_MAX_METADATA_BYTES_PER_FILE
    integer(kind=c_int),parameter :: MPP_IO_C_MAX_VARS_PER_FILE = CMPP_IO_MAX_VARS_PER_FILE
    integer(kind=c_int),parameter :: MPP_IO_C_NC_BYTE = CMPP_IO_NC_BYTE
    integer(kind=c_int),parameter :: MPP_IO_C_NC_CHAR = CMPP_IO_NC_CHAR
    integer(kind=c_int),parameter :: MPP_IO_C_NC_DOUBLE = CMPP_IO_NC_DOUBLE
    integer(kind=c_int),parameter :: MPP_IO_C_NC_FLOAT = CMPP_IO_NC_FLOAT
    integer(kind=c_int),parameter :: MPP_IO_C_NC_INT = CMPP_IO_NC_INT
    integer(kind=c_int),parameter :: MPP_IO_C_NC_SHORT = CMPP_IO_NC_SHORT
    integer(kind=c_int),parameter :: MPP_IO_C_NC_UNLIMITED = CMPP_IO_NC_UNLIMITED
    integer(kind=c_int),parameter :: MPP_IO_C_CHAR = CMPP_IO_CHAR
    integer(kind=c_int),parameter :: MPP_IO_C_INT = CMPP_IO_INT
    integer(kind=c_int),parameter :: MPP_IO_C_FLOAT = CMPP_IO_FLOAT
    integer(kind=c_int),parameter :: MPP_IO_C_DOUBLE = CMPP_IO_DOUBLE

end module mpp_io_c
