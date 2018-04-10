 !*****************************************************************************
 !mpp_scatter_pelist interface which binds to C.

#ifdef _GNU_FBINDINGS
 !-----------------------------------------------------------------------------
 !mpp_scatter_pelist
  interface
      subroutine iso_c_mpp_scatter_pelist_array(scatter_data, &
                                                scatter_data_x_size, &
                                                scatter_data_y_size, &
                                                scatter_data_z_size, &
                                                mpp_type, &
                                                array_segment, &
                                                scatter_spot_x_start_index, &
                                                scatter_spot_x_end_index, &
                                                scatter_spot_y_start_index, &
                                                scatter_spot_y_end_index, &
                                                scatter_spot_z_start_index, &
                                                scatter_spot_z_end_index, &
                                                pelist, &
                                                pelist_size, &
                                                pelist_root_flag, &
                                                root_scatter_spot_x_offset, &
                                                root_scatter_spot_y_offset, &
                                                root_scatter_spot_z_offset, &
                                                mpp_context) &
      bind(C,name='mpp_c_scatter_pelist_array_wrap')
        use iso_c_binding, only: c_size_t,c_int32_t,c_ptr
        implicit none
        type(*),dimension(*),intent(in)            :: scatter_data
        integer(c_size_t),value,intent(in)         :: scatter_data_x_size
        integer(c_size_t),value,intent(in)         :: scatter_data_y_size
        integer(c_size_t),value,intent(in)         :: scatter_data_z_size
        integer(c_int32_t),value,intent(in)        :: mpp_type
        type(*),dimension(*),intent(inout)         :: array_segment
        integer(c_int32_t),value,intent(in)        :: scatter_spot_x_start_index
        integer(c_int32_t),value,intent(in)        :: scatter_spot_x_end_index
        integer(c_int32_t),value,intent(in)        :: scatter_spot_y_start_index
        integer(c_int32_t),value,intent(in)        :: scatter_spot_y_end_index
        integer(c_int32_t),value,intent(in)        :: scatter_spot_z_start_index
        integer(c_int32_t),value,intent(in)        :: scatter_spot_z_end_index
        integer(c_int32_t),dimension(*),intent(in) :: pelist
        integer(c_size_t),value,intent(in)         :: pelist_size
        integer(c_int32_t),value,intent(in)        :: pelist_root_flag
        integer(c_int32_t),value,intent(in)        :: root_scatter_spot_x_offset
        integer(c_int32_t),value,intent(in)        :: root_scatter_spot_y_offset
        integer(c_int32_t),value,intent(in)        :: root_scatter_spot_z_offset
        type(c_ptr)                                :: mpp_context
      end subroutine iso_c_mpp_scatter_pelist_array
   end interface

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
