 !*****************************************************************************
 !mpp_gather_pelist interface which binds to C.

#ifdef _INTEL16_FBINDINGS
 !-----------------------------------------------------------------------------
 !mpp_gather_pelist
  interface
      subroutine iso_c_mpp_gather_pelist(gather_data, &
                                         gather_data_x_size, &
                                         gather_data_y_size, &
                                         gather_data_z_size, &
                                         mpp_type, &
                                         array_segment, &
                                         gather_spot_x_start_index, &
                                         gather_spot_x_end_index, &
                                         gather_spot_y_start_index, &
                                         gather_spot_y_end_index, &
                                         gather_spot_z_start_index, &
                                         gather_spot_z_end_index, &
                                         pelist, &
                                         pelist_size, &
                                         pelist_root_flag, &
                                         root_gather_spot_x_offset, &
                                         root_gather_spot_y_offset, &
                                         root_gather_spot_z_offset, &
                                         mpp_context) &
      bind(C,name='mpp_c_gather_pelist_wrap')
        use iso_c_binding, only: c_size_t,c_int32_t,c_ptr
        implicit none
        type(*),dimension(..),intent(inout)        :: gather_data
        integer(c_size_t),value,intent(in)         :: gather_data_x_size
        integer(c_size_t),value,intent(in)         :: gather_data_y_size
        integer(c_size_t),value,intent(in)         :: gather_data_z_size
        integer(c_int32_t),value,intent(in)        :: mpp_type
        type(*),dimension(..),intent(in)           :: array_segment
        integer(c_int32_t),value,intent(in)        :: gather_spot_x_start_index
        integer(c_int32_t),value,intent(in)        :: gather_spot_x_end_index
        integer(c_int32_t),value,intent(in)        :: gather_spot_y_start_index
        integer(c_int32_t),value,intent(in)        :: gather_spot_y_end_index
        integer(c_int32_t),value,intent(in)        :: gather_spot_z_start_index
        integer(c_int32_t),value,intent(in)        :: gather_spot_z_end_index
        integer(c_int32_t),dimension(*),intent(in) :: pelist
        integer(c_size_t),value,intent(in)         :: pelist_size
        integer(c_int32_t),value,intent(in)        :: pelist_root_flag
        integer(c_int32_t),value,intent(in)        :: root_gather_spot_x_offset
        integer(c_int32_t),value,intent(in)        :: root_gather_spot_y_offset
        integer(c_int32_t),value,intent(in)        :: root_gather_spot_z_offset
        type(c_ptr)                                :: mpp_context
      end subroutine iso_c_mpp_gather_pelist
   end interface

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
