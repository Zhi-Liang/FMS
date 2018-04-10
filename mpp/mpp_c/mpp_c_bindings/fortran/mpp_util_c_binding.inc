 !*****************************************************************************
 !mpp_util interfaces which binds to c.

 !-----------------------------------------------------------------------------
 !mpp_init
  interface
      subroutine iso_c_mpp_init(mpp_shared,mpp_context,local_comm) &
      bind(C,name='mpp_c_init_wrap')
        use iso_c_binding, only: c_ptr,c_int
        implicit none
        type(c_ptr)                     :: mpp_shared
        type(c_ptr)                     :: mpp_context
        integer(c_int),value,intent(in) :: local_comm
      end subroutine iso_c_mpp_init
   end interface

 !-----------------------------------------------------------------------------
 !mpp_exit
  interface
      subroutine iso_c_mpp_exit(mpp_shared,mpp_context) &
      bind(C,name='mpp_c_exit_wrap')
        use iso_c_binding, only: c_ptr
        implicit none
        type(c_ptr) :: mpp_shared
        type(c_ptr) :: mpp_context
      end subroutine iso_c_mpp_exit
   end interface

 !-----------------------------------------------------------------------------
 !mpp_pe
  interface
      function iso_c_mpp_pe(mpp_context) &
      result(world_rank_id) &
      bind(C,name='mpp_c_pe')
        use iso_c_binding, only: c_ptr,c_int32_t
        implicit none
        type(c_ptr),value,intent(in) :: mpp_context
        integer(c_int32_t)           :: world_rank_id
      end function iso_c_mpp_pe
  end interface

 !-----------------------------------------------------------------------------
 !mpp_npes
  interface
      function iso_c_mpp_npes(mpp_context) &
      result(current_pelist_size) &
      bind(C,name='mpp_c_npes')
        use iso_c_binding, only: c_ptr,c_size_t
        implicit none
        type(c_ptr),value,intent(in) :: mpp_context
        integer(c_size_t)            :: current_pelist_size
      end function iso_c_mpp_npes
  end interface

 !-----------------------------------------------------------------------------
 !mpp_root_pe
  interface
      function iso_c_mpp_root_pe(mpp_context) &
      result(current_pelist_root_rank) &
      bind(C,name='mpp_c_root_pe')
        use iso_c_binding, only: c_ptr,c_int32_t
        implicit none
        type(c_ptr),value,intent(in) :: mpp_context
        integer(c_int32_t)           :: current_pelist_root_rank
      end function iso_c_mpp_root_pe
  end interface

 !-----------------------------------------------------------------------------
 !mpp_set_root_pe
  interface
      subroutine iso_c_mpp_set_root_pe(mpp_context,root_rank_id) &
      bind(C,name='mpp_c_set_root_pe')
        use iso_c_binding, only: c_ptr,c_int32_t
        implicit none
        type(c_ptr)                         :: mpp_context
        integer(c_int32_t),value,intent(in) :: root_rank_id
      end subroutine iso_c_mpp_set_root_pe
  end interface

 !-----------------------------------------------------------------------------
 !mpp_declare_pelist
  interface
      subroutine iso_c_mpp_declare_pelist(mpp_context,pelist_name, &
                                          pelist_name_len,pelist, &
                                          pelist_size) &
      bind(C,name='mpp_c_declare_pelist')
        use iso_c_binding, only: c_ptr,c_char,c_size_t,c_int32_t
        implicit none
        type(c_ptr)                                    :: mpp_context
        character(kind=c_char),dimension(*),intent(in) :: pelist_name
        integer(c_size_t),value,intent(in)             :: pelist_name_len
        integer(c_int32_t),dimension(*),intent(in)     :: pelist
        integer(c_size_t),value,intent(in)             :: pelist_size
      end subroutine iso_c_mpp_declare_pelist
  end interface

 !-----------------------------------------------------------------------------
 !mpp_get_current_pelist_name
  interface
      function iso_c_mpp_get_current_pelist_name(mpp_context) &
      result(current_pelist_name) &
      bind(C,name='mpp_c_get_current_pelist_name')
        use iso_c_binding, only: c_ptr
        implicit none
        type(c_ptr),value,intent(in) :: mpp_context
        type(c_ptr)                  :: current_pelist_name
      end function iso_c_mpp_get_current_pelist_name
  end interface

 !-----------------------------------------------------------------------------
 !mpp_get_current_pelist_name_len
  interface
      function iso_c_mpp_get_current_pelist_name_len(mpp_context) &
      result(current_pelist_name_len) &
      bind(C,name='mpp_c_get_current_pelist_name_len')
        use iso_c_binding, only: c_ptr,c_size_t
        implicit none
        type(c_ptr),value,intent(in)  :: mpp_context
        integer(c_size_t)             :: current_pelist_name_len
      end function iso_c_mpp_get_current_pelist_name_len
  end interface

 !-----------------------------------------------------------------------------
 !mpp_get_current_pelist
  interface
      function iso_c_mpp_get_current_pelist(mpp_context) &
      result(current_pelist) &
      bind(C,name='mpp_c_get_current_pelist')
        use iso_c_binding, only: c_ptr
        implicit none
        type(c_ptr),value,intent(in) :: mpp_context
        type(c_ptr)                  :: current_pelist
      end function iso_c_mpp_get_current_pelist
  end interface

 !-----------------------------------------------------------------------------
 !mpp_set_current_pelist
  interface
      subroutine iso_c_mpp_set_current_pelist(mpp_context,pelist,pelist_size, &
                                              no_sync_flag) &
      bind(C,name='mpp_c_set_current_pelist')
        use iso_c_binding, only: c_ptr,c_int32_t,c_size_t,c_int8_t
        implicit none
        type(c_ptr)                                :: mpp_context
        integer(c_int32_t),dimension(*),intent(in) :: pelist
        integer(c_size_t),value,intent(in)         :: pelist_size
        integer(c_int8_t),value,intent(in)         :: no_sync_flag
      end subroutine iso_c_mpp_set_current_pelist
  end interface

 !-----------------------------------------------------------------------------
 !mpp_get_current_pelist_comm_id
  interface
      function iso_c_mpp_get_current_pelist_comm_id(mpp_context) &
      result(current_pelist_comm_id) &
      bind(C,name='mpp_c_get_current_pelist_comm_id')
        use iso_c_binding, only: c_ptr,c_int
        implicit none
        type(c_ptr),value,intent(in) :: mpp_context
        integer(c_int)               :: current_pelist_comm_id
      end function iso_c_mpp_get_current_pelist_comm_id
  end interface

 !-----------------------------------------------------------------------------
 !mpp_clock_set_grain
  interface
      subroutine iso_c_mpp_clock_set_grain(mpp_context,grain) &
      bind(C,name='mpp_c_clock_set_grain')
        use iso_c_binding, only: c_ptr,c_int32_t
        implicit none
        type(c_ptr)                         :: mpp_context
        integer(c_int32_t),value,intent(in) :: grain
      end subroutine iso_c_mpp_clock_set_grain
  end interface

 !-----------------------------------------------------------------------------
 !mpp_clock_id
  interface
      function iso_c_mpp_clock_id(mpp_context,timer_name,timer_name_len, &
                                  grain,sync_flag,detail_flag) &
      result(timer_id) &
      bind(C,name='mpp_c_clock_id')
        use iso_c_binding, only: c_ptr,c_char,c_size_t,c_int32_t,c_int8_t
        implicit none
        type(c_ptr)                                    :: mpp_context
        character(kind=c_char),dimension(*),intent(in) :: timer_name
        integer(c_size_t),value,intent(in)             :: timer_name_len
        integer(c_int32_t),value,intent(in)            :: grain
        integer(c_int8_t),value,intent(in)             :: sync_flag
        integer(c_int8_t),value,intent(in)             :: detail_flag
        integer(c_int32_t)                             :: timer_id
      end function iso_c_mpp_clock_id
  end interface

 !-----------------------------------------------------------------------------
 !mpp_clock_begin
  interface
      subroutine iso_c_mpp_clock_begin(mpp_context,timer_id) &
      bind(C,name='mpp_c_clock_begin')
        use iso_c_binding, only: c_ptr,c_int32_t
        implicit none
        type(c_ptr)                         :: mpp_context
        integer(c_int32_t),value,intent(in) :: timer_id
      end subroutine iso_c_mpp_clock_begin
    end interface

 !-----------------------------------------------------------------------------
 !mpp_clock_end
  interface
      subroutine iso_c_mpp_clock_end(mpp_context,timer_id) &
      bind(C,name='mpp_c_clock_end')
        use iso_c_binding, only: c_ptr,c_int32_t
        implicit none
        type(c_ptr)                         :: mpp_context
        integer(c_int32_t),value,intent(in) :: timer_id
      end subroutine iso_c_mpp_clock_end
    end interface

 !-----------------------------------------------------------------------------
 !mpp_record_time_start
  interface
      subroutine iso_c_mpp_record_time_start(mpp_context) &
      bind(C,name='mpp_c_record_time_start')
        use iso_c_binding, only: c_ptr
        implicit none
        type(c_ptr) :: mpp_context
      end subroutine iso_c_mpp_record_time_start
    end interface

 !-----------------------------------------------------------------------------
 !mpp_record_time_end
  interface
      subroutine iso_c_mpp_record_time_end(mpp_context) &
      bind(C,name='mpp_c_record_time_end')
        use iso_c_binding, only: c_ptr
        implicit none
        type(c_ptr) :: mpp_context
      end subroutine iso_c_mpp_record_time_end
    end interface

 !-----------------------------------------------------------------------------
 !read_input_nml
  interface
      subroutine iso_c_read_input_nml(mpp_context) &
      bind(C,name='mpp_c_read_input_nml')
        use iso_c_binding, only: c_ptr
        implicit none
        type(c_ptr) :: mpp_context
      end subroutine iso_c_read_input_nml
    end interface

 !-----------------------------------------------------------------------------
 !mpp_get_namelist_buffer_len
  interface
      function iso_c_mpp_get_namelist_buffer_len(mpp_context) &
      result(namelist_buffer_len) &
      bind(C,name='mpp_c_get_namelist_buffer_len')
        use iso_c_binding, only: c_ptr,c_size_t
        implicit none
        type(c_ptr),value,intent(in) :: mpp_context
        integer(c_size_t)            :: namelist_buffer_len
      end function iso_c_mpp_get_namelist_buffer_len
  end interface

 !-----------------------------------------------------------------------------
 !mpp_get_namelist_buffer
  interface
      function iso_c_mpp_get_namelist_buffer(mpp_context) &
      result(namelist_buffer) &
      bind(C,name='mpp_c_get_namelist_buffer')
        use iso_c_binding, only: c_ptr
        implicit none
        type(c_ptr),value,intent(in) :: mpp_context
        type(c_ptr)                  :: namelist_buffer
      end function iso_c_mpp_get_namelist_buffer
  end interface

 !-----------------------------------------------------------------------------
 !mpp_get_input_nml_buffer_size
  interface
      function iso_c_mpp_get_input_nml_buffer_size(mpp_context) &
      result(input_nml_buffer_size) &
      bind(C,name='mpp_c_get_input_nml_buffer_size')
        use iso_c_binding, only: c_ptr,c_size_t
        implicit none
        type(c_ptr),value,intent(in) :: mpp_context
        integer(c_size_t)            :: input_nml_buffer_size
      end function iso_c_mpp_get_input_nml_buffer_size
  end interface

 !-----------------------------------------------------------------------------
 !mpp_get_input_nml_buffer
  interface
      function iso_c_mpp_get_input_nml_buffer(mpp_context) &
      result(input_nml_buffer) &
      bind(C,name='mpp_c_get_input_nml_buffer')
        use iso_c_binding, only: c_ptr
        implicit none
        type(c_ptr),value,intent(in) :: mpp_context
        type(c_ptr)                  :: input_nml_buffer
      end function iso_c_mpp_get_input_nml_buffer
  end interface

 !-----------------------------------------------------------------------------

 !*****************************************************************************
