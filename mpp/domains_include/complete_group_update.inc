nsend = group%nsend
nrecv = group%nrecv
gridtype = group%gridtype
update_flags = group%flags_v
nscalar = group%nscalar
nvector = group%nvector
if(nvector > 0) recv = group%recv_v
buffer_start_pos = group%buffer_start_pos

if(nrecv>0) then
   call mpp_clock_begin(group_wait_clock)
   call mpp_sync_self(check=EVENT_RECV, request=group%request_recv(1:nrecv), &
                        msg_size=group%recv_size(1:nrecv), msg_type=group%type_recv(1:nrecv))
   call mpp_clock_end(group_wait_clock)
endif

!---unpack the buffer
nunpack = group%nunpack
call mpp_clock_begin(group_unpk_clock)
if( group%k_loop_inside ) then
!$OMP parallel do default(none) shared(nunpack,group,nscalar,nvector,buffer_start_pos,buffer,sHold,xHold,yHold) &
!$OMP                           private(buffer_pos,pos,m,is, ie, js, je,rotation,n,k,ke)
   do n = nunpack, 1, -1
      buffer_pos = group%unpack_buffer_pos(n) + buffer_start_pos
      pos = buffer_pos
      is = group%unpack_is(n); ie = group%unpack_ie(n)
      js = group%unpack_js(n); je = group%unpack_je(n)
      if( group%unpack_type(n) == FIELD_S ) then
         ke = group%ksize_s
         do l=1, nscalar
            call unpack_buffer(group%isize_s, group%jsize_s, sHold(l,1)%data, buffer, pos, is, ie, js, je, ke)
         enddo
      else if( group%unpack_type(n) == FIELD_X ) then
         ke = group%ksize_v
         do l=1, nvector
            call unpack_buffer(group%isize_x, group%jsize_x, xHold(l,1)%data, buffer, pos, is, ie, js, je, ke)
         enddo
      else if( group%unpack_type(n) == FIELD_Y ) then
         ke = group%ksize_v
         do l=1, nvector
            call unpack_buffer(group%isize_y, group%jsize_y, yHold(l,1)%data, buffer, pos, is, ie, js, je, ke)
         enddo
      endif
   enddo
endif
call mpp_clock_end(group_unpk_clock)

! ---northern boundary fold
if(nvector > 0 .AND. BTEST(domain%fold,NORTH) .AND. (.NOT.BTEST(update_flags,SCALAR_BIT)) ) then
   l_size = nvector
   tMe = 1
   ke = group%ksize_v
#include "update_fold.inc"
endif

if(nsend>0) then
   call mpp_clock_begin(group_wait_clock)
   call mpp_sync_self(check=EVENT_SEND, request=group%request_send(1:nsend) )
   call mpp_clock_end(group_wait_clock)
endif
