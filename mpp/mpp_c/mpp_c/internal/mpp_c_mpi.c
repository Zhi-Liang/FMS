#include "mpp_c_mpi.h"

#ifndef use_libMPI
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <execinfo.h>
#include <time.h>

static struct sp_mesg *sp_comm_ptr = NULL;

/*---------------------------------------------------------------------------*/
/*Determine if the struct sp_mesg object is initialized.*/
int sp_mesg_is_init(struct sp_mesg const * const self)
{
    /*Local variables*/
    int init_flag = 0; /*Initialization flag.*/

    if (self == NULL)
    {
        init_flag = 0;
    }
    else
    {
        init_flag = 1;
    }

    return init_flag;
}

/*---------------------------------------------------------------------------*/
/*Abort if the struct sp_mesg object is not initialized.*/
void check_sp_mesg_init_state(struct sp_mesg const * const self,
                              char const *routine_name)
{
    if (!sp_mesg_is_init(self))
    {
        fprintf(stderr,"Error: %s: you must first call MPI_Init.\n",
                routine_name);
        MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Return the appropriate size in bytes of the type specified by the inputted
  value.*/
size_t single_process_type_num_bytes(MPI_Datatype val)
{
    /*Local variables*/
    size_t num_bytes = 0; /*Size of type represented by val in bytes.*/

    /*Get the number of bytes for inputted val value.*/
    switch (val)
    {
        case MPI_INT32_T:
            num_bytes = 4;
            break;
        case MPI_UINT32_T:
            num_bytes = 4;
            break;
        case MPI_INT64_T:
            num_bytes = 8;
            break;
        case MPI_UINT64_T:
            num_bytes = 8;
            break;
        case MPI_FLOAT:
            num_bytes = 4;
            break;
        case MPI_DOUBLE:
            num_bytes = 8;
            break;
        case MPI_CHAR:
            num_bytes = 1;
            break;
        default:
            fprintf(stderr,"Error: single_process_type_num_bytes:"
                           " unsupported type.\n");
            MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
    }

    return num_bytes;
}

/*---------------------------------------------------------------------------*/
/*Make sure a pointer is not null.*/
void check_pointer(void const * const ptr,
                   char const *routine_name)
{
    if (ptr == NULL)
    {
        fprintf(stderr,"Error: %s: inputted pointer is null.\n",routine_name);
        MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Alltoall.*/
int MPI_Alltoall(const void *sendbuf,
                 int sendcount,
                 MPI_Datatype sendtype,
                 void *recvbuf,
                 int recvcount,
                 MPI_Datatype recvtype,
                 MPI_Comm comm __attribute__((unused)))
{
    /*Local variables*/
    size_t num_bytes = 0; /*Size of type represented by sendtype in bytes.*/

    check_sp_mesg_init_state(sp_comm_ptr,"MPI_Alltoall");
    check_pointer(sendbuf,"MPI_Alltoall");
    check_pointer(recvbuf,"MPI_Alltoall");
    if (sendcount != recvcount)
    {
        fprintf(stderr,"Error: MPI_Alltoall: the send and recv counts are"
                       " not equal.\n");
        MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
    }
    if (sendtype != recvtype)
    {
        fprintf(stderr,"Error: MPI_Alltoall: the send and recv data have"
                       " different types.\n");
        MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
    }
    num_bytes = single_process_type_num_bytes(sendtype);
    memcpy(recvbuf,sendbuf,num_bytes*recvcount);

    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Alltoallv.*/
int MPI_Alltoallv(const void *sendbuf,
                  const int sendcounts[],
                  const int sdispls[] __attribute__((unused)),
                  MPI_Datatype sendtype,
                  void *recvbuf,
                  const int recvcounts[],
                  const int rdispls[] __attribute__((unused)),
                  MPI_Datatype recvtype,
                  MPI_Comm comm __attribute__((unused)))
{
    /*Local variables*/
    size_t num_bytes = 0; /*Size of type represented by sendtype in bytes.*/

    check_sp_mesg_init_state(sp_comm_ptr,"MPI_Alltoallv");
    check_pointer(sendbuf,"MPI_Alltoallv");
    check_pointer(recvbuf,"MPI_Alltoallv");
    if (sendcounts[0] != recvcounts[0])
    {
        fprintf(stderr,"Error: MPI_Alltoallv: the send and recv counts are"
                       " not equal.\n");
        MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
    }
    if (sendtype != recvtype)
    {
        fprintf(stderr,"Error: MPI_Alltoallv: the send and recv data have"
                       " different types.\n");
        MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
    }
    num_bytes = single_process_type_num_bytes(sendtype);
    memcpy(recvbuf,sendbuf,num_bytes*recvcounts[0]);

    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Group_translate_ranks.*/
int MPI_Group_translate_ranks(MPI_Group group1 __attribute__((unused)),
                              int n __attribute__((unused)),
                              const int ranks1[],
                              MPI_Group group2 __attribute__((unused)),
                              int ranks2[])
{
    check_sp_mesg_init_state(sp_comm_ptr,"MPI_Group_translate_ranks");
    ranks2[0] = ranks1[0];

    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Bcast.*/
int MPI_Bcast(void *buffer,
              int count __attribute__((unused)),
              MPI_Datatype datatype __attribute__((unused)),
              int root __attribute__((unused)),
              MPI_Comm comm __attribute__((unused)))
{
    check_sp_mesg_init_state(sp_comm_ptr,"MPI_Bcast");
    check_pointer(buffer,"MPI_Bcast");
    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Abort.*/
int MPI_Abort(MPI_Comm comm __attribute__((unused)),
              int errorcode __attribute__((unused)))
{
    /*Local variables*/
    void *func_addresses[20];
    size_t num_funcs = 0;
    char **func_names = NULL;
    unsigned int i = 0;

    num_funcs = backtrace(func_addresses,20);
    func_names = backtrace_symbols(func_addresses,num_funcs);
    fprintf(stderr,"Program called MPI_Abort. Printing backtrace:\n");
    for (i=0;i<num_funcs;i++)
    {
        fprintf(stderr,"%s\n",func_names[i]);
    }
    exit(EXIT_FAILURE);

    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Wtime.*/
double MPI_Wtime()
{
    /*Local variables.*/
    double out_time = 0.0;
    struct timespec tv;
    int ierr = 0;

    ierr = clock_gettime(CLOCK_MONOTONIC,&tv);
    if (ierr != 0)
    {
        MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
    }
    out_time = (double)tv.tv_sec + ((double)1.e-9)*((double)tv.tv_nsec);

    return out_time;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Initialized.*/
int MPI_Initialized(int *ptr)
{
    if (sp_mesg_is_init(sp_comm_ptr))
    {
        *ptr = 1;
    }
    else
    {
        *ptr = 0;
    }

    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Init.*/
int MPI_Init(void *ptr1 __attribute__((unused)),
             void *ptr2 __attribute__((unused)))
{
    if (sp_mesg_is_init(sp_comm_ptr))
    {
        fprintf(stderr,"Error: MPI_Init: MPI has already been initialized.\n");
        MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
    }
    sp_comm_ptr = NULL;
    sp_comm_ptr = (struct sp_mesg *)malloc(sizeof(struct sp_mesg));
    sp_comm_ptr->recv_data_ptr = NULL;
    sp_comm_ptr->recv_count = 0;
    sp_comm_ptr->recv_datatype = 0;
    sp_comm_ptr->send_data_ptr = NULL;
    sp_comm_ptr->send_count = 0;
    sp_comm_ptr->send_datatype = 0;

    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Finalize.*/
int MPI_Finalize()
{
    check_sp_mesg_init_state(sp_comm_ptr,"MPI_Finalize");
    sp_comm_ptr->recv_data_ptr = NULL;
    if (sp_comm_ptr->send_data_ptr != NULL)
    {
        free((void *)(sp_comm_ptr->send_data_ptr));
        sp_comm_ptr->send_data_ptr = NULL;
    }
    sp_comm_ptr->recv_count = 0;
    sp_comm_ptr->recv_datatype = 0;
    sp_comm_ptr->send_count = 0;
    sp_comm_ptr->send_datatype = 0;
    free((void *)sp_comm_ptr);
    sp_comm_ptr = NULL;

    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Comm_rank.*/
int MPI_Comm_rank(MPI_Comm comm __attribute__((unused)),
                  int *rank)
{
    check_sp_mesg_init_state(sp_comm_ptr,"MPI_Comm_rank");
    *rank = 0;

    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Comm_group.*/
int MPI_Comm_group(MPI_Comm comm __attribute__((unused)),
                   MPI_Group *group)
{
    check_sp_mesg_init_state(sp_comm_ptr,"MPI_Comm_group");
    *group = 0;

    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Sincle process version of MPI_Comm_size.*/
int MPI_Comm_size(MPI_Comm comm __attribute__((unused)),
                  int *size)
{
    check_sp_mesg_init_state(sp_comm_ptr,"MPI_Comm_size");
    *size = 1;

    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Wait.*/
int MPI_Wait(MPI_Request *request,
             MPI_Status *status __attribute__((unused)))
{
    check_sp_mesg_init_state(sp_comm_ptr,"MPI_Wait");
    if (*request != MPI_DUMMY_REQUEST)
    {
        fprintf(stderr,"Error: MPI_Wait: invalid request.\n");
        MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
    }

    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Get_count.*/
int MPI_Get_count(const MPI_Status *status __attribute__((unused)),
                  MPI_Datatype datatype __attribute__((unused)),
                  int *count)
{
    check_sp_mesg_init_state(sp_comm_ptr,"MPI_Get_count");
    *count = sp_comm_ptr->recv_count;

    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Allreduce.*/
int MPI_Allreduce(const void *sendbuf,
                  void *recvbuf,
                  int count,
                  MPI_Datatype datatype,
                  MPI_Op op __attribute__((unused)),
                  MPI_Comm comm __attribute__((unused)))
{
    /*Local variables*/
    size_t num_bytes = 0; /*Size of type represented by datatype in bytes.*/

    check_sp_mesg_init_state(sp_comm_ptr,"MPI_Allreduce");
    check_pointer(sendbuf,"MPI_Allreduce");
    check_pointer(recvbuf,"MPI_Allreduce");
    num_bytes = single_process_type_num_bytes(datatype);
    memcpy(recvbuf,sendbuf,num_bytes*count);

    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Group_incl.*/
int MPI_Group_incl(MPI_Group group,
                   int n __attribute__((unused)),
                   const int ranks[] __attribute__((unused)),
                   MPI_Group *newgroup)
{
    check_sp_mesg_init_state(sp_comm_ptr,"MPI_Group_incl");
    *newgroup = group;

    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Comm_create.*/
int MPI_Comm_create(MPI_Comm comm,
                    MPI_Group group __attribute__((unused)),
                    MPI_Comm *newcomm)
{
    check_sp_mesg_init_state(sp_comm_ptr,"MPI_Comm_create");
    *newcomm = comm;

    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Comm_free.*/
int MPI_Comm_free(MPI_Comm *comm __attribute__((unused)))
{
    check_sp_mesg_init_state(sp_comm_ptr,"MPI_Comm_free");
    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Group_free.*/
int MPI_Group_free(MPI_Group *group __attribute__((unused)))
{
    check_sp_mesg_init_state(sp_comm_ptr,"MPI_Group_free");
    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Recv.*/
int MPI_Recv(void *buf,
             int count,
             MPI_Datatype datatype,
             int source __attribute__((unused)),
             int tag __attribute__((unused)),
             MPI_Comm comm __attribute__((unused)),
             MPI_Status *status __attribute__((unused)))
{
    /*Local variables*/
    size_t num_bytes = 0; /*Size of type represented by dataytype in bytes.*/

    check_sp_mesg_init_state(sp_comm_ptr,"MPI_Recv");
    if (sp_comm_ptr->recv_data_ptr != NULL)
    {
        fprintf(stderr,"Error: MPI_Recv: there is currently an outstanding"
                       " receive request.\n");
        MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
    }
    if (sp_comm_ptr->send_data_ptr == NULL)
    {
        fprintf(stderr,"Error: MPI_Recv: there is no data to receive."
                       " Please first post a send request.\n");
        MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
    }
    if (count != sp_comm_ptr->send_count)
    {
        fprintf(stderr,"Error: MPI_Recv: there is a size mismatch between"
                       " the sent and receive data counts.\n");
        MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
    }
    if (datatype != sp_comm_ptr->send_datatype)
    {
        fprintf(stderr,"Error: MPI_Recv: the send and recv datatypes do not"
                       " match.\n");
        MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
    }

    num_bytes = single_process_type_num_bytes(datatype);
    memcpy(buf,sp_comm_ptr->send_data_ptr,num_bytes*count);
    sp_comm_ptr->recv_data_ptr = NULL;
    free(sp_comm_ptr->send_data_ptr);
    sp_comm_ptr->send_data_ptr = NULL;

    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Irecv.*/
int MPI_Irecv(void *buf,
              int count,
              MPI_Datatype datatype,
              int source __attribute__((unused)),
              int tag __attribute__((unused)),
              MPI_Comm comm __attribute__((unused)),
              MPI_Request *request)
{
    /*Local variables*/
    size_t num_bytes = 0;
    check_sp_mesg_init_state(sp_comm_ptr,"MPI_Irecv");
    check_pointer(buf,"MPI_Irecv");

    if (sp_comm_ptr->recv_data_ptr != NULL)
    {
        fprintf(stderr,"Error: MPI_Irecv: there is currently an"
                       " outstanding receive request.\n");
        MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
    }

    sp_comm_ptr->recv_count = count;
    *request = MPI_DUMMY_REQUEST;

    if (sp_comm_ptr->send_data_ptr == NULL)
    {
        sp_comm_ptr->recv_data_ptr = buf;
        sp_comm_ptr->recv_datatype = datatype;
    }
    else
    {
        if (count != sp_comm_ptr->send_count)
        {
            fprintf(stderr,"Error: MPI_Irecv: the send and recv counts do not"
                           " match.\n");
            MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
        }
        if (datatype != sp_comm_ptr->send_datatype)
        {
            fprintf(stderr,"Error: MPI_Irecv: the send and recv datatypes do"
                       " not match.\n");
            MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
        }
        num_bytes = single_process_type_num_bytes(datatype);
        memcpy(buf,sp_comm_ptr->send_data_ptr,num_bytes*count);
        sp_comm_ptr->recv_data_ptr = NULL;
        free(sp_comm_ptr->send_data_ptr);
        sp_comm_ptr->send_data_ptr = NULL;
    }

    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Isend.*/
int MPI_Isend(const void *buf,
              int count,
              MPI_Datatype datatype,
              int dest __attribute__((unused)),
              int tag __attribute__((unused)),
              MPI_Comm comm __attribute__((unused)),
              MPI_Request *request)
{
    /*Local variables*/
    size_t num_bytes = 0; /*Size of type represented by dataytype in bytes.*/

    check_sp_mesg_init_state(sp_comm_ptr,"MPI_Isend");
    check_pointer(buf,"MPI_Isend");

    if (sp_comm_ptr->send_data_ptr != NULL)
    {
        fprintf(stderr,"Error: MPI_Isend: there is currently an"
                       " outstanding send request.\n");
        MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
    }

    sp_comm_ptr->send_count = count;
    *request = MPI_DUMMY_REQUEST;

    if (sp_comm_ptr->recv_data_ptr == NULL)
    {
        num_bytes = single_process_type_num_bytes(datatype);
        sp_comm_ptr->send_data_ptr = malloc(num_bytes*count);
        memcpy(sp_comm_ptr->send_data_ptr,buf,num_bytes*count);
        sp_comm_ptr->send_datatype = datatype;
    }
    else
    {
        if (count != sp_comm_ptr->recv_count)
        {
            fprintf(stderr,"Error: MPI_Isend: the send and recv counts do not"
                           " match.\n");
            MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
        }
        if (datatype != sp_comm_ptr->recv_datatype)
        {
            fprintf(stderr,"Error: MPI_Isend: the send and recv datatypes do"
                       " not match.\n");
            MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
        }
        num_bytes = single_process_type_num_bytes(datatype);
        memcpy(sp_comm_ptr->recv_data_ptr,buf,num_bytes*count);
        sp_comm_ptr->recv_data_ptr = NULL;
        sp_comm_ptr->send_data_ptr = NULL;
    }

    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Barrier.*/
int MPI_Barrier(MPI_Comm comm __attribute__((unused)))
{
    check_sp_mesg_init_state(sp_comm_ptr,"MPI_Barrier");
    return MPI_SUCCESS;
}

/*---------------------------------------------------------------------------*/

#endif
