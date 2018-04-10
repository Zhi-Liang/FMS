/*Header file which controls the use of MPI for the mpp_c library.*/

#ifndef SET_MPP_C_MPI_H_
#define SET_MPP_C_MPI_H_

#ifdef use_libMPI
#include <mpi.h>
#else
#include <stdlib.h>

#define MPI_INT32_T 0
#define MPI_UINT32_T 1
#define MPI_INT64_T 2
#define MPI_UINT64_T 3
#define MPI_FLOAT 4
#define MPI_DOUBLE 5
#define MPI_CHAR 6
#define MPI_COMM_NULL -1
#define MPI_GROUP_NULL -1
#define MPI_REQUEST_NULL -1
#define MPI_COMM_WORLD 0
#define MPI_SUCCESS 0
#define MPI_MAX 0
#define MPI_MIN 0
#define MPI_SUM 0
#define MPI_ANY_SOURCE 0
#define MPI_DUMMY_REQUEST 10

typedef int MPI_Datatype;
typedef int MPI_Comm;
typedef int MPI_Group;
typedef int MPI_Request;
typedef int MPI_Status;
typedef int MPI_Op;

struct sp_mesg {
    void *recv_data_ptr;
    int recv_count;
    MPI_Datatype recv_datatype;
    void *send_data_ptr;
    int send_count;
    MPI_Datatype send_datatype;
};

/*---------------------------------------------------------------------------*/
/*Determine if the struct sp_mesg object is initialized.*/
int sp_mesg_is_init(struct sp_mesg const * const self);

/*---------------------------------------------------------------------------*/
/*Abort if the struct sp_mesg object is not initialized.*/
void check_sp_mesg_init_state(struct sp_mesg const * const self,
                              char const *routine_name);

/*---------------------------------------------------------------------------*/
/*Return the appropriate size in bytes of the type specified by the inputted
  value.*/
size_t single_process_type_num_bytes(MPI_Datatype val);

/*---------------------------------------------------------------------------*/
/*Make sure a pointer is not null.*/
void check_pointer(void const * const ptr,
                   char const *routine_name);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Alltoall.*/
int MPI_Alltoall(const void *sendbuf,
                 int sendcount,
                 MPI_Datatype sendtype,
                 void *recvbuf,
                 int recvcount,
                 MPI_Datatype recvtype,
                 MPI_Comm comm);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Alltoallv.*/
int MPI_Alltoallv(const void *sendbuf,
                  const int sendcounts[],
                  const int sdispls[],
                  MPI_Datatype sendtype,
                  void *recvbuf,
                  const int recvcounts[],
                  const int rdispls[],
                  MPI_Datatype recvtype,
                  MPI_Comm comm);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Group_translate_ranks.*/
int MPI_Group_translate_ranks(MPI_Group group1,
                              int n,
                              const int ranks1[],
                              MPI_Group group2,
                              int ranks2[]);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Bcast.*/
int MPI_Bcast(void *buffer,
              int count,
              MPI_Datatype datatype,
              int root,
              MPI_Comm comm);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Abort.*/
int MPI_Abort(MPI_Comm comm,
              int errorcode);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Wtime.*/
double MPI_Wtime();

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Initialized.*/
int MPI_Initialized(int *ptr);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Init.*/
int MPI_Init(void *ptr1,
             void *ptr2);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Finalize.*/
int MPI_Finalize();

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Comm_rank.*/
int MPI_Comm_rank(MPI_Comm comm,
                  int *rank);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Comm_group.*/
int MPI_Comm_group(MPI_Comm comm,
                   MPI_Group *group);

/*---------------------------------------------------------------------------*/
/*Sincle process version of MPI_Comm_size.*/
int MPI_Comm_size(MPI_Comm comm,
                  int *size);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Wait.*/
int MPI_Wait(MPI_Request *request,
             MPI_Status *status);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Get_count.*/
int MPI_Get_count(const MPI_Status *status,
                  MPI_Datatype datatype,
                  int *count);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Allreduce.*/
int MPI_Allreduce(const void *sendbuf,
                  void *recvbuf,
                  int count,
                  MPI_Datatype datatype,
                  MPI_Op op,
                  MPI_Comm comm);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Group_incl.*/
int MPI_Group_incl(MPI_Group group,
                   int n,
                   const int ranks[],
                   MPI_Group *newgroup);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Comm_create.*/
int MPI_Comm_create(MPI_Comm comm,
                    MPI_Group group,
                    MPI_Comm *newcomm);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Comm_free.*/
int MPI_Comm_free(MPI_Comm *comm);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Group_free.*/
int MPI_Group_free(MPI_Group *group);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Recv.*/
int MPI_Recv(void *buf,
             int count,
             MPI_Datatype datatype,
             int source,
             int tag,
             MPI_Comm comm,
             MPI_Status *status);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Irecv.*/
int MPI_Irecv(void *buf,
              int count,
              MPI_Datatype datatype,
              int source,
              int tag,
              MPI_Comm comm,
              MPI_Request *request);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Isend.*/
int MPI_Isend(const void *buf,
              int count,
              MPI_Datatype datatype,
              int dest,
              int tag,
              MPI_Comm comm,
              MPI_Request *request);

/*---------------------------------------------------------------------------*/
/*Single process version of MPI_Barrier.*/
int MPI_Barrier(MPI_Comm comm);

/*---------------------------------------------------------------------------*/

#endif
#endif
