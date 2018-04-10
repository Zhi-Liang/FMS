/** @file */
/*Header file containing macro definitions for the mpp_c library.*/

#ifndef SET_MPP_C_MACROS_H_
#define SET_MPP_C_MACROS_H_

/*Data type parameters*/
#define MPP_INT32 10000
#define MPP_UINT32 10001
#define MPP_INT64 10002
#define MPP_UINT64 10003
#define MPP_REAL32 10004
#define MPP_REAL64 10005
#define MPP_CHAR 10006

/*Event type parameters*/
#define EVENT_SEND 4
#define EVENT_RECV 3
#define EVENT_BCAST 2
#define EVENT_REDUCE 1
#define EVENT_WAIT 5
#define EVENT_ALLTOALL 6
#define EVENT_START_TIMER 7
#define EVENT_STOP_TIMER 8

/*Timer type parameters*/
#define MPP_C_RUNTIME_TIMER 0
#define MPP_C_COMPONENT_TIMER 1
#define MPP_C_SUBCOMPONENT_TIMER 2
#define MPP_C_MODULE_DRIVER_TIMER 3
#define MPP_C_MODULE_TIMER 4
#define MPP_C_ROUTINE_TIMER 5
#define MPP_C_LOOP_TIMER 6
#define MPP_C_INFRASTRUCTURE_TIMER 7

/*Default communication tags*/
#define MPP_C_GATHER_TAG 5
#define MPP_C_GATHERV_TAG 6
#define MPP_C_GATHER_PELIST_TAG 7
#define MPP_C_SCATTER_PELIST_TAG 8

/*Maximum allowed string lengths (including the trailing null)*/
#define MPP_C_EVENT_NAME_LENGTH 17
#define MPP_C_TIMER_NAME_LENGTH 49
#define MPP_C_MAX_PELIST_NAME_LENGTH 33
#define MPP_C_MAX_ERROR_MESG_LENGTH 1025
#define MPP_C_MAX_ROUTINE_NAME_LENGTH 64
#define MPP_C_LOGFILE_NAME_LENGTH 48
#define MPP_C_ETCFILE_NAME_LENGTH 48
#define MPP_C_EVENT_SUMMARY_FILE_NAME_LENGTH 48
#define MPP_C_MAX_NML_FILE_NAME_LENGTH 5120

/*Maximum allowed array lengths*/
#define MPP_C_MAX_ARRAY_SIZE 100000000
#define MPP_C_MAX_NUM_TIMERS 200
#define MPP_C_MAX_REQUESTS 1000
#define MPP_C_MAX_NUM_PELISTS 32
#define MPP_C_MAX_EVENTS 6000
#define MPP_C_NUM_EVENT_TYPES 8
#define MPP_C_MAX_BINS 20

/*"Null" parameters*/
#define MPP_C_NULL_TIMER_GRAIN -1
#define MPP_C_NULL_TIMER_ID -1
#define MPP_C_NULL_PELIST 4294967295
#define MPP_C_NULL_PELIST_SIZE -1
#define MPP_C_NULL_REQUEST -1
#define MPP_C_NULL_MESG_SIZE 4294967295
#define MPP_C_NULL_MESG_TYPE -1
#define MPP_C_NULL_EVENT -1

/*Miscellaneous*/
#define MPP_C_TRUE 1
#define MPP_C_FALSE 0
#define MPP_C_WORLD_ROOT 0
#define FATAL 0
#define WARNING 1
#define NOTE 2
#define ANY_RANK -1
#define MPP_C_TIMER_GRAIN_TOO_FINE -1
#define MPP_C_NO_MATCH 4294967295
#define MPP_C_NML_PARSE_SUCCESS 1
#define MPP_C_NML_PARSE_FAILURE 0
#define MPP_C_WORK_BUFFER_INIT_SIZE 1024
#define MPP_C_WORK_BUFFER_MAX_SIZE 5242880
#define MPP_C_NAMELIST_BUFFER_MAX_SIZE 5242880

#endif
