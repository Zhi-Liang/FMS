#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <float.h>
#include <limits.h>
#include <ctype.h>
#include "error_cjson.h"
#include <mpi.h>

int ierr;
int rank;
/*!
   \author Tom Robinson thomas.robinson@noaa.gov
   \date July 2016
   \brief This function handles the error messages for the C side of the cjson routines. 
**/
void error_mesg_cjson (char * errorloc, char * message, int level)
{
  int ierr = MPI_Comm_rank(MPI_COMM_WORLD, &rank);
printf("The level is %d \n",level);
  if (level == NOTE && rank == 0) {
     printf("NOTE from PE 0: %s - %s \n",errorloc,message);
				  }
  else if (level == NOTE && rank != 0) {}
  else if (level == WARNING) {
     printf("WARNING from PE %d: %s - %s \n",rank,errorloc,message);
                             }
  else if (level == FATAL) {
     printf("FATAL from PE %d: %s - %s \n",rank,errorloc,message);
     ierr = MPI_ABORT (MPI_COMM_WORLD,999);
                           }
}
