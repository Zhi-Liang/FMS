/* *********************************************************************************** */
// \Author Tom Robinson (Some help from Garrett Wright)
// This program is the C part of the json routines that are run in cjson_fortran_mod. 
// Each function accepts the name of the json file, the length of the json file, the 
// name of namelist, and the name of the variable and returns the value of the variable
// of inquiry.  Seeing as how Tom Robinson is not a C programmer, this code could 
// probably use a knowlegable set of eyes to make it run "correctly".
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <float.h>
#include <limits.h>
#include <ctype.h>
#include "cJSON.h"
#include "error_cjson.h"

/** This function parses the json file and returns the C-pointer to the parsed JSON */
/*cJSON * parseJson (char * my_json_string)
{
   cJSON * root = cJSON_Parse(my_json_string); // Parse json file
  if (!root){
     exit (1);
//     errorMEShandle();
  }
 return root;
}
*/
/** This function check to see if the namelist and variable exist in the json */
void json_check_for_variable (char * my_json_string, char * nml_name, char * var_name, int* nmlexist, int* varexist)
{
  *nmlexist=0; *varexist = 0;
  int i;
   cJSON * root = cJSON_Parse(my_json_string); // Parse json file
  if (!root){
     errorhandle();
  }
 cJSON * nml = cJSON_GetObjectItem(root,nml_name); // Find the namelist
    if (!nml)
    {
     *nmlexist = -1;
     *varexist = -1;
    }
    else
    {
     *nmlexist = 1; //Set the flag for the namelist existing
     for (i=0; i<cJSON_GetArraySize(nml); i++ )  {
        cJSON * member = cJSON_GetArrayItem(nml,i); // Find the variable 
        if (!member){
                int ivalue=errorvar ();
                    }
        cJSON * var = cJSON_GetObjectItem(member,var_name);
        if (var){*varexist=1;}
                                                 }
    }
cJSON_Delete(root);

}

/* ********************************************************************************************* */
/* Finds the length of the json array */ 
int nml_length (cJSON * root, cJSON * nml)
{
  int ii = cJSON_GetArraySize(nml); // Find the size of the array "var_name"
  return ii;
}
/* ********************************************************************************************* */
/* ********************************************************************************************* */
// Returns a scalar integer
int integer_val (cJSON * boot, char * my_json_string, char * nml_name , char * var_name)
{
 int i;
 int ivalue = -1;
 cJSON * root = cJSON_Parse(my_json_string); // Parse json file
  if (!root){
     i=errorhandle();
  }
 cJSON * nml = cJSON_GetObjectItem(root,nml_name); // Find the namelist
    if (!nml)
    {
     i=errornml ();
    }
 for (i=0; i<cJSON_GetArraySize(nml); i++ )  {
        cJSON * member = cJSON_GetArrayItem(nml,i); // Find the variable 
        if (!member){
                ivalue=errorvar ();
        }
        cJSON * var = cJSON_GetObjectItem(member,var_name);
        if (var){                       
                ivalue = cJSON_GetObjectItem(member,var_name)->valueint;
        }
                        }
//   if (ivalue == -1){ivalue = errorMESintegernml (var_name,nml_name,nml,ivalue);}
 cJSON_Delete (root);
 return ivalue;
}
// ///////////////////////////////////////////////////////////////////////////////////////////// //
// Returns a scalar double
double real_val (cJSON * boot, char * my_json_string, char * nml_name , char * var_name)
{
 int i;
 int ierr;
 double rvalue;
 cJSON * root = cJSON_Parse(my_json_string); // Parse json file
  if (!root){
     ierr=errorhandle();
  }
 cJSON * nml = cJSON_GetObjectItem(root,nml_name); // Find the namelist
    if (!nml)
    {
     ierr =errornml ();
    }
 for (i=0; i<cJSON_GetArraySize(nml); i++ )  {
        cJSON * member = cJSON_GetArrayItem(nml,i); // Find the variable 
        if (!member){
                ierr=errorvar ();
        }
        cJSON * var = cJSON_GetObjectItem(member,var_name);
        if (var){                       
                rvalue = cJSON_GetObjectItem(member,var_name)->valuedouble;
        }
                        }
 cJSON_Delete (root);
 return rvalue;
}
// Returns a scalar character array
// ///////////////////////////////////////////////////////////////////////////////////////////// //
char * string_val (cJSON * boot, char * my_json_string, char * nml_name , char * var_name)
{
 int i; int ierr; char * svalue;
 cJSON * root = cJSON_Parse(my_json_string); // Parse json file
  if (!root){
     ierr=errorhandle();
  }
 cJSON * nml = cJSON_GetObjectItem(root,nml_name); // Find the namelist
    if (!nml)
    {
     ierr=errornml ();
    }
 for (i=0; i<cJSON_GetArraySize(nml); i++ )  {
        cJSON * member = cJSON_GetArrayItem(nml,i); // Find the variable 
        if (!member){
                ierr=errorvar ();
        }
        cJSON * var = cJSON_GetObjectItem(member,var_name);
        if (var){                       
                svalue = cJSON_GetObjectItem(member,var_name)->valuestring;
        }
                        }
 char string[strlen(svalue)];
 strcpy(string,svalue);
 char * s = malloc( sizeof(char) * ( strlen(svalue) + 1 ) );
 strcpy(s,svalue);
 cJSON_Delete (root);

 return s;
}
/* ********************************************************************************************* */
/* ****************************************** ARRAYS ******************************************* */
// Get the length of the array variable
int array_length (char * my_json_string, char * nml_name , char * var_name)
{
 int i; int ierr; int ii = 0;
 cJSON * root = cJSON_Parse(my_json_string); // Parse json file
// cJSON * root = boot ;
  if (!root){
     ierr=errorhandle();
  }
  cJSON * nml = cJSON_GetObjectItem(root,nml_name); // Find the namelist
    if (!nml)
    {
     ierr=errornml ();
    }

 for (i=0; i<cJSON_GetArraySize(nml); i++ )  {

        cJSON * member = cJSON_GetArrayItem(nml,i); // Find the variable 

        if (!member){
                ierr=errorvar ();
        }
        else {
        cJSON * var = cJSON_GetObjectItem(member,var_name); 
         if (var){ii = cJSON_GetArraySize(var);} // Find the size of the array "var_name"
        }
  }
  cJSON_Delete (root);
  if (ii == 0) {printf("FATAL: \n");exit (1);}
  return ii;
}
// ///////////////////////////////////////////////////////////////////////////////////////////// //
int integer_array (cJSON * boot, char * my_json_string, char * nml_name , char * var_name, int array_index)
{
 int i;
 int integer;
 cJSON * root = cJSON_Parse(my_json_string); // Parse json file
// cJSON * root = boot ;
  if (!root){
     i=errorhandle();
  }
  cJSON * nml = cJSON_GetObjectItem(root,nml_name); // Find the namelist
    if (!nml)
    {
     i=errornml ();
    }

 for (i=0; i<cJSON_GetArraySize(nml); i++ )  {

        cJSON * member = cJSON_GetArrayItem(nml,i); // Find the variable 

        if (!member){
                i=errorvar ();
        }
        else {
        cJSON * var = cJSON_GetObjectItem(member,var_name);     
                if (var){     
                        integer = cJSON_GetArrayItem(var,array_index)->valueint; // Find the value of the array at "array_index"
                }
        }
                        }
 cJSON_Delete (root);
 return integer;
}

// ///////////////////////////////////////////////////////////////////////////////////////////// //
double real_array (cJSON * boot, char * my_json_string, char * nml_name , char * var_name, int array_index)
{
 int i;
 double r;
 cJSON * root = cJSON_Parse(my_json_string); // Parse json file
// cJSON * root = boot ;
  if (!root){
     i=errorhandle();
  }
  cJSON * nml = cJSON_GetObjectItem(root,nml_name); // Find the namelist
    if (!nml)
    {
     i=errornml ();
    }

 for (i=0; i<cJSON_GetArraySize(nml); i++ )  {

        cJSON * member = cJSON_GetArrayItem(nml,i); // Find the variable 

        if (!member){
                i=errorvar ();
        }
        else {
        cJSON * var = cJSON_GetObjectItem(member,var_name);     
                if (var){     
                        r = cJSON_GetArrayItem(var,array_index)->valuedouble; // Find the value of the array at "array_index"
                }
        }
                        }
 cJSON_Delete (root);
 return r;
}
// ///////////////////////////////////////////////////////////////////////////////////////////// //
char * string_array (cJSON * boot, char * my_json_string, char * nml_name , char * var_name, int array_index)
{
 int i;
 char * svalue;
 cJSON * root = cJSON_Parse(my_json_string); // Parse json file
// cJSON * root = boot ;
  if (!root){
     i=errorhandle();
  }
  cJSON * nml = cJSON_GetObjectItem(root,nml_name); // Find the namelist
    if (!nml)
    {
     i=errornml ();
    }

 for (i=0; i<cJSON_GetArraySize(nml); i++ )  {

        cJSON * member = cJSON_GetArrayItem(nml,i); // Find the variable 

        if (!member){
                i=errorvar ();
        }
        else {
        cJSON * var = cJSON_GetObjectItem(member,var_name);     
                if (var){     
                        svalue = cJSON_GetArrayItem(var,array_index)->valuestring; // Find the value of the array at "array_index"
                }
        }
                        }


 char string[strlen(svalue)];
 strcpy(string,svalue);
 char * s = malloc( sizeof(char) * ( strlen(svalue) + 1 ) );
 strcpy(s,svalue);
 cJSON_Delete (root);

 return s;

}
/* ********************************************************************************************* */
/* ********************************************************************************************* */
/* ********************************************************************************************* */
/* ********************************************************************************************* */
int errorhandle ()
{
         int i;
         char partE[101];
         char message [200];
         char *error = (char*)cJSON_GetErrorPtr(); //error is the json file where the syntax error occurred
         strcpy(message,"JSON file has a syntax error at or just before: \n");
         for (i=0; i <100; i++)
         {
           partE[i] = error[i];
         }
           partE[100] = '\0';
           strcat(message,partE);
         error_mesg_cjson("namelist_array.c",message,0);
         fprintf(stdout,"FATAL: JSON - JSON file has a syntax error at or just before: \n %s \n",partE);
         exit (1);
   return 1;
}

int errornml (char * nml_name)
{
 char errormes [100];
 
 char * mesg1 = "The namelist "; 
 char * mesg2 = " has a syntax error or is not present in the JSON."; 
 strcpy(errormes,mesg1); 
 strcat(errormes,nml_name);
 strcat(errormes,mesg2);
  error_mesg_cjson("namelist_array.c",errormes,0);

 exit (1);
 return 1;
}
int errorvar (char * nml_name, char * var_name)
{
 char * mesg1 = "The variable ";
 char * mesg2 = " in ";
 char * mesg3 = " is not formatted correctly in the json.";
 
 char errormes [100];
 strcpy(errormes,mesg1); 
 strcat(errormes,var_name);
 strcat(errormes,mesg2);
 strcat(errormes,nml_name);
 strcat(errormes,mesg3);
  error_mesg_cjson("namelist_array.c",errormes,0);
 exit (1);
 return 1;
}
