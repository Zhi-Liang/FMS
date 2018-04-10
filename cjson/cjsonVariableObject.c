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
cJSON * parseJson (char * my_json_string)
{
   cJSON * root = cJSON_Parse(my_json_string); // Parse json file
  if (!root){
     errorMEShandle();
  }
 return root;
}

/** This function check to see if the namelist exists */
int namelist_exist (char * my_json_string, char * nml_name)
{
   cJSON * root = cJSON_Parse(my_json_string); // Parse json file
  if (!root){
     errorhandle();
  }
 cJSON * nml = cJSON_GetObjectItem(root,nml_name); // Find the namelist
    if (!nml)
    {
     return -1;
    }
   return 1;
}


/** This function parses the json file and finds the nml (used for variables that are scalars)*/
cJSON * scalarVal (cJSON * root, char * nml_name , char * var_name, char * attribute)
{

 cJSON * nml = cJSON_GetObjectItem(root,nml_name); // Find the namelist
    if (!nml)     
    {
     errorMESnml (nml_name);
    }
 cJSON * var = cJSON_GetObjectItem(nml,var_name); // Find the variable 
    if (!var)  
    {
     errorMESvar (var_name,nml_name);
    }
 cJSON * val = cJSON_GetObjectItem(var,attribute);
    if (!val)  
    {
     errorMESval (var_name,nml_name,attribute);
    }
  

  return var; // Return the nml to get the variable value (used with cJSON_GetObjectItem)
}
/** This function parses the json file and finds the var (used for json arrays)*/
cJSON * arrayVal (cJSON * root, char * nml_name , char * var_name, char * attribute)
{

 cJSON * nml = cJSON_GetObjectItem(root,nml_name); // Find the namelist
    if (!nml)     
    {
     errorMESnml (nml_name);
    }
 cJSON * var = cJSON_GetObjectItem(nml,var_name); // Find the variable 
    if (!var)  
    {
     errorMESvar (var_name,nml_name);
    }
 cJSON * val = cJSON_GetObjectItem(var,attribute);
    if (!val)  
    {
     errorMESval (var_name,nml_name,attribute);
    }

  return val;// Return the var to get the variable value from an array (Used with cJSON_GetArrayItem)
}

int existval (cJSON * root, char * nml_name , char * var_name, char * attribute)
{
    int fv = 1;
 cJSON * nml = cJSON_GetObjectItem(root,nml_name); // Find the namelist
    if (!nml)     
    {
     errorMESnml (nml_name);
    }
 cJSON * var = cJSON_GetObjectItem(nml,var_name); // Find the variable 
    if (!var)  
    {
     errorMESvar (var_name,nml_name);
    }
 cJSON * val = cJSON_GetObjectItem(var,attribute);
    if (!val)  
    {
     fv = 0;
    }
  

  return fv; 
}

int whatlevel (cJSON * root, char * nml_name , char * ob_name, char * var_name, char * attribute)
{
    int fv = 100; int i;
 cJSON * nml = cJSON_GetObjectItem(root,nml_name); // Find the namelist
    if (!nml)     
    {
     errorMESnml (nml_name);
    }
 cJSON * ob  = cJSON_levelObjectItem(nml,ob_name); // Find the variable 
 cJSON * var = cJSON_levelObjectItem(ob,var_name);
 cJSON * val = cJSON_levelObjectItem(var,attribute);

  if (!val)
        {fv=3;} // The variable is an Object in the namelist
/*  if (arvar)
        {fv=2;} // The variable is a key in the namelist 
*/
  if (!val && !var && !ob )
        {fv=1;} // The variable does not exist in the namelist


 int sizenml = cJSON_GetArraySize(nml); 
 if (sizenml>0) {
     for (i=0; i<cJSON_GetArraySize(nml); i++ )  {

        cJSON * member = cJSON_GetArrayItem(nml,i); // Find the variable 

        if (!member){
               error_mesg_cjson("whatlevel","The namelist array is not set up correctly",0); 
                    }
        else        {
               cJSON * arvar = cJSON_GetObjectItem(member,ob_name); 
  if (arvar)
        {fv=2;} // The variable is a key in the namelist
                    }
                                                 }
                    }
  return fv; 


}

/* ********************************************************************************************* */
/* ***************************************** integers ****************************************** */
/** Returns a scalar integer                                                                     */
int int_jsonscalar (cJSON * root, char * nml_name , char * var_name, char * attribute)
{
  cJSON * var = scalarVal (root, nml_name , var_name, attribute);
  int ivalue = cJSON_GetObjectItem(var,attribute)->valueint;
   if (ivalue == -1){ivalue = errorMESinteger (var_name,nml_name,var,attribute,ivalue);}
 return ivalue;
}
/// Retruns a value from an integer array
int int_jsonarray (cJSON * root, char * nml_name , char * var_name, char * attribute, int array_index, double *ivalue)
{
  cJSON * val = arrayVal (root, nml_name , var_name, attribute);
  int ii = cJSON_GetArrayItem(val,array_index)->valueint; // Find the value of the array at "array_index"
   if (ii == -1){ii = errorMESintarray (var_name,nml_name,val,array_index,ii);}
 return ii;
}
/* ****************************************** REALS ******************************************** */
double real_jsonscalar (cJSON * root, char * nml_name , char * var_name, char * attribute)
{
  cJSON * var = scalarVal (root, nml_name , var_name, attribute);
  double rvalue = cJSON_GetObjectItem(var,attribute)->valuedouble; // Get the value of the variable
 return rvalue;
}

double real_jsonarray (cJSON * root, char * nml_name , char * var_name, char * attribute, int array_index, float *rvalue)
{
  cJSON * val = arrayVal (root, nml_name , var_name, attribute);
  double r = cJSON_GetArrayItem(val,array_index)->valuedouble; // Find the value of the array at "array_index"
 return r;
}
/* ***************************************** STRINGS ******************************************* */
char * string_jsonscalar (cJSON * root, char * nml_name , char * var_name, char * attribute, char * svalue)
{
  cJSON * var = scalarVal (root, nml_name , var_name, attribute);
  svalue = cJSON_GetObjectItem(var,attribute)->valuestring; // Find the string value
 return svalue;
}

char * string_jsonarray (cJSON * root, char * nml_name , char * var_name, char * attribute, int is, char * svalue)
{
  cJSON * val = arrayVal (root, nml_name , var_name,attribute);
  svalue = cJSON_GetArrayItem(val,is)->valuestring; // Find the value of the array at "array_index"
 return svalue;
}
/* ********************************************************************************************* */
/* ********************************************************************************************* */
int cjson_array_length (cJSON * root, char * nml_name , char * var_name, char * attribute)
{
  cJSON * val = arrayVal (root, nml_name , var_name, attribute); //Parse for the var
  int ii = cJSON_GetArraySize(val); // Find the size of the array "var_name"
  return ii;
}
/* ****************************************** ERRORS ******************************************* */
int errorMEShandle  () /// If there is a syntax error with the json file
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
         error_mesg_cjson("cjsonVariableObject.c",message,0); 
         fprintf(stdout,"FATAL: JSON - JSON file has a syntax error at or just before: \n %s \n",partE);
         exit (1);
   return 1;
}

int errorMESnml (char * nml_name) /// Function called if the group "nml_name" does not exist in the json file
{
         char message [100];
         strcpy(message,nml_name);
	 strcat(message," does not exist in the JSON file  \n");
         error_mesg_cjson("cjsonVariableObject.c",message,0);
         fprintf(stdout,"FATAL: JSON - %s does not exist in the JSON file  \n",nml_name);
         exit (1);
   return 1;
}

int errorMESvar (char * var_name,char * nml_name) ///Function called if "var_name" does not exist in "nml_name" in the json file
{
         char message [100];
         strcpy(message,var_name);
	 strcat(message," does not exist in the object ");
         strcat(message,nml_name);
	 strcat(message," in the JSON file  \n");
         error_mesg_cjson("cjsonVariableObject.c",message,0);

         fprintf(stdout,"FATAL: JSON - %s does not exist in the group %s the JSON file  \n",var_name,nml_name);
         exit (1);
   return 1;
}

int errorMESval (char * var_name,char * nml_name, char * attribute) ///Function called if "var_name" does not exist in "nml_name" in the json file
{
         char message [150];
         strcpy(message,var_name);
         strcat(message," in the JSON object (namelist) ");
         strcat(message,nml_name);
         strcat(message," does not have the attribute ");
         strcat(message,attribute);
         strcat(message," the JSON file  \n");
         error_mesg_cjson("cjsonVariableObject.c",message,0);

         fprintf(stdout,"FATAL: JSON - %s in the group %s does not have the attribute %s in the JSON file  \n",var_name,nml_name,attribute);
         exit (1);
   return 1;
}
int errorMESinteger (char * var_name,char * nml_name,cJSON * var, char * attribute, int ivalue)
{
   double dd = cJSON_GetObjectItem(var,attribute)->valuedouble;
   if (dd > 5 || dd < -5){ 
                 char message [150];
	         strcpy(message,"The integer ");
                 strcat(message,var_name);
                 strcat(message," in ");
                 strcat(message,nml_name);
		 strcat(message," is outside of the integer range\n");
                 error_mesg_cjson("cjsonVariableObject.c",message,0);
                          printf("The integer in %s in %s is outside of the integer range\n",var_name,nml_name);
                          exit (1);
                         }
   return ivalue;
}


int errorMESintarray (char * var_name,char * nml_name,cJSON * val, int array_index, int ii)
{
  double dd  = cJSON_GetArrayItem(val,array_index)->valuedouble; // Find the value of the array at "array_index"
   if (dd > 5 || dd < -5){         
                 char message [150];
                 strcpy(message,"The integer ");
                 strcat(message,var_name);
                 strcat(message," in ");
                 strcat(message,nml_name);
                 strcat(message," is outside of the integer range\n");
                 error_mesg_cjson("cjsonVariableObject.c",message,0);
                      printf("The integer in %s in %s is outside of the integer range\n",var_name,nml_name);
                      exit (1);
                     }
   return ii;
}

