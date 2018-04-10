/* *********************************************************************************** */
// \Author Tom Robinson 
/* This set of C functions is to be used to get values from a json in C.  Its for C and 
 * of C
*/
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <float.h>
#include <limits.h>
#include <ctype.h>
#include "error_cjson.h"
#include "cJSON.h"
#include "namelist_array.h"
#include "json.h"

void init_json_c ()
{

        cjson_output = cJSON_CreateObject ();

}


void array_output(void * var,int asize,int vartype)
{
 int a;
 cJSON * array_out = cJSON_CreateArray();
 for (a=0;a<asize;a++){
                if (vartype == INTEGER_VAR){
                   cJSON_AddItemToArray(array_out,cJSON_CreateNumber( ((int *)var)[a] ));
                   }
                else if (vartype == DOUBLE_VAR){
                   cJSON_AddItemToArray(array_out,cJSON_CreateNumber( ((double *)var)[a] ));
                   }
                else if (vartype == FLOAT_VAR){
                   cJSON_AddItemToArray(array_out,cJSON_CreateNumber( ((float *)var)[a] ));
                   }
                else if (vartype == LOG_VAR){
                   if ( ((int *)var)[a] == 0) {cJSON_AddItemToArray(array_out,cJSON_CreateFalse() );}
                   else                       {cJSON_AddItemToArray(array_out,cJSON_CreateTrue() );}
                   }
               else if (vartype == CHAR_VAR){
                  char * em = "String arrays currently not supported.  Pass string as a scalar \n";
                  error_mesg_cjson("cargs_json",em,FATAL);
                   }
 }
 cJSON_AddItemToObject(var_output, "value",array_out );
}


void cargs_json(struct jsonvariable var[], size_t len)
{
  int i; int a;  /* Integers for looping */
  int n; /* The array size to loop through */
  int varexists; /* A flag to see if a variable exists */
  int array_len; /* The size of the variable array */
  cJSON * cjson; /* The parsed JSON */

 nml_output = cJSON_CreateObject(); /*Crate the nml_name in the output JSON*/
 cJSON_AddItemToObject(cjson_output,var[0].nml_name,nml_output);

  for (i=0; i < len; i++) /* Loop through the struct containing variable information */
        {
        if (i==0){
/* Parse the JSON */
         cjson = cJSON_Parse(var[i].json);
          if (!cjson) {
            int ierr = errorParse();
                     }
                 }
/* Checks to see if nml_name and var_name are present in the JSON.
 * Returns  1 if nml_name and    var_name are present. 
 * Returns  0 if nml_name is but var_name is not present
 * Returns -1 if nml_name is not              present.
 */
        varexists = checknml (cjson, var[i].json, var[i].nml_name, var[i].var_name);
//        if (varexists == -1) {return;} /* Exit the routine if the nml_name does not exist */
//        if (varexists == 0 ) {continue;} /* If var_name doesn't exist, go onto the next var_name */

/* If the variable is not -1 0 or 1, then there is an error with this routine */
//        if (varexists != 1 ) {printf("ERROR\n");exit(1);}

/* Put the variable in the output JSON */
                var_output = cJSON_CreateObject(); 
                cJSON_AddItemToObject(nml_output,var[i].var_name,var_output); 
       if (var[i].asize == SCALAR_SIZE) /* Scalar Handling */
                {             
/* Cast the void*var pointer to the actual type and get the value */
                if (var[i].vartype == INTEGER_VAR){
                   if (varexists == 1) {
                     *((int*)var[i].var) = integer_val(cjson,var[i].json,var[i].nml_name,var[i].var_name);
                     }
                   cJSON_AddItemToObject(var_output, "value", cJSON_CreateNumber( *((int*)var[i].var) ) );
                   }
                else if (var[i].vartype == DOUBLE_VAR){
                   if (varexists == 1) {                     
                     *((double*)var[i].var) = real_val(cjson,var[i].json,var[i].nml_name,var[i].var_name);    
                     }
                   cJSON_AddItemToObject(var_output, "value", cJSON_CreateNumber( *((double*)var[i].var) ) );
                   }
                else if (var[i].vartype == FLOAT_VAR){
                   if (varexists == 1) {
                     *((float*)var[i].var) = real_val(cjson,var[i].json,var[i].nml_name,var[i].var_name);   
                     }
                   cJSON_AddItemToObject(var_output, "value", cJSON_CreateNumber( *((float*)var[i].var) ) );
                   }
                if (var[i].vartype == LOG_VAR){
                   if (varexists == 1) {
                     *((int*)var[i].var) = integer_val(cjson,var[i].json,var[i].nml_name,var[i].var_name);
                     }
                   if (*((int*)var[i].var) == 0) {cJSON_AddItemToObject(var_output, "value", cJSON_CreateFalse() );}
                   else                          {cJSON_AddItemToObject(var_output, "value", cJSON_CreateTrue() );}
                   }
                else if (var[i].vartype == CHAR_VAR){
                   if (varexists == 1) {
                /* Save the string as a pointer */
                    char * svalue = string_val(cjson,var[i].json,var[i].nml_name,var[i].var_name); 
                /* Copy the string pointer into the variable and then free the pointer */
                    strcpy ( ((char *)var[i].var),svalue);
                    free (svalue);    
                    }
                   cJSON_AddItemToObject(var_output, "value", cJSON_CreateString( ((char *)var[i].var) ) );
                   }
                }
        else /* Array handling requires getting the size of the variable and the size of the array 
              * in the JSON.  The array is looped through using which ever value is smaller.
              */
                {
           if (varexists == 1) {
                     
/* Get the size of the JSON array */
                array_len = array_length (var[i].json,var[i].nml_name,var[i].var_name);
                if (array_len < var[i].asize)
                     {
                     n=array_len;
                     }
                else {
                     n=var[i].asize;
                     }
                for (a=0 ; a < n ; a++) {
                if (var[i].vartype == INTEGER_VAR || var[i].vartype == LOG_VAR){
                   ((int *)var[i].var)[a] = integer_array(cjson,var[i].json,var[i].nml_name,var[i].var_name,a);
                   }
                else if (var[i].vartype == DOUBLE_VAR){
                   ((double *)var[i].var)[a] = real_array(cjson,var[i].json,var[i].nml_name,var[i].var_name,a);    
                   }
                else if (var[i].vartype == FLOAT_VAR){
                   ((float *)var[i].var)[a] = real_array(cjson,var[i].json,var[i].nml_name,var[i].var_name,a);   
                   }
                else if (var[i].vartype == CHAR_VAR){
                  char * em = "String arrays currently not supported.  Pass string as a scalar \n";
                  error_mesg_cjson("cargs_json",em,FATAL);
/*                  
                   char * svalue = string_val(cjson,var[i].json,var[i].nml_name,var[i].var_name);
                   ((char *)var[i].var)[a] = string_array(cjson,var[i].json,var[i].nml_name,var[i].var_name,a); 
*/
                   } 
                                        } /* ARRAY FOR LOOP */
           } /* if varexists */
                array_output(var[i].var,var[i].asize,var[i].vartype);
                } /* THE ELSE TO DETERMINE ITS AN ARRAY */
        }
 cJSON_Delete (cjson); /* Free the parsed JSON */ 
 printf("%s \n ",cJSON_Print(cjson_output));
}


char * jsonStringInit ()
{
 FILE* fp ;
 char buff ; 
 int count  ;
 count = 0;
 fp = fopen("input.json","r"); //Open the JSON file
 while (fgetc(fp)!=EOF)
 { 
        count = count + 1;
        buff = fgetc(fp);
//        printf ("%c %d \n",buff,count);
 }
 rewind (fp);
 char* jsonout = (char*)malloc((count+2)*sizeof(char)*2);
 fread(jsonout,sizeof(char),(count+1)*2,fp);
 int iout = fclose(fp);
 fp = NULL;
 return jsonout;
}

cJSON * checkparse (char * my_json_string, cJSON * root)
{
// If the json is not parsed as a C pointer, parse it
 if (!root) {
          cJSON * root = cJSON_Parse(my_json_string); // Parse json file
            }
// Check for syntax error in the JSON
 if (!root) {
          int i = errorParse();
            }
 return root;
}

/* Checks to see if nml_name and var_name are present in the JSON.
 *
 * Returns  1 if nml_name and    var_name are present. 
 * Returns  0 if nml_name is but var_name is not present
 * Returns -1 if nml_name is not              present.
 */
int checknml (cJSON * root, char * my_json_string, char * nml_name, char * var_name)
{
 int varexist = 0 ; int i; int iw;


 cJSON * nml = cJSON_GetObjectItem(root,nml_name); // Find the namelist
     if (!nml)
     {
         // iw = WARNnml(nml_name); //Sends a note 
     }
     else
     {
          for (i=0; i<cJSON_GetArraySize(nml); i++ )  
          {
               cJSON * member = cJSON_GetArrayItem(nml,i); // Find the variable 
               if (!member){
                iw=errorMember (nml_name);
                           }
               cJSON * var = cJSON_GetObjectItem(member,var_name);
               if (var){varexist=1;}
          }
          if (varexist == 0) {//printf("HHEEERRRRREEE\n");//iw =errorVarNotPresent(nml_name,var_name);
                             }
          return varexist;
     }
       
}


int errorParse  () /// If there is a syntax error with the json file
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
         error_mesg_cjson("checkparse",message,FATAL);
         fprintf(stdout,"FATAL: JSON - JSON file has a syntax error at or just before: \n %s \n",partE);
         exit (1);
   return 1;
}

int errorMember (char * nml_name)
{
 char * mesg = "There is a formatting problem with the JSON array ";
 char errormes [100];
 strcpy(errormes, mesg);
 strcat(errormes,nml_name);
 error_mesg_cjson("checknml",errormes,FATAL); // Prints a fatal
 exit (1);
 return 1;
}

int WARNnml (char * nml_name)
{
 char errormes [100];

 char * mesg1 = "The namelist ";
 char * mesg2 = " is not present in the JSON.";
 strcpy(errormes,mesg1);
 strcat(errormes,nml_name);
 strcat(errormes,mesg2);
 error_mesg_cjson("checknml",errormes,NOTE); //A note
 free (mesg1); free (mesg2);
 return 1;
}

int errorVarNotPresent(char * nml_name,char * var_name)
{
 char * mesg1 = "The variable ";
 char * mesg2 = " in ";
 char * mesg3 = " is not found in the json.  Default should be used.";

 char errormes [100];
 strcpy(errormes,mesg1);
 strcat(errormes,var_name);
 strcat(errormes,mesg2);
 strcat(errormes,nml_name);
 strcat(errormes,mesg3);
 error_mesg_cjson("json.c::checknml",errormes,NOTE); //Prints a note
 free (mesg1); free (mesg2); free (mesg3);
return 1;
}

