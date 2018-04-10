#include "cJSON.h"

#define SCALAR_SIZE  -1
#define INTEGER_VAR  1 
#define DOUBLE_VAR  2  
#define FLOAT_VAR  3   
#define CHAR_VAR 4  
#define LOG_VAR 5 

cJSON * cjson_output; /* Output JSON pointer in C */
cJSON * nml_output  ; /* Output pointer to nml_name */
cJSON * var_output  ; /* Output pointer to var_name */

struct jsonvariable {
 char * json    ; //The string holding the json
 char * nml_name; //The name of the JSON array containing var_name
 char * var_name; //The variable name
 int vartype    ; //int=1, double=2, float=3, char=4
 void* var      ; //The actual variable
 int asize      ; //The size of the array (0 is scalar)
};

cJSON * checkparse (char * my_json_string, cJSON * root);
int checknml (cJSON * root, char * my_json_string, char * nml_name, char * var_name);
char * jsonStringInit ();

/*
extern cJSON * checkparse (char * my_json_string, cJSON * root);
extern int checknml (cJSON * root, char * my_json_string, char * nml_name);
*/
