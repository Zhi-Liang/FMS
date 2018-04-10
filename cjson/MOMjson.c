#include <stdio.h>
#include <stdlib.h>
#include "cJSON.h"
char * mom_json_file (cJSON *root, char *out) 
{

//  FILE *fp = fopen("MOM_input.json", "ab");
   out=cJSON_Print(root);
//    fputs(out, fp);
//    fclose(fp);
//    cJSON_Delete(root);
//        free(out); 
   return out;
}
