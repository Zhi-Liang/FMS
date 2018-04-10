#include <stdio.h>
#include <stdlib.h>
#include "cJSON.h"
/// Writes the json pointer to a file called output.json in the current directory
int write_json_file (cJSON *root) 
{
  FILE *fp = fopen("./output.json", "w");
   char * out=cJSON_Print(root);
  if (!out){
     return -1; 
           }
  else     {
    fputs(out, fp);
    fclose(fp);
   return 1;
           }
}

char * combine_json (char * json1 , char * json2)
{
 int s1;
 int s2;
/* Get the size of the JSONs */
 s1 = strlen(json1);
 s2 = strlen(json2);
/* Allocate memory for the JSON */ 
 char * fulljson = (char*)malloc((s1*s2) * sizeof(char));
/* Copy json1 into fulljson up to the character before the null 
 * to get rid of the last curly brace
 */ 
 int i = 0 ;
 while (i < s1-1){
  fulljson[i] = json1[i] ;
  i++;
 } 
 fulljson[i] = 44 ; /* add a comma */
 i++;
/* Copy json1 into fulljson excluding the first character to get
 * rid of the first curly brace
 */
 int j = 1;
 while (j < s2){
  fulljson[i] = json2[j] ;
  i++; j++;
 } 
fulljson[i]=0; /* Put the null character on the end */
 printf ("%d %d \n",s1,i);
 printf ("%s %s \n",json1,json2);
 printf ("%s \n" ,fulljson);
return fulljson;
}

