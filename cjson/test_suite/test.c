#ifdef test_json_mom

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cMOM2json.h"
#include "../cJSON.h"

int main( int argc, char* argv[]){
    if( argc != 2 ){ fprintf(stderr, "You need 1 arg\n"); exit(1);}
    // ./MOM_input
    cJSON* test = MOM2json_v2( argv[1] );
    MOM_override(test);
  //  char* json = cJSON_Print(test);
    cJSON_Delete(test);
 //   printf("%s\n", json);
//    free(json);
    return 0;
}

#endif
