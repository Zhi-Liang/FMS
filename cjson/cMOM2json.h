#include "cJSON.h"
#ifndef MOM2JSON_H_INCLUDED
#define MOM2JSON_H_INCLUDED

void cJSON_construct_val( cJSON* fmt, char* phrase );
void cJSON_construct_def( cJSON* fmt, char* phrase );

int is_blank( char* fline );

char* format_ptr( char* str );
char* trim_line( char* str );
char* MOM2json( char* file_loc );

#endif
