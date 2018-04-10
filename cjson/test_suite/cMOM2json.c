#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include "../cJSON.h"

int mom_num = 0; // not needed
static char filename[20];

char* trim_line( char* );
void trim( char* );
void MOM_override( cJSON* );
void format();
void add_comment();
int debug = 1;

/* Construction */
/* Easy printf */
void ptf( char* message, char* func ){
    if( debug == 1 ){
        printf("%s : %s\n", func, message);
    }
}

cJSON* createArray( char* val ){
   cJSON* arr = cJSON_CreateArray();
   char number[5] = {'\0'};
   int i = 0;
   int in_number = 0;
   int ind = 0;
   while( val[i] != '\0' ){
       if( i > 4 ){ fprintf(stderr, "Failed to Create Array correctly"); exit(2); }
       if( in_number == 0 && val[i] == ' ' ){

       } else if( in_number == 1 && val[i] == ' ' ){
           number[ind] = '\0';
           cJSON_AddItemToArray( arr, cJSON_CreateNumber(atof(number)));
           ind = 0;
           number[ind] = '\0';
           in_number = 0;
       } else if( (val[i] >= '0' && val[i] <= '9') || val[i] == '.' ){
           if( in_number == 0 ) in_number = 1;
           number[ind] = val[i];
           ind++;
       } else if( val[i] == ',' ){

       }
       i++;
   }

   if( in_number == 1){
       cJSON_AddItemToArray( arr, cJSON_CreateNumber(atof(number)));
   }
   return arr;
}

void cJSON_construct_val( cJSON **fmt, char *val ){
    // you have the phrase determine action
    if( fmt == NULL ){ fprintf(stderr, "Failed format is NULL"); exit(2);}
    if( val == NULL ){ fprintf(stderr, "Failed val is NULL : Construct"); exit(2);}
    int i = 0;
    int isnumber = 1;
    while( val[i] != '\0' ){
        if( val[i] == 'e') val[i] = 'E';
        if( !( (val[i] >= '0' && val[i] <= '9') || val[i] == '.'  || val[i] == 'E' || val[i] == 'e' || val[i] == '+' || val[i] == '-' ) ){
            isnumber = 0;
            break;
        }
        i++;
    }

    if( strstr( val, "True" ) != NULL || strstr( val, "true") != NULL ){
        cJSON_AddTrueToObject(*fmt, "value");
    } else if( strstr( val, "False" ) != NULL || strstr(val, "false") != NULL ){
        cJSON_AddFalseToObject(*fmt, "value");
    } else if(strstr(val, "\"" ) != NULL){
        i = 0;
        while( val[i] != '\0' ){
            if(val[i] == '\"') val[i] = ' ';
            i++;
        }
        cJSON_AddStringToObject(*fmt, "value", val);
    } else if ( isnumber == 1 ){
        double num = atof(val);
        cJSON_AddNumberToObject(*fmt, "value", num);
    } else if ( strchr( val, ',') != NULL  ){
        cJSON_AddItemToObject(*fmt, "value", createArray( val ));
    } else {
        fprintf( stderr, "Not a valid entry |%s|\n", val );
        exit(2);
    }
}

void cJSON_construct_def( cJSON **fmt, char *phrase ){
    // you have the phrase determine action
    //
    if( strstr( phrase, "True" ) != NULL ){
        cJSON_AddTrueToObject(*fmt, "default");
    } else if( strstr( phrase, "False" ) != NULL ){
        cJSON_AddFalseToObject(*fmt, "default");
    } else if(strstr(phrase, "\"" ) != NULL){
        int i = 0;
        while( phrase[i] != '\0' ){
            if(phrase[i] == '\"') phrase[i] = ' ';
            i++;
        }
        trim(phrase);
        cJSON_AddStringToObject(*fmt, "default", phrase);
    } else {cJSON_AddNumberToObject(*fmt, "default", atof(phrase));}
}

void cJSON_const_elem( cJSON **fmt, char *phrase, int type, int num_override ){
    // you have the phrase determine action
    //
    printf("Switching |%s|\n", phrase);
    char name[20];
    strcpy(name, "MOM_override");
    size_t i; size_t length = strlen(name);
    snprintf(name+length, 2, "%.2d", num_override);

    switch(type){
    case 1:
        cJSON_AddStringToObject(*fmt, "override", name);
        break;
    case 2:
        cJSON_AddTrueToObject(*fmt, "prev");
        cJSON_AddStringToObject(*fmt, "override", name);
        break;
    case 8:
        cJSON_AddNumberToObject(*fmt, "prev", atof(phrase));
        cJSON_AddStringToObject(*fmt, "override", name);
        break;
    case 16:
        cJSON_AddStringToObject(*fmt, "prev", phrase);
        cJSON_AddStringToObject(*fmt, "override", name);
        break;
    }
}

void cJSON_replace( cJSON **fmt, char *phrase, int type, int num_override ){
    // you have the phrase determine action
    char name[20];
    char scrap[20];
    strcpy(name, "MOM_override");
    strcpy(scrap, phrase);
    size_t length = strlen(name);
    if( mom_num > 9 )
        snprintf(name+length, 2, "%d", mom_num);
    else
        snprintf(name+length, 3, "%d", mom_num);
    trim(scrap);

    switch(type){ // copy
    case 1:
        cJSON_AddTrueToObject(*fmt, "value");
        cJSON_AddStringToObject(*fmt, "override", name);
        break;
    case 2:
        cJSON_AddTrueToObject(*fmt, "value");
        cJSON_AddStringToObject(*fmt, "override", name);
        break;
    case 8:
        cJSON_AddNumberToObject(*fmt, "value", atof(scrap));
        cJSON_AddStringToObject(*fmt, "override", name);
        break;
    case 16:
        cJSON_AddStringToObject(*fmt, "value", scrap);
        cJSON_AddStringToObject(*fmt, "override", name);
        break;
    }
}


/* Formatting and String Man */
/* Takes a char* creates a copy and frees the previous char* */

void format( char* line){
    int i     = 0;
    int start = 0;
    int max = strlen(line);
    while(line[i] != '\0' && (line[i] == ' ' || line[i] == '\n')){
        i++; // index
    }
    
    // copy
    while(line[i] != '\0' /*&& (line[i] != ' ' && line[i] != '\n')*/){
        line[start] = line[i];
        start++; i++;
    }
    line[start] = '\0';
    //printf("Show format : |%s|\n", line);

    // remove empty whitespace
    start--;
    while( start >= 0 && ( line[start] == ' ' || line[start] == '\n' )){
        start--;
    }
    line[start+1] = '\0';
}

int is_blank( char* fline ){ /* memory safe */
        int flag = 1;
        int i    = 0;

        for(  i = 0; i < strlen(fline); i++  ){
            if(  !(fline[i] == ' ' || fline[i] == '\n' || fline[i] == '\t')  ){
                flag = 0; break;
            }
        }
        return flag; // 1: it is blank 0: not blank
}

/* >>>>>>          MOM_input Converter         <<<< */

void tokenize( char* tokens[], int size, char* line ){
    int i;
    //printf("Line Tokener: |%s|\n", line);
    tokens[0] = strtok(line, "=" ); // mandatory
    tokens[1] = strtok(NULL, "!" );
    //printf("Tokens : %s   %s\n", tokens[0], tokens[1]);
    if( is_blank(tokens[0]) || tokens[0] == NULL )
      {  printf("Tokenizer failed**\n"); exit(0); }
    if( is_blank(tokens[1]) || tokens[1] == NULL )
      {  printf("Tokenizer failed**\n"); exit(0); }

    for( i = 2; i < size; i++){
        tokens[i] = strtok(NULL, "\n]=");
    }
    if( tokens[2] != NULL ){
        char* loc;
        if( (loc = strchr( tokens[2], '[' )) != NULL )
            *loc = ' ';
    }
    for( i = 0; i < 5; i++ ){
        if( tokens[i] == NULL ) break;
        format(tokens[i]);
        //printf("|%s|  ", tokens[i]);
    }
    //printf("\n");
}

cJSON* MOM2json_v2( char* file_loc ){ /* "./MOM_input" */
    if( access(file_loc, F_OK) != 0 ){
        return NULL;
    }

    FILE* file = fopen( file_loc, "rb");
    char fline[150];

    char* sep;
    char* excl;
    char* ret;
    char* phrase      = NULL;
    // a value that needs to be freed or constructed
    cJSON * root      = cJSON_CreateObject();
    cJSON * fmt       = NULL;
    cJSON * obj       = NULL;
    //cJSON_AddItemToObject(root, "module" , obj = cJSON_CreateObject());
    // create object on first action

    int ismodule      = 0;
    int type          = 0;

    while( fgets(fline, 150, file) != NULL ){

        /* This removes modules name and commented variables*/
        /* For parsing out the str from the file */

        if(is_blank(fline)){ continue; }
        //format(fline);

        /* ---------------- Commented section for cJSON ------- */
        type = 0;
        // 0 = comment
        // 1 = assignment
        // 2 = module (object)
        ret = strchr(fline, '=');
        excl = strchr(fline, '!');

        if( ret == NULL && excl == NULL ){
            continue; // ignore because it is not the correct format
            // pass
        } else if( excl == NULL ){ //   '=' only

            fprintf(stderr, "Invalid line\n");
            fprintf(stderr, "Fline : |%s|\n", fline);
            exit(1);

        } else if( ret == NULL ){  //   '!' only

            type = 0; // comment
           
            // pass
        } else if( ret != NULL && excl != NULL ){  // module or assignment

            //  contains '=' and '!'
            // module or comment
            if( strstr(fline, "===") != NULL ){ // module type
                type = 2;
            } else{ // assignment type

                int i = 0, j = 0;
                char* location[2] = { NULL, NULL };

                while( fline[i] != '\0' ){
                    if( j > 2 ) { fprintf(stderr, "Too many parenthesis\n"); exit(3); }
                    if(fline[i] == '!')
                        location[j++] = &fline[i++];
                    else
                        i++;
                }

                int assignment = 0;
                for( i = 0; i < 2; i++ ){
                    if(location[i] > ret){
                        assignment = 1;
                    }
                }

                if( assignment == 1 ){
                    type = 1;
                } else {
                    fprintf(stderr, "Invalid format\n");
                    exit(2);
                }
            }
        } else {
//            printf("Failed to get a valid string\n");
            exit(3);
        }
        /* Contains all parts */

       /* Parsing section skips */
       /* ------------------ Seperating tokens ----------------*/
       if( type == 0 ){ // comment
           char* comment = strchr( fline , '!' );
           format(++comment);
           if( obj == NULL ){ add_comment(root, comment); }
           else if( obj->child == NULL ){
               add_comment( obj, comment );
           } else {
               cJSON* pointer = obj->child;
               int insert2obj = 1; // yes
               while(pointer != NULL){
                   if( fmt == pointer ){
                       //printf("Pointer is found in list obj\n");
                       insert2obj = 0; // no
                       break;
                   }
                   pointer = pointer->next;
               }
               if( insert2obj == 1 ){
                   add_comment( obj, comment );
               } else {
                   add_comment( fmt, comment );
               }
           }
       } else if(type == 1 ){
           // assignment
           char* tokens[5] = { NULL, NULL,  NULL,  NULL, NULL };
                            // var   value  units  def   default_value
           
           tokenize(tokens, 5, fline);
           if( obj == NULL ){
               cJSON_AddItemToObject(root, "module" , obj = cJSON_CreateObject());
               cJSON_AddItemToObject(obj, tokens[0], fmt = cJSON_CreateObject());
           } else {
               cJSON_AddItemToObject(obj, tokens[0], fmt = cJSON_CreateObject());
           }
           //printf("Variable name : |%s|  | Assignment : |%s|\n", tokens[0], tokens[1]);
           cJSON_construct_val(&fmt, tokens[1]);
           if( tokens[2] != NULL && strcmp(tokens[2], "default") == 0 ){
               // value in 4
               cJSON_construct_def( &fmt, tokens[3] ); // get value assignment
           } else if( tokens[2] != NULL && tokens[4] != NULL ){
               // units and value
               cJSON_AddStringToObject(fmt, "units", tokens[2]);
               cJSON_construct_def( &fmt, tokens[4] ); // get value assignment
           } else if( tokens[2] != NULL && tokens[3] == NULL ){
               // units
               cJSON_AddStringToObject(fmt, "units", tokens[2]);
           } else {
               // no default or units
               int i, count = 0;;
               for( i = 0; i < 5; i++ ){
                   if( tokens[i] == NULL ) break;
                   count++;
               }
               if( count != 2 ){ printf("Units and default are not parsed correctly\n");
                   exit(0);
               }
           }
           // assignment
       } else if( type == 2 ){
           //module
           ret = strstr( fline, "==="); ret += 3;
           ret = strtok(ret, "=");
           format(ret);

           cJSON_AddItemToObject(root, ret, obj = cJSON_CreateObject());
       } else {
           printf("Failed to work capture type\n");
           exit(1);
       }
    }
    fclose(file);
    return root;
}

void add_comment( cJSON* elem, char* comment ){
    cJSON_AddStringToObject(elem, "!", comment);
}

/* >>>>>        MOM_overrides            <<< */

/* Creating Override and New Values */
void add_MOMObj( cJSON* elem, cJSON* cJSON_obj, char* token, int type ){
    if(elem == NULL){fprintf( stderr, "Add_Obj failed"); exit(3);}
    if(token == NULL){fprintf( stderr, "Add_Obj failed"); exit(3);}
    cJSON* values = cJSON_GetObjectItem(elem, "value");
    cJSON* MOM_arr = cJSON_GetObjectItem(elem, "MOM_overrideHistory");


    // create override entry
    cJSON* object     = cJSON_CreateObject();
    /*cJSON* override   = cJSON_CreateString(filename);*/
    cJSON_AddItemToObject( object, filename, cJSON_obj );

    cJSON* MOM_override = NULL;
    if( MOM_arr != NULL ){
        // append to MOM_array
        MOM_override = MOM_arr;
        cJSON_AddItemToArray(MOM_override, object);
    } else {
        MOM_override = cJSON_CreateArray();
        cJSON_AddItemToArray(MOM_override, object);
    }
    if( MOM_arr == NULL ){
        cJSON_AddItemToObject(elem, "MOM_overrideHistory", MOM_override);
    }

    // Create elements : takes from value
    //cJSON_replace(&object, token, type, mom_num);
    /*
    char name[20];
    strcpy(name, "MOM_override");
    size_t i; size_t length = strlen(name);
    */
    /*
    if( mom_num > 9 )
        snprintf(name+length, 2, "%d", mom_num);
    else
        snprintf(name+length, 2, "%d", mom_num);
        */

    //cJSON* obj = cJSON_CreateObject();
    //cJSON_construct_val(&obj,token);
    //cJSON_DeleteItemFromObject(elem, "value");
    //cJSON_construct_val(&elem, token);
}

void add_NewObj( cJSON* elem, cJSON* cJSON_obj, char* token, int type ){
//    if(elem == NULL){fprintf( stderr, "Add_Obj failed"); exit(3);}
//    if(token == NULL){fprintf( stderr, "Add_Obj failed"); exit(3);}
//    cJSON* values = cJSON_GetObjectItem(elem, "value");
//    cJSON* MOM_arr = cJSON_GetObjectItem(elem, "MOM_overrideHistory");
//
//
//    // create override entry
//    cJSON* object     = cJSON_CreateObject();
//    /*cJSON* override   = cJSON_CreateString(filename);*/
//    cJSON_AddItemToObject( object, filename, cJSON_obj );
//
//    cJSON* MOM_override = NULL;
//    if( MOM_arr != NULL ){
//        // append to MOM_array
//        MOM_override = MOM_arr;
//        cJSON_AddItemToArray(MOM_override, object);
//    } else {
//        MOM_override = cJSON_CreateArray();
//        cJSON_AddItemToArray(MOM_override, object);
//    }
//    if( MOM_arr == NULL ){
//        cJSON_AddItemToObject(elem, "MOM_overrideHistory", MOM_override);
//    }
//
//    // Create elements : takes from value
//    //cJSON_replace(&object, token, type, mom_num);
//    /*
//    char name[20];
//    strcpy(name, "MOM_override");
//    size_t i; size_t length = strlen(name);
//    */
//    /*
//    if( mom_num > 9 )
//        snprintf(name+length, 2, "%d", mom_num);
//    else
//        snprintf(name+length, 2, "%d", mom_num);
//        */
//
//    //cJSON* obj = cJSON_CreateObject();
//    //cJSON_construct_val(&obj,token);
//    //cJSON_DeleteItemFromObject(elem, "value");
//    //cJSON_construct_val(&elem, token);
}

void cJSON_MOMValue( cJSON* obj, char* val ){ //get line for parsing here
    if(obj == NULL){ printf("Override error : Object NULL\n"); exit(2);}

    cJSON* value = cJSON_GetObjectItem( obj, "value");
    // not mutating value
    cJSON* cJSON_obj = NULL;

    switch( value->type ){ // find out for true and false
        case 1  : // False
            if( strstr( val, "false" ) != NULL || strstr( val, "False") != NULL ){
                cJSON_obj = cJSON_CreateFalse();
                add_MOMObj(obj, cJSON_obj, val, 1 );
            } else if( strstr( val, "true" ) != NULL  || strstr( val, "True" ) != NULL ){
                cJSON_obj = cJSON_CreateTrue();
                add_MOMObj(obj, cJSON_obj, val, 2 );
            } else {
                fprintf(stderr, "Value did not parse correctly\n");
                exit(2);
            }
            break;
        case 2  : // True
            if( strstr( val, "false" ) != NULL || strstr( val, "False" ) != NULL ){
                cJSON_obj = cJSON_CreateFalse();
                add_MOMObj(obj, cJSON_obj, val, 1 );
            } else if( strstr( val, "true" ) != NULL  || strstr( val, "True" ) != NULL ){
                cJSON_obj = cJSON_CreateTrue();
                add_MOMObj(obj, cJSON_obj, val, 2 );
            } else {
                fprintf(stderr, "Value did not parse correctly\n");
                exit(2);
            }
            break;
        case 4  : // NULL
            fprintf(stderr, "cJSON_value error : Trying to create NULL\n");
            exit(2);
            break;
        case 8  : // int or double
            cJSON_obj = cJSON_CreateNumber(atof(val));
            add_MOMObj(obj, cJSON_obj, val, value->type );
            break;
        case 16 : // string
            cJSON_obj = cJSON_CreateString(val);
            add_MOMObj(obj, cJSON_obj, val, value->type );
            break;
        case 32 : // array
            fprintf(stderr, "cJSON_value error : Trying to create object\n");
            exit(2);
            break;
        default:
            fprintf(stderr, "cJSON_value error : Does not fit cases\n");
            exit(2);

    }
    return;
}

void cJSON_NewValue( cJSON* obj, char* tokens[], int size ){ //get line for parsing here
    cJSON* fmt = cJSON_CreateObject();
    cJSON_construct_val(&fmt, tokens[1]);
    cJSON_AddItemToObject(obj, tokens[0], fmt );
}

/* 2 Override Options */
cJSON* cJSON_override(  cJSON* root, char* tokens[], int size ){
    cJSON* trav = root->child; // gets you to modules
    cJSON* ret = NULL;

    while(trav != NULL){
        // go in and search variable name
        if( ( ret = cJSON_GetObjectItem(trav, tokens[0]) ) != NULL ){
            cJSON_MOMValue( ret, tokens[1] ); //send value
            break;
        }
        trav = trav->next;
    }
    return ret;
}

void cJSON_addvariable(  cJSON* root, char* tokens[], int size ){
    if( root == NULL || root->child ==NULL){ fprintf(stderr, "No root to add a variable\n"); exit(2); }
    cJSON* module_obj = root->child; // module
    cJSON_NewValue( module_obj, tokens, size );
}

void trim (char* line){ // for token
    int i     = 0;
    int start = 0;
    //printf("Trimming %s\n", line);
    while(line[i] != '\0' && (line[i] == ' ' || line[i] == '\n')){
        i++; // index
    }
    
    while(line[i] != '\0' && (line[i] != ' ' && line[i] != '\n')){
        line[start] = line[i];
        start++; i++;
    }
    line[start] = '\0';
    //printf("Trimming %s\n", line);
}

void MOM_override( cJSON* json ){
    if( json == NULL ){
        //char* output = malloc(sizeof(char) * 1);
        //strcpy(output, "");
        return;
    }
    strcpy(filename, "MOM_override");
    size_t length = strlen(filename);
    int i;
    for( i = 0; i < 100; i++){

        if( i != 0 ){
            if( i < 9)
                snprintf(filename+length, 2, "%d", ++mom_num);
            else
                snprintf(filename+length, 3, "%d", ++mom_num);
        }
        if( access(filename, F_OK) != 0 )
            continue;
            
        FILE* file = fopen( filename, "rb");
        char fline   [150];

        char* sep;
        char* ret;
        // a value that needs to be freed or constructed
        cJSON * fmt       = NULL;
        cJSON * obj       = NULL;

        while( fgets(fline, 150, file) != NULL ){ // parsing in

            if(is_blank(fline)){ continue; }
            /* ---------------- Commented section for cJSON ------- */
            if( strchr(fline,'!') != NULL ){ //skip comments
                continue;
            }

            //format( fline );
            char* error_1 = strstr(fline, "#");
            char* error_2 = strstr(fline, "override");
            if( error_1 == NULL && error_2 != NULL ){
                printf("Invalid format %s\n", fline);
                exit(1);
            } else if( error_1 != NULL && error_2 == NULL ){
                printf("Invalid format %s\n", fline);
                exit(2);
            }

            int is_override = 0;
            char* tokens[2]   = {NULL, NULL};
            //printf("Fline : |%s|\n", fline);
            char* word_ptr = strstr(fline, "#override");
            if( word_ptr != NULL ){
                is_override = 1;
                word_ptr += 9;
            } else {
                word_ptr = fline;
            }

            tokens[0] = strtok(word_ptr, "=");
            tokens[1] = strtok(NULL, "\n");
            format(tokens[0]); format(tokens[1]);
            printf("Tokens : 0 - |%s| 1 - |%s|\n", tokens[0], tokens[1]);


            // TODO add adding and override functions
            if( is_override == 0 ){
                cJSON_addvariable(json, tokens, 2); // run add
            } else if( is_override == 1 ){
               cJSON* check =cJSON_override(json, tokens, 2);// run override
               if( check == NULL ){
                   fprintf( stderr, "Failed to override : value not found\n");
                   exit(1);
               }
            }
        }
        fclose(file);
    }
    printf("%s\n", cJSON_Print(json));
    return;
}
