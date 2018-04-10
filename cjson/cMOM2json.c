#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include "cJSON.h"

char* trim_line( char* );

void cJSON_construct_val( cJSON **fmt, char *text ){
    // you have the phrase determine action
    int i = 0;
    // copy
    int size = (int)strlen(text);
    char* tmp; //= malloc(sizeof(char) * strlen(text) + 1);
    char phrase[size];
    strcpy(phrase, text);

    if( strstr( phrase, "True" ) != NULL ){
        cJSON_AddTrueToObject(*fmt, "value");

    } else if( strstr( phrase, "False" ) != NULL ){
        cJSON_AddFalseToObject(*fmt, "value");

    } else if(strstr(phrase, "\"" ) != NULL){

        while( phrase[i] != '\0' ){
            if(phrase[i] == '\"') phrase[i] = ' ';
            i++;
        }
        tmp = trim_line(phrase);
        cJSON_AddStringToObject(*fmt, "value", tmp);
        free(tmp);
    } else {cJSON_AddNumberToObject(*fmt, "value", atof(phrase));}
}

void cJSON_construct_def( cJSON **fmt, char *phrase ){
    // you have the phrase determine action
    //
    if( strstr( phrase, "True" ) != NULL ){
        cJSON_AddTrueToObject(*fmt, "default");
        return;
    } else if( strstr( phrase, "False" ) != NULL ){
        cJSON_AddFalseToObject(*fmt, "default");
        return;
    } else if(strstr(phrase, "\"" ) != NULL){
        cJSON_AddStringToObject(*fmt, "default", phrase);
    } else {cJSON_AddNumberToObject(*fmt, "default", atof(phrase));}
}

/* Takes a char* creates a copy and frees the previous char* */
char* format_stk( char* str ){
    if (str == NULL) return NULL;
    char* passback = NULL;

    /* prep to what point start */
    int i = 0, k = 0;
    while( str[i] == ' ' || str[i] == '\n' || str[i] == '!' ){
        i++;
    }
    /* string length */
    int ind = i, size_new = 0;
    while( str[ind] != '\0' ){
        if( str[ind] == '\n' ) break;
        size_new++;
        ind++;
    }

    /* copy over */
    passback = (char*) malloc(sizeof(char) * (size_new + 1));
    
    for( ind = i; ind < size_new + i; ind++){
        passback[k++] = str[ind];
    }

    passback[k] = '\0';
    //free(str);
    return passback;
}

char* trim_line( char* str ){
    if (str == NULL) return NULL;
    char* passback = NULL;

    /* prep to what point start */
    int i = 0, k = 0;
    while( str[i] == ' ' || str[i] == '\n' ){
        i++;
    }
    /* string length */
    int ind = i, size_new = 0;
    while( str[ind] != '\0' ){
        if( str[ind] == '\n' ) break;
        size_new++;
        ind++;
    }
    /*remove trailing whitespace */
    ind--;
    while(str[ind] == ' '){
        size_new--;
        ind--;
    }

    /* copy over */
    passback = (char*) malloc(sizeof(char) * (size_new + 1));
    
    for( ind = i; ind < size_new + i; ind++){
        passback[k] = str[ind];
        k++;
    }

    passback[k] = '\0';
    //printf("\nPassback: {%d} |%s|\n\n", k, passback);
    return passback;
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

char* MOM2json( char* file_loc ){ /* "./MOM_input" */
    if( access(file_loc, F_OK) != 0 ){
        char* output = malloc(sizeof(char) * 1);
        strcpy(output, "");
        return output;
    }

    FILE* file = fopen( file_loc, "rb");
    char fline[150];

    char* sep;
    char* ret;
    char* phrase      = NULL;
    // a value that n eeds to be freed or constructed
    cJSON * root      = cJSON_CreateObject();
    cJSON * fmt       = NULL;
    cJSON * obj       = NULL;
    cJSON_AddItemToObject(root, "module" , obj = cJSON_CreateObject());

    int ismodule      = 0;
    //int stop = 0;
    while( fgets(fline, 150, file) != NULL ){
        /* Object creation vv */

        /* This removes modules name and commented variables*/
        /* For parsing out the str from the file */


        if(is_blank(fline)){ continue; }
        /* ---------------- Commented section for cJSON ------- */
        ret = strchr(fline, '=');
        if( ret != NULL ){// does not have equal sign for variable
            ret = strstr(fline, " !");
            if( fline[0] == '!' && fline[2] == '=' ){ }
            else if(ret == NULL){
                printf("Warning no exclamation at the end\n");
                printf("Warning : %s", fline);
                fline[strlen(fline) - 1] = '!';
                fline[strlen(fline)] = '\0';
                printf("Warning : %s <-- tmp, please add later\n", fline);
            }
        }

        ismodule = 0;
        ret = strchr(fline, '!');
        if( fline[0] == '!' ){ 

            sep = strstr(fline, "===");
            if( sep != NULL ){
                ismodule = 1;
            }
                /* allow comments and not modules */
        } else if( ret != NULL )
            { /*intentional  *contains ! as comment  */ } 
        else 
            { continue; }
        /* Parsing section skips */

        char *line = trim_line(fline);
        //printf("line : |%s|\n", line);
        /* ------------ Object or comments ------------ */
        if( ismodule == 1 ){
            ret = strstr( line, "===");
            ret = ret + 3;
            ret = strtok(ret, "=");
            ret = trim_line(ret);

            cJSON_AddItemToObject(root, ret, obj = cJSON_CreateObject());
            free(ret);
        } else if( (isalpha(fline[0]) || isalpha(fline[1])) && strchr(fline, '=') != NULL ){
            // finalize comment
            if( fline[0] == '!'){
                phrase = strtok(line, "=" ); // mutates string
                phrase = trim_line(phrase);
            } else{
                phrase = strtok(line, "=!" ); // mutates string
                phrase = trim_line(phrase);
            }

            //printf("Adding> Name: |%s|\n", phrase);
            //if an object is made use additem to obj
            if( obj == NULL ){
                cJSON_AddItemToObject(root, phrase, fmt = cJSON_CreateObject());
            } else {
                cJSON_AddItemToObject(obj, phrase, fmt = cJSON_CreateObject());
            }
            free(phrase);

            phrase = strtok(NULL, "=!" ); // mutates string
            cJSON_construct_val( &fmt, phrase ); // get value assignment
            //printf("A> Value: %s\n", phrase);

            phrase = strtok(NULL, "=!" ); // mutates string
            while( phrase != NULL ){ // has tokens
                //printf("A-Line: %s\n", line);
                int i;

                if(is_blank(phrase)){
                    break;
                }

                /* Setting up defaults  and units */
                if ( strstr(phrase, "[") != NULL ){
                    char str[50];
                    i = 0;
                    while(phrase[i++] != '['){ // set index
                    }
                    int k = 0;

                    while(phrase[i] != ']'){
                       str[k++] = phrase[i++];
                    }
 
                    str[k] = '\0';
                    cJSON_AddStringToObject(fmt, "units", str);
                } else if(strstr(phrase, "default") != NULL){

                } else {
                    cJSON_construct_def( &fmt, phrase ); // get value assignment
                }
                //printf("A> Value:  %s\n", phrase );
                phrase = strtok(NULL, "=!"); //mutates string
            }
        } else{ // comments only
            /* printf("B> %s", line); */
            ret = strchr(line, '!'); 
            ret = format_stk(ret);
            cJSON_AddStringToObject(fmt, "!", ret);
            free(ret);
            // add str to list
        }
        free(line);

    }
    fclose(file);
    char* rendered = cJSON_Print(root);
    cJSON_Delete(root);
    /*
    //printf("%s\n", cJSON_Print(root));
    //FILE* wfile = fopen( "output", "wb" );
    */
    return rendered;
}

//char* MOM_override( cJSON* json ){
//    /*
//    if( access(file_loc, F_OK) == 0 ){
//    } else {
//        char* output = malloc(sizeof(char) * 1);
//        strcpy(output, "");
//        return output;
//    }
//    */
//    // for( int = 0; i < 100; i++ )
//
//    FILE* file = fopen( "MOM_override", "rb");
//    char fline[150];
//
//    char* sep;
//    char* ret;
//    char* phrase      = NULL;
//    // a value that needs to be freed or constructed
//    cJSON * fmt       = NULL;
//    cJSON * obj       = NULL;
//
//    int ismodule      = 0;
//
//    while( fgets(fline, 150, file) != NULL ){ // parsing in
//
//        if(is_blank(fline)){ continue; }
//        /* ---------------- Commented section for cJSON ------- */
//        if( (ret = strchr(fline,'=')) == NULL ){ //skip comments
//            continue;
//        }
//        phrase = trim_line( fline );
//   /* ------------ Object or comments ------------ */
//        if( (isalpha(phrase[0]) || phrase[0] == '#')) && strchr(fline, '=') != NULL ){
//            // finalize comment
//            if( fline[0] == '!'){
//                phrase = strtok(line, "=" ); // mutates string
//                phrase = trim_line(phrase);
//            }
//
//            //printf("Adding> Name: |%s|\n", phrase);
//            //if an object is made use additem to obj
//            if( obj == NULL ){
//                cJSON_AddItemToObject(root, phrase, fmt = cJSON_CreateObject());
//            } else {
//                cJSON_AddItemToObject(obj, phrase, fmt = cJSON_CreateObject());
//            }
//            free(phrase);
//
//            phrase = strtok(NULL, "=!" ); // mutates string
//            cJSON_construct_val( &fmt, phrase ); // get value assignment
//            //printf("A> Value: %s\n", phrase);
//
//                int i;
//
//                if(is_blank(phrase)){
//                    break;
//                }
//
//                /* Setting up defaults  and units */
//                if ( strstr(phrase, "[") != NULL ){
//                    char str[50];
//                    i = 0;
//                    while(phrase[i++] != '['){ // set index
//                    }
//                    int k = 0;
//
//                    while(phrase[i] != ']'){
//                       str[k++] = phrase[i++];
//                    }
// 
//                    str[k] = '\0';
//                    cJSON_AddStringToObject(fmt, "units", str);
//                } else if(strstr(phrase, "default") != NULL){
//
//                } else {
//                    cJSON_construct_def( &fmt, phrase ); // get value assignment
//                }
//                //printf("A> Value:  %s\n", phrase );
//                phrase = strtok(NULL, "=!"); //mutates string
//            }
//        } else{ // comments only
//            /* printf("B> %s", line); */
//            ret = strchr(line, '!'); 
//            ret = format_stk(ret);
//            cJSON_AddStringToObject(fmt, "!", ret);
//            free(ret);
//            // add str to list
//        }
//        free(line);
//
//    }
//    fclose(file);
//}
