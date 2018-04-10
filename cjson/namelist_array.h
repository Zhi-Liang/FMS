extern int integer_val (cJSON * boot, char * my_json_string, char * nml_name , char * var_name);
extern double real_val (cJSON * boot, char * my_json_string, char * nml_name , char * var_name);
extern char * string_val (cJSON * boot, char * my_json_string, char * nml_name , char * var_name);

extern int integer_array (cJSON * boot, char * my_json_string, char * nml_name , char * var_name, int array_index);
extern double real_array (cJSON * boot, char * my_json_string, char * nml_name , char * var_name, int array_index);
extern char * string_array (cJSON * boot, char * my_json_string, char * nml_name , char * var_name, int array_index);


int array_length (char * my_json_string, char * nml_name , char * var_name);
