/** @file */
/*Header file which defines a "verbosity" enumerator.*/

#ifndef SET_VERBOSITY_T_ENUMDEF_H_
#define SET_VERBOSITY_T_ENUMDEF_H_

/*---------------------------------------------------------------------------*/
/**>
    This enumerator sets the level of verbosity used for stdout prints.
*/
enum e_verbosity
{
    VERBOSE_OFF=0,
    VERBOSE_ON=1,
    VERBOSE_LOUD=3
};
typedef enum e_verbosity verbosity_t;

/*---------------------------------------------------------------------------*/

#endif
