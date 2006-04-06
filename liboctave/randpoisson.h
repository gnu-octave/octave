/* This code is in the public domain */

#ifndef _RANDPOISSON_H

#include "oct-types.h"

#ifdef  __cplusplus
extern "C" {
#endif

extern double oct_randp (double L);
extern void oct_fill_randp (double L, octave_idx_type n, double *p);

#ifdef  __cplusplus
}
#endif
#endif

/*
;;; Local Variables: ***
;;; mode: C ***
;;; End: ***
*/

