/* This code is in the public domain */

#ifndef _RANDGAMMA_H

#include "oct-types.h"

#ifdef  __cplusplus
extern "C" {
#endif

extern double oct_randg (double a);
extern void oct_fill_randg (double a, octave_idx_type n, double *p);

#ifdef  __cplusplus
}
#endif
#endif

/*
;;; Local Variables: ***
;;; mode: C ***
;;; End: ***
*/

