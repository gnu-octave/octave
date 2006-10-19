/*

Copyright (C) 2006 Paul Kienzle

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

/*

This code was originally distributed as part of Octave Forge under
the following terms:

Author: Paul Kienzle
I grant this code to the public domain.
2001-03-22

THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.

*/

/* mex.h is for use in C-programs only; do NOT include it in mex.cc */

#if ! defined (MEXPROTO_H)
#define MEXPROTO_H

#if defined (__cplusplus)
#include <cstdlib>
extern "C" {
#else
#include <stdlib.h>
#endif

#define MXARRAY_TYPEDEFS_ONLY
#include "mxarray.h"
#undef MXARRAY_TYPEDEFS_ONLY

// Interface to the interpreter.
extern const char *mexFunctionName (void);

extern int mexCallMATLAB (int nargout, mxArray *argout[], int nargin,
			  mxArray *argin[], const char *fname);

extern void mexSetTrapFlag (int flag);
extern int mexEvalString (const char *s);
extern void mexErrMsgTxt (const char *s);
extern void mexErrMsgIdAndTxt (const char *id, const char *s);
extern void mexWarnMsgTxt (const char *s);
extern void mexWarnMsgIdAndTxt (const char *id, const char *s);
extern void mexPrintf (const char *fmt, ...);
  
extern mxArray *mexGetVariable (const char *space, const char *name);
extern const mxArray *mexGetVariablePtr (const char *space, const char *name);

extern int mexPutVariable (const char *space, const char *name, mxArray *ptr);

extern void mexMakeArrayPersistent (mxArray *ptr);
extern void mexMakeMemoryPersistent (void *ptr);

extern int mexAtExit (void (*f) (void));
extern const mxArray *mexGet (double handle, const char *property);
extern int mexIsGlobal (const mxArray *ptr);
extern int mexIsLocked (void);
extern void mexLock (void);
extern int mexSet (double handle, const char *property, mxArray *val);
extern void mexUnlock (void);

// Floating point predicates.
extern int mxIsFinite (double v);
extern int mxIsInf (double v);
extern int mxIsNaN (double v);

// Floating point values.
extern double mxGetEps (void);
extern double mxGetInf (void);
extern double mxGetNaN (void);
  
// Memory management.
extern void *mxCalloc (size_t n, size_t size);
extern void *mxMalloc (size_t n);
extern void *mxRealloc (void *ptr, size_t size);
extern void mxFree (void *ptr);
  
// Constructors.
extern mxArray *mxCreateCellArray (int ndims, const int *dims);
extern mxArray *mxCreateCellMatrix (int m, int n);
extern mxArray *mxCreateCharArray (int ndims, const int *dims);
extern mxArray *mxCreateCharMatrixFromStrings (int m, const char **str);
extern mxArray *mxCreateDoubleMatrix (int nr, int nc, mxComplexity flag);
extern mxArray *mxCreateDoubleScalar (double val);
extern mxArray *mxCreateLogicalArray (int ndims, const int *dims);
extern mxArray *mxCreateLogicalMatrix (int m, int n);
extern mxArray *mxCreateLogicalScalar (int val);
extern mxArray *mxCreateNumericArray (int ndims, const int *dims, mxClassID class_id, mxComplexity flag);
extern mxArray *mxCreateNumericMatrix (int m, int n, mxClassID class_id, mxComplexity flag);
extern mxArray *mxCreateSparse (int m, int n, int nzmax, mxComplexity flag);
extern mxArray *mxCreateSparseLogicalMatrix (int m, int n, int nzmax);
extern mxArray *mxCreateString (const char *str);
extern mxArray *mxCreateStructArray (int ndims, int *dims, int num_keys, const char **keys);
extern mxArray *mxCreateStructMatrix (int rows, int cols, int num_keys, const char **keys);

// Copy constructor.
extern mxArray *mxDuplicateArray (const mxArray *v);

// Destructor.
extern void mxDestroyArray (mxArray *v);

// Type Predicates.
extern int mxIsCell (const mxArray *ptr);
extern int mxIsChar (const mxArray *ptr);
extern int mxIsClass (const mxArray *ptr, const char *name);
extern int mxIsComplex (const mxArray *ptr);
extern int mxIsDouble (const mxArray *ptr);
extern int mxIsInt16 (const mxArray *ptr);
extern int mxIsInt32 (const mxArray *ptr);
extern int mxIsInt64 (const mxArray *ptr);
extern int mxIsInt8 (const mxArray *ptr);
extern int mxIsLogical (const mxArray *ptr);
extern int mxIsNumeric (const mxArray *ptr);
extern int mxIsSingle (const mxArray *ptr);
extern int mxIsSparse (const mxArray *ptr);
extern int mxIsStruct (const mxArray *ptr);
extern int mxIsUint16 (const mxArray *ptr);
extern int mxIsUint32 (const mxArray *ptr);
extern int mxIsUint64 (const mxArray *ptr);
extern int mxIsUint8 (const mxArray *ptr);

// Odd type+size predicate.
extern int mxIsLogicalScalar (const mxArray *ptr);

// Odd type+size+value predicate.
extern int mxIsLogicalScalarTrue (const mxArray *ptr);

// Size predicate.
extern int mxIsEmpty (const mxArray *ptr);

// Just plain odd thing to ask of a value.
extern int mxIsFromGlobalWS (const mxArray *ptr);

// Dimension extractors.
extern int mxGetM (const mxArray *ptr);
extern int mxGetN (const mxArray *ptr);
extern int *mxGetDimensions (const mxArray *ptr);
extern int mxGetNumberOfDimensions (const mxArray *ptr);
extern int mxGetNumberOfElements (const mxArray *ptr);

// Dimension setters.
extern void mxSetM (mxArray *ptr, int M);
extern void mxSetN (mxArray *ptr, int N);
extern void mxSetDimensions (mxArray *ptr, int *dims, int ndims);
  
// Data extractors.
extern double *mxGetPi (const mxArray *ptr);
extern double *mxGetPr (const mxArray *ptr);
extern double mxGetScalar (const mxArray *ptr);
extern mxChar *mxGetChars (const mxArray *ptr);
extern mxLogical *mxGetLogicals (const mxArray *ptr);
extern void *mxGetData (const mxArray *ptr);
extern void *mxGetImagData (const mxArray *ptr);

// Data setters.
extern void mxSetPr (mxArray *ptr, double *pr);
extern void mxSetPi (mxArray *ptr, double *pi);
extern void mxSetData (mxArray *ptr, void *data);
extern void mxSetImagData (mxArray *ptr, void *pi);

// Classes.
extern mxClassID mxGetClassID (const mxArray *ptr);
extern const char *mxGetClassName (const mxArray *ptr);

extern void mxSetClassName (mxArray *ptr, const char *name);

// Cell support.
extern mxArray *mxGetCell (const mxArray *ptr, int idx);

extern void mxSetCell (mxArray *ptr, int idx, mxArray *val);

// Sparse support.
extern int *mxGetIr (const mxArray *ptr);
extern int *mxGetJc (const mxArray *ptr);
extern int mxGetNzmax (const mxArray *ptr);

extern void mxSetIr (mxArray *ptr, int *ir);
extern void mxSetJc (mxArray *ptr, int *jc);
extern void mxSetNzmax (mxArray *ptr, int nzmax);

// Structure support.
extern int mxAddField (mxArray *ptr, const char *key);

extern void mxRemoveField (mxArray *ptr, int key_num);

extern mxArray *mxGetField (const mxArray *ptr, int index, const char *key);
extern mxArray *mxGetFieldByNumber (const mxArray *ptr, int index, int key_num);

extern void mxSetField (mxArray *ptr, int index, const char *key, mxArray *val);
extern void mxSetFieldByNumber (mxArray *ptr, int index, int key_num, mxArray *val);

extern int mxGetNumberOfFields (const mxArray *ptr);

extern const char *mxGetFieldNameByNumber (const mxArray *ptr, int key_num);
extern int mxGetFieldNumber (const mxArray *ptr, const char *key);

extern int mxGetString (const mxArray *ptr, char *buf, int buflen);
extern char *mxArrayToString (const mxArray *ptr);
  
// Miscellaneous.
#ifdef NDEBUG
#define mxAssert(expr, msg) \
  do \
    { \
      if (! expr) \
        { \
          mexPrintf ("Assertion failed: %s, at line %d of file \"%s\".\n%s\n", \
                     #expr, __LINE__, __FILE__, msg); \
        } \
    } \
  while (0)

#define mxAssertS(expr, msg) \
  do \
    { \
      if (! expr) \
        { \
          mexPrintf ("Assertion failed at line %d of file \"%s\".\n%s\n", \
                     __LINE__, __FILE__, msg); \
          abort (); \
        } \
    } \
  while (0)
#else
#define mxAssert(expr, msg)
#define mxAssertS(expr, msg)
#endif

extern int mxCalcSingleSubscript (const mxArray *ptr, int nsubs, int *subs);

extern int mxGetElementSize (const mxArray *ptr);

#if defined (__cplusplus)
}
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C ***
;;; End: ***
*/
