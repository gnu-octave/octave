////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2006-2024 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

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

#if ! defined (octave_mexproto_h)
#define octave_mexproto_h 1

#include "octave-config.h"

#if defined (__cplusplus)
#  include <cstdlib>
using std::size_t;
extern "C" {
#else
#  include <stdlib.h>
#  include <stdbool.h>
#endif

#if ! defined (MX_HAS_INTERLEAVED_COMPLEX)
#  define MX_HAS_INTERLEAVED_COMPLEX 0
#endif

#include "mxtypes.h"

/* Prototype for the gateway function.  */

extern void mexFunction (int nlhs, mxArray *plhs[],
                         int nrhs, const mxArray *prhs[]);

/* Interface to the interpreter.  */
extern OCTMEX_API const char * mexFunctionName (void);

extern OCTMEX_API int
mexCallMATLAB (int nargout, mxArray *argout[],
               int nargin, mxArray *argin[],
               const char *fname);

extern OCTMEX_API mxArray *
mexCallMATLABWithTrap (int nargout, mxArray *argout[],
                       int nargin, mxArray *argin[],
                       const char *fname);

extern OCTMEX_API int mexEvalString (const char *s);
extern OCTMEX_API mxArray * mexEvalStringWithTrap (const char *s);

extern OCTMEX_API void mexSetTrapFlag (int flag);

extern OCTMEX_API void mexErrMsgTxt (const char *s);
extern OCTMEX_API void mexErrMsgIdAndTxt (const char *id, const char *s, ...);
extern OCTMEX_API void mexWarnMsgTxt (const char *s);
extern OCTMEX_API void mexWarnMsgIdAndTxt (const char *id, const char *s, ...);
extern OCTMEX_API int mexPrintf (const char *fmt, ...);

extern OCTMEX_API mxArray *
mexGetVariable (const char *space, const char *name);
extern OCTMEX_API const mxArray *
mexGetVariablePtr (const char *space, const char *name);

extern OCTMEX_API int
mexPutVariable (const char *space, const char *name, const mxArray *ptr);

#if MX_HAS_INTERLEAVED_COMPLEX
#  define mexGet mexGet_interleaved
#endif
extern OCTMEX_API const mxArray *
mexGet (double handle, const char *property);

extern OCTMEX_API int
mexSet (double handle, const char *property, mxArray *val);

extern OCTMEX_API void mexMakeArrayPersistent (mxArray *ptr);
extern OCTMEX_API void mexMakeMemoryPersistent (void *ptr);

extern OCTMEX_API void mexLock (void);
extern OCTMEX_API void mexUnlock (void);

extern OCTMEX_API int mexIsGlobal (const mxArray *ptr);
extern OCTMEX_API int mexIsLocked (void);

extern OCTMEX_API int mexAtExit (void (*f) (void));

/* Floating point predicates.  */
extern OCTMEX_API bool mxIsFinite (double v);
extern OCTMEX_API bool mxIsInf (double v);
extern OCTMEX_API bool mxIsNaN (double v);

/* Floating point values.  */
extern OCTMEX_API double mxGetEps (void);
extern OCTMEX_API double mxGetInf (void);
extern OCTMEX_API double mxGetNaN (void);

/* Memory management.  */
extern OCTMEX_API void * mxCalloc (size_t n, size_t size);
extern OCTMEX_API void * mxMalloc (size_t n);
extern OCTMEX_API void * mxRealloc (void *ptr, size_t size);
extern OCTMEX_API void mxFree (void *ptr);

/* Constructors.  */
#if MX_HAS_INTERLEAVED_COMPLEX
#  define mxCreateCellArray mxCreateCellArray_interleaved
#  define mxCreateCellMatrix mxCreateCellMatrix_interleaved
#  define mxCreateCharArray mxCreateCharArray_interleaved
#  define mxCreateCharMatrixFromStrings mxCreateCharMatrixFromStrings_interleaved
#  define mxCreateDoubleMatrix mxCreateDoubleMatrix_interleaved
#  define mxCreateDoubleScalar mxCreateDoubleScalar_interleaved
#  define mxCreateLogicalArray mxCreateLogicalArray_interleaved
#  define mxCreateLogicalMatrix mxCreateLogicalMatrix_interleaved
#  define mxCreateLogicalScalar mxCreateLogicalScalar_interleaved
#  define mxCreateNumericArray mxCreateNumericArray_interleaved
#  define mxCreateNumericMatrix mxCreateNumericMatrix_interleaved
#  define mxCreateUninitNumericArray mxCreateUninitNumericArray_interleaved
#  define mxCreateUninitNumericMatrix mxCreateUninitNumericMatrix_interleaved
#  define mxCreateSparse mxCreateSparse_interleaved
#  define mxCreateSparseLogicalMatrix mxCreateSparseLogicalMatrix_interleaved
#  define mxCreateString mxCreateString_interleaved
#  define mxCreateStructArray mxCreateStructArray_interleaved
#  define mxCreateStructMatrix mxCreateStructMatrix_interleaved
#endif

extern OCTMEX_API mxArray *
mxCreateCellArray (mwSize ndims, const mwSize *dims);
extern OCTMEX_API mxArray *
mxCreateCellMatrix (mwSize m, mwSize n);
extern OCTMEX_API mxArray *
mxCreateCharArray (mwSize ndims, const mwSize *dims);
extern OCTMEX_API mxArray *
mxCreateCharMatrixFromStrings (mwSize m, const char **str);
extern OCTMEX_API mxArray *
mxCreateDoubleMatrix (mwSize nr, mwSize nc, mxComplexity flag);
extern OCTMEX_API mxArray *
mxCreateDoubleScalar (double val);
extern OCTMEX_API mxArray *
mxCreateLogicalArray (mwSize ndims, const mwSize *dims);
extern OCTMEX_API mxArray *
mxCreateLogicalMatrix (mwSize m, mwSize n);
extern OCTMEX_API mxArray *
mxCreateLogicalScalar (mxLogical val);
extern OCTMEX_API mxArray *
mxCreateNumericArray (mwSize ndims, const mwSize *dims,
                      mxClassID class_id, mxComplexity flag);
extern OCTMEX_API mxArray *
mxCreateNumericMatrix (mwSize m, mwSize n,
                       mxClassID class_id, mxComplexity flag);
extern OCTMEX_API mxArray *
mxCreateUninitNumericArray (mwSize ndims, const mwSize *dims,
                            mxClassID class_id, mxComplexity flag);
extern OCTMEX_API mxArray *
mxCreateUninitNumericMatrix (mwSize m, mwSize n,
                             mxClassID class_id, mxComplexity flag);

extern OCTMEX_API mxArray *
mxCreateSparse (mwSize m, mwSize n, mwSize nzmax, mxComplexity flag);
extern OCTMEX_API mxArray *
mxCreateSparseLogicalMatrix (mwSize m, mwSize n, mwSize nzmax);
extern OCTMEX_API mxArray *
mxCreateString (const char *str);
extern OCTMEX_API mxArray *
mxCreateStructArray (mwSize ndims, const mwSize *dims,
                     int num_keys, const char **keys);
extern OCTMEX_API mxArray *
mxCreateStructMatrix (mwSize rows, mwSize cols,
                      int num_keys, const char **keys);

/* Copy constructor.  */
extern OCTMEX_API mxArray *mxDuplicateArray (const mxArray *v);

/* Destructor.  */
extern OCTMEX_API void mxDestroyArray (mxArray *v);

/* Type Predicates.  */
extern OCTMEX_API bool mxIsCell (const mxArray *ptr);
extern OCTMEX_API bool mxIsChar (const mxArray *ptr);
extern OCTMEX_API bool mxIsClass (const mxArray *ptr, const char *name);
extern OCTMEX_API bool mxIsComplex (const mxArray *ptr);
extern OCTMEX_API bool mxIsDouble (const mxArray *ptr);
/* Matlab seems to have deprecated IsFunctionHandle, but it seems useful */
extern OCTMEX_API bool mxIsFunctionHandle (const mxArray *ptr);
extern OCTMEX_API bool mxIsInt16 (const mxArray *ptr);
extern OCTMEX_API bool mxIsInt32 (const mxArray *ptr);
extern OCTMEX_API bool mxIsInt64 (const mxArray *ptr);
extern OCTMEX_API bool mxIsInt8 (const mxArray *ptr);
extern OCTMEX_API bool mxIsLogical (const mxArray *ptr);
extern OCTMEX_API bool mxIsNumeric (const mxArray *ptr);
extern OCTMEX_API bool mxIsSingle (const mxArray *ptr);
extern OCTMEX_API bool mxIsSparse (const mxArray *ptr);
extern OCTMEX_API bool mxIsStruct (const mxArray *ptr);
extern OCTMEX_API bool mxIsUint16 (const mxArray *ptr);
extern OCTMEX_API bool mxIsUint32 (const mxArray *ptr);
extern OCTMEX_API bool mxIsUint64 (const mxArray *ptr);
extern OCTMEX_API bool mxIsUint8 (const mxArray *ptr);

/* Odd type+size predicate.  */
extern OCTMEX_API bool mxIsLogicalScalar (const mxArray *ptr);

/* Odd type+size+value predicate.  */
extern OCTMEX_API bool mxIsLogicalScalarTrue (const mxArray *ptr);

/* Size predicates.  */
extern OCTMEX_API bool mxIsEmpty (const mxArray *ptr);
extern OCTMEX_API bool mxIsScalar (const mxArray *ptr);

/* Just plain odd thing to ask of a value.  */
extern OCTMEX_API bool mxIsFromGlobalWS (const mxArray *ptr);

/* Dimension extractors.  */
extern OCTMEX_API size_t mxGetM (const mxArray *ptr);
extern OCTMEX_API size_t mxGetN (const mxArray *ptr);
extern OCTMEX_API const mwSize * mxGetDimensions (const mxArray *ptr);
extern OCTMEX_API mwSize mxGetNumberOfDimensions (const mxArray *ptr);
extern OCTMEX_API size_t mxGetNumberOfElements (const mxArray *ptr);

/* Dimension setters.  */
extern OCTMEX_API void mxSetM (mxArray *ptr, mwSize M);
extern OCTMEX_API void mxSetN (mxArray *ptr, mwSize N);
extern OCTMEX_API int mxSetDimensions (mxArray *ptr, const mwSize *dims,
                                          mwSize ndims);

#if MX_HAS_INTERLEAVED_COMPLEX
extern OCTMEX_API int mxMakeArrayReal (mxArray *ptr);
extern OCTMEX_API int mxMakeArrayComplex (mxArray *ptr);
#endif

/* Data extractors.  */
extern OCTMEX_API double * mxGetPr (const mxArray *ptr);
extern OCTMEX_API double mxGetScalar (const mxArray *ptr);
extern OCTMEX_API mxChar * mxGetChars (const mxArray *ptr);
extern OCTMEX_API mxLogical *mxGetLogicals (const mxArray *ptr);
extern OCTMEX_API void * mxGetData (const mxArray *ptr);
#if MX_HAS_INTERLEAVED_COMPLEX
extern OCTMEX_API mxDouble * mxGetDoubles (const mxArray *p);
extern OCTMEX_API mxSingle * mxGetSingles (const mxArray *p);
extern OCTMEX_API mxInt8 * mxGetInt8s (const mxArray *p);
extern OCTMEX_API mxInt16 * mxGetInt16s (const mxArray *p);
extern OCTMEX_API mxInt32 * mxGetInt32s (const mxArray *p);
extern OCTMEX_API mxInt64 * mxGetInt64s (const mxArray *p);
extern OCTMEX_API mxUint8 * mxGetUint8s (const mxArray *p);
extern OCTMEX_API mxUint16 * mxGetUint16s (const mxArray *p);
extern OCTMEX_API mxUint32 * mxGetUint32s (const mxArray *p);
extern OCTMEX_API mxUint64 * mxGetUint64s (const mxArray *p);

extern OCTMEX_API mxComplexDouble * mxGetComplexDoubles (const mxArray *p);
extern OCTMEX_API mxComplexSingle * mxGetComplexSingles (const mxArray *p);
#else
extern OCTMEX_API double * mxGetPi (const mxArray *ptr);
extern OCTMEX_API void * mxGetImagData (const mxArray *ptr);
#endif

/* Data setters.  */
extern OCTMEX_API void mxSetPr (mxArray *ptr, double *pr);
extern OCTMEX_API void mxSetData (mxArray *ptr, void *data);
#if MX_HAS_INTERLEAVED_COMPLEX
extern OCTMEX_API int mxSetDoubles (mxArray *p, mxDouble *d);
extern OCTMEX_API int mxSetSingles (mxArray *p, mxSingle *d);
extern OCTMEX_API int mxSetInt8s (mxArray *p, mxInt8 *d);
extern OCTMEX_API int mxSetInt16s (mxArray *p, mxInt16 *d);
extern OCTMEX_API int mxSetInt32s (mxArray *p, mxInt32 *d);
extern OCTMEX_API int mxSetInt64s (mxArray *p, mxInt64 *d);
extern OCTMEX_API int mxSetUint8s (mxArray *p, mxUint8 *d);
extern OCTMEX_API int mxSetUint16s (mxArray *p, mxUint16 *d);
extern OCTMEX_API int mxSetUint32s (mxArray *p, mxUint32 *d);
extern OCTMEX_API int mxSetUint64s (mxArray *p, mxUint64 *d);

extern OCTMEX_API int mxSetComplexDoubles (mxArray *p, mxComplexDouble *d);
extern OCTMEX_API int mxSetComplexSingles (mxArray *p, mxComplexSingle *d);
#else
extern OCTMEX_API void mxSetPi (mxArray *ptr, double *pi);
extern OCTMEX_API void mxSetImagData (mxArray *ptr, void *pi);
#endif

/* Classes.  */
extern OCTMEX_API mxClassID mxGetClassID (const mxArray *ptr);
extern OCTMEX_API const char * mxGetClassName (const mxArray *ptr);
extern OCTMEX_API void mxSetClassName (mxArray *ptr, const char *name);
extern OCTMEX_API mxArray *
mxGetProperty (const mxArray *ptr, mwIndex idx, const char *property_name);
extern OCTMEX_API void
mxSetProperty (mxArray *ptr, mwIndex idx,
               const char *property_name, const mxArray *property_value);

/* Cell support.  */
extern OCTMEX_API mxArray * mxGetCell (const mxArray *ptr, mwIndex idx);

extern OCTMEX_API void mxSetCell (mxArray *ptr, mwIndex idx, mxArray *val);

/* Sparse support.  */
extern OCTMEX_API mwIndex * mxGetIr (const mxArray *ptr);
extern OCTMEX_API mwIndex * mxGetJc (const mxArray *ptr);
extern OCTMEX_API mwSize mxGetNzmax (const mxArray *ptr);

extern OCTMEX_API void mxSetIr (mxArray *ptr, mwIndex *ir);
extern OCTMEX_API void mxSetJc (mxArray *ptr, mwIndex *jc);
extern OCTMEX_API void mxSetNzmax (mxArray *ptr, mwSize nzmax);

/* Structure support.  */
extern OCTMEX_API int mxAddField (mxArray *ptr, const char *key);

extern OCTMEX_API void mxRemoveField (mxArray *ptr, int key_num);

extern OCTMEX_API mxArray *
mxGetField (const mxArray *ptr, mwIndex index, const char *key);
extern OCTMEX_API mxArray *
mxGetFieldByNumber (const mxArray *ptr, mwIndex index, int key_num);

extern OCTMEX_API void
mxSetField (mxArray *ptr, mwIndex index, const char *key, mxArray *val);
extern OCTMEX_API void
mxSetFieldByNumber (mxArray *ptr, mwIndex index, int key_num, mxArray *val);

extern OCTMEX_API int mxGetNumberOfFields (const mxArray *ptr);

extern OCTMEX_API const char *
mxGetFieldNameByNumber (const mxArray *ptr, int key_num);
extern OCTMEX_API int mxGetFieldNumber (const mxArray *ptr, const char *key);

extern OCTMEX_API int
mxGetString (const mxArray *ptr, char *buf, mwSize buflen);
extern OCTMEX_API char *mxArrayToString (const mxArray *ptr);

/* Miscellaneous.  */
extern OCTMEX_API mwIndex
mxCalcSingleSubscript (const mxArray *ptr, mwSize nsubs, mwIndex *subs);

extern OCTMEX_API size_t mxGetElementSize (const mxArray *ptr);

#if defined (MEX_DEBUG)

#  define mxAssert(expr, msg)                                           \
  do                                                                    \
    {                                                                   \
      if (! (expr))                                                     \
        {                                                               \
          if (msg && msg[0])                                            \
            mexErrMsgIdAndTxt                                           \
              ("Octave:MEX",                                            \
               "Assertion failed: %s, at line %d of file \"%s\".\n%s\n", \
               #expr, __LINE__, __FILE__, msg);                         \
          else                                                          \
            mexErrMsgIdAndTxt                                           \
              ("Octave:MEX",                                            \
               "Assertion failed: %s, at line %d of file \"%s\".\n",    \
               #expr, __LINE__, __FILE__);                              \
        }                                                               \
    }                                                                   \
  while (0)

#  define mxAssertS(expr, msg)                                          \
  do                                                                    \
    {                                                                   \
      if (! (expr))                                                     \
        {                                                               \
          if (msg && msg[0])                                            \
            mexErrMsgIdAndTxt                                           \
              ("Octave:MEX",                                            \
               "Assertion failed at line %d of file \"%s\".\n%s\n",     \
               __LINE__, __FILE__, msg);                                \
          else                                                          \
            mexErrMsgIdAndTxt                                           \
              ("Octave:MEX",                                            \
               "Assertion failed at line %d of file \"%s\".\n",         \
               __LINE__, __FILE__);                                     \
        }                                                               \
    }                                                                   \
  while (0)

#else
#  define mxAssert(expr, msg)
#  define mxAssertS(expr, msg)
#endif

#if defined (__cplusplus)
}
#endif

#endif
