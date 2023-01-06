////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2001-2023 The Octave Project Developers
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

#if ! defined (octave_mex_h)
#define octave_mex_h 1

#include "octave-config.h"

#define HAVE_OCTAVE

typedef void mxArray;

/* -V4 stuff */
#if defined (V4)
#  define Matrix mxArray
#  define REAL mxREAL
#endif

#define mxMAXNAME 64

#include "mexproto.h"

#if defined (__cplusplus)
extern "C" {
#endif

#if defined (V4)
void mexFunction (int nlhs, mxArray *plhs[], int nrhs, mxArray *prhs[]);
#else
void mexFunction (int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]);
#endif

/* V4 floating point routines renamed in V5.  */
#define mexIsNaN mxIsNaN
#define mexIsFinite mxIsFinite
#define mexIsInf mxIsInf
#define mexGetEps mxGetEps
#define mexGetInf mxGetInf
#define mexGetNaN mxGetNaN

#define mexGetGlobal(nm) mexGetArray (nm, "global")
#define mexGetMatrix(nm) mexGetArray (nm, "caller")
#define mexGetMatrixPtr(nm) mexGetArrayPtr (nm, "caller")

#define mexGetArray(nm, space) mexGetVariable (space, nm)
#define mexGetArrayPtr(nm, space) mexGetVariablePtr (space, nm)

#define mexPutMatrix(ptr) mexPutVariable ("caller", "", ptr)
#define mexPutArray(ptr, space) mexPutVariable (space, "", ptr)

#define mxCreateFull mxCreateDoubleMatrix

#define mxCreateScalarDouble mxCreateDoubleScalar

#define mxFreeMatrix mxDestroyArray

#define mxIsString mxIsChar

/* Apparently these are also defined.  */

#if ! defined (UINT64_T)
#  define UINT64_T uint64_t
#endif

#if ! defined (uint64_T)
#  define uint64_T uint64_t
#endif

#if ! defined (INT64_T)
#  define INT64_T int64_t
#endif

#if ! defined (int64_T)
#  define int64_T int64_t
#endif

#if ! defined (UINT32_T)
#  define UINT32_T uint32_t
#endif

#if ! defined (uint32_T)
#  define uint32_T uint32_t
#endif

#if ! defined (INT32_T)
#  define INT32_T int32_t
#endif

#if ! defined (int32_T)
#  define int32_T int32_t
#endif

#if ! defined (UINT16_T)
#  define UINT16_T uint16_t
#endif

#if ! defined (uint16_T)
#  define uint16_T uint16_t
#endif

#if ! defined (INT16_T)
#  define INT16_T int16_t
#endif

#if ! defined (int16_T)
#  define int16_T int16_t
#endif

#if ! defined (UINT8_T)
#  define UINT8_T uint8_t
#endif

#if ! defined (uint8_T)
#  define uint8_T uint8_t
#endif

#if ! defined (INT8_T)
#  define INT8_T int8_t
#endif

#if ! defined (int8_T)
#  define int8_T int8_t
#endif

#if defined (__cplusplus)
}
#endif

#endif
