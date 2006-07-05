/*

Copyright (C) 2001, 2006 Paul Kienzle

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
the follwoing terms:

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

#if ! defined (MEX_H)
#define MEX_H

#define HAVE_OCTAVE

typedef void mxArray;

enum mxComplexity
  {
    mxREAL = 0,
    mxCOMPLEX = 1
  };

typedef enum
  {
    mxUNKNOWN_CLASS = 0,
    mxCELL_CLASS,
    mxSTRUCT_CLASS,
    mxLOGICAL_CLASS,
    mxCHAR_CLASS,
    mxUNUSED_CLASS,
    mxDOUBLE_CLASS,
    mxSINGLE_CLASS,
    mxINT8_CLASS,
    mxUINT8_CLASS,
    mxINT16_CLASS,
    mxUINT16_CLASS,
    mxINT32_CLASS,
    mxUINT32_CLASS,
    mxINT64_CLASS,
    mxUINT64_CLASS,
    mxFUNCTION_CLASS,
  } mxClassID;

#if 0
/* typedef Uint16 mxChar; */
typedef unsigned short mxChar;
#endif

#if ! defined (__cplusplus)
typedef int bool;
#endif

typedef int mxLOGICAL;

/* -V4 stuff */
#if defined (V4)
#define Matrix mxArray
#define REAL mxREAL
#endif

#define mxMAXNAME 64

#if defined (__cplusplus)
extern "C" {
#endif

#if defined (V4)
void mexFunction (int nlhs, mxArray* plhs[], int nrhs, mxArray *prhs[]);
#else
void mexFunction (int nlhs, mxArray* plhs[], int nrhs, const mxArray *prhs[]);
#endif
  
#include "mexproto.h"

/* V4 floating point routines renamed in V5.  */
#define mexIsNaN mxIsNaN
#define mexIsFinite mxIsFinite
#define mexIsInf mxIsInf
#define mexGetEps mxGetEps
#define mexGetInf mxGetInf
#define mexGetNaN mxGetNan
  
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

#if defined (__cplusplus)
}
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C ***
;;; End: ***
*/
