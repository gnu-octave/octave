// %NO_EDIT_WARNING%

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

Part of this code was originally distributed as part of Octave Forge under
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

#if ! defined (octave_mxtypes_h)
#define octave_mxtypes_h 1

#include "octave-config.h"

typedef enum
  {
  mxUNKNOWN_CLASS = 0,
  mxCELL_CLASS,
  mxSTRUCT_CLASS,
  mxLOGICAL_CLASS,
  mxCHAR_CLASS,
  mxVOID_CLASS,
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
  mxFUNCTION_CLASS
}
mxClassID;

typedef enum
  {
  mxREAL = 0,
  mxCOMPLEX = 1
}
mxComplexity;

/* Matlab uses a wide char (uint16) internally, but Octave uses plain char. */
/* typedef Uint16 mxChar; */
typedef char mxChar;

typedef unsigned char mxLogical;

typedef double mxDouble;
typedef float mxSingle;

typedef int8_t mxInt8;
typedef int16_t mxInt16;
typedef int32_t mxInt32;
typedef int64_t mxInt64;

typedef uint8_t mxUint8;
typedef uint16_t mxUint16;
typedef uint32_t mxUint32;
typedef uint64_t mxUint64;

typedef struct { mxDouble real; mxDouble imag; } mxComplexDouble;
typedef struct { mxSingle real; mxSingle imag; } mxComplexSingle;

/* We don't have these yet but we can define the types. */
typedef struct { mxInt8 real; mxInt8 imag; } mxComplexInt8;
typedef struct { mxInt16 real; mxInt16 imag; } mxComplexInt16;
typedef struct { mxInt32 real; mxInt32 imag; } mxComplexInt32;
typedef struct { mxInt64 real; mxInt64 imag; } mxComplexInt64;

typedef struct { mxUint8 real; mxUint8 imag; } mxComplexUint8;
typedef struct { mxUint16 real; mxUint16 imag; } mxComplexUint16;
typedef struct { mxUint32 real; mxUint32 imag; } mxComplexUint32;
typedef struct { mxUint64 real; mxUint64 imag; } mxComplexUint64;

/*
 * FIXME: Mathworks says mwSize, mwIndex should be int generally.
 * But on 64-bit systems, or when mex -largeArrayDims is used, it is size_t.
 * mwSignedIndex is supposed to be ptrdiff_t.  All of this is confusing.
 * Its better to conform to the same indexing as the rest of Octave.
 */
typedef %OCTAVE_IDX_TYPE% mwSize;
typedef %OCTAVE_IDX_TYPE% mwIndex;
typedef %OCTAVE_IDX_TYPE% mwSignedIndex;

#endif
