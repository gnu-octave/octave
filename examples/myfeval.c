/*

Copyright (C) 2006, 2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#include "mex.h"

void
mexFunction (int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
  char *str;

  mexPrintf ("Hello, World!\n");

  mexPrintf ("I have %d inputs and %d outputs\n", nrhs, nlhs);

  if (nrhs < 1 || ! mxIsString (prhs[0])) 
    mexErrMsgTxt ("function name expected");

  str = mxArrayToString (prhs[0]);

  mexPrintf ("I'm going to call the interpreter function %s\n", str);

  mexCallMATLAB (nlhs, plhs, nrhs-1, prhs+1, str);

  mxFree (str);
}
