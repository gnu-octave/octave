/*

Copyright (C) 2007 John W. Eaton

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
  double handle;
  char property[256];

  if (nrhs < 2 || nrhs > 3)
    mexErrMsgTxt ("incorrect number of arguments");
  if (!mxIsDouble(prhs[0]))
    mexErrMsgTxt ("handle expected to be a double scalar");
  if (!mxIsChar (prhs[1]))
    mexErrMsgTxt ("expected property to be a string");
  
  handle = mxGetScalar (prhs[0]);
  mxGetString (prhs[1], property, 256);
  plhs[0] = mxDuplicateArray (mexGet (handle, property));
  
  if (nrhs == 3)
    if (mexSet (handle, property, mxDuplicateArray (prhs[2])))
      mexErrMsgTxt ("failed to set property");
}
  

