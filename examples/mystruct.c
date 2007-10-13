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
  int i;
  mwIndex j;
  mxArray *v;
  const char *keys[] = { "this", "that" };

  if (nrhs != 1 || ! mxIsStruct (prhs[0]))
    mexErrMsgTxt ("expects struct");

  for (i = 0; i < mxGetNumberOfFields (prhs[0]); i++)
    for (j = 0; j < mxGetNumberOfElements (prhs[0]); j++)
      {
        mexPrintf ("field %s(%d) = ", 
                   mxGetFieldNameByNumber (prhs[0], i), j);
        v = mxGetFieldByNumber (prhs[0], j, i);
        mexCallMATLAB (0, 0, 1, &v, "disp");
      }

  v = mxCreateStructMatrix (2, 2, 2, keys);

  mxSetFieldByNumber (v, 0, 0, mxCreateString ("this1"));
  mxSetFieldByNumber (v, 0, 1, mxCreateString ("that1"));
  mxSetFieldByNumber (v, 1, 0, mxCreateString ("this2"));
  mxSetFieldByNumber (v, 1, 1, mxCreateString ("that2"));
  mxSetFieldByNumber (v, 2, 0, mxCreateString ("this3"));
  mxSetFieldByNumber (v, 2, 1, mxCreateString ("that3"));
  mxSetFieldByNumber (v, 3, 0, mxCreateString ("this4"));
  mxSetFieldByNumber (v, 3, 1, mxCreateString ("that4"));

  if (nlhs)
    plhs[0] = v;
}
