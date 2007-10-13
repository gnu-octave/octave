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

#include <string.h>
#include "mex.h"

void
mexFunction (int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  mwIndex i, j;
  mwSize m, n;
  mxChar *pi, *po;

  if (nrhs != 1 || ! mxIsChar (prhs[0]) || 
      mxGetNumberOfDimensions (prhs[0]) > 2)
    mexErrMsgTxt ("expecting char matrix");

  m = mxGetM (prhs[0]);
  n = mxGetN (prhs[0]);
  pi = mxGetChars (prhs[0]);
  plhs[0] = mxCreateNumericMatrix (m, n, mxCHAR_CLASS, mxREAL);
  po = mxGetChars (plhs[0]);

  for (j = 0; j < n; j++)
    for (i = 0; i < m; i++)
      po [j*m + m - 1 - i] = pi [j*m + i];
}
