/*

Copyright (C) 2006, 2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or 
modify it under the terms of the GNU General Public License 
as published by the Free Software Foundation; either
version 3  of the License, or (at your option) any later 
version.

Octave is distributed in the hope that it will be useful, 
but WITHOUT ANY WARRANTY; without even the implied warranty
of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public 
License along with Octave; see the file COPYING.  If not,
see <http://www.gnu.org/licenses/>.

*/

#include "mex.h"

void
mexFunction (int nlhs, mxArray* plhs[], int nrhs, 
	     const mxArray* prhs[])
{
  mwIndex i;
  mwSize n;
  double *vri, *vro;
  
  if (nrhs != 1 || ! mxIsNumeric (prhs[0]))
    mexErrMsgTxt ("expects matrix");

  n = mxGetNumberOfElements (prhs[0]);
  plhs[0] = (mxArray *) mxCreateNumericArray 
    (mxGetNumberOfDimensions (prhs[0]),
     mxGetDimensions (prhs[0]), mxGetClassID (prhs[0]),
     mxIsComplex (prhs[0]));
  vri = mxGetPr (prhs[0]);
  vro = mxGetPr (plhs[0]);

  if (mxIsComplex (prhs[0]))
    {
      double *vii, *vio;
      vii = mxGetPi (prhs[0]);
      vio = mxGetPi (plhs[0]);

      for (i = 0; i < n; i++)
	{
	  vro [i] = vri [i] * vri [i] - vii [i] * vii [i];
	  vio [i] = 2 * vri [i] * vii [i];
	}
    }
  else
    {
      for (i = 0; i < n; i++)
	vro [i] = vri [i] * vri [i];
    }
}
