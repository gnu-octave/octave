/*

Copyright (C) 2006, 2007 David Bateman

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
mexFunction (int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  mwSize n, m, nz;
  mxArray *v;
  mwIndex i;
  double *pr, *pi;
  double *pr2, *pi2;
  mwIndex *ir, *jc;
  mwIndex *ir2, *jc2;
  
  if (nrhs != 1 || ! mxIsSparse (prhs[0]))
    mexErrMsgTxt ("expects sparse matrix");

  m = mxGetM (prhs [0]);
  n = mxGetN (prhs [0]);
  nz = mxGetNzmax (prhs [0]);
  
  if (mxIsComplex (prhs[0]))
    {
      mexPrintf ("Matrix is %d-by-%d complex sparse matrix", m, n);
      mexPrintf (" with %d elements\n", nz);

      pr = mxGetPr (prhs[0]);
      pi = mxGetPi (prhs[0]);
      ir = mxGetIr (prhs[0]);
      jc = mxGetJc (prhs[0]);

      i = n;
      while (jc[i] == jc[i-1] && i != 0) i--;
      mexPrintf ("last non-zero element (%d, %d) = (%g, %g)\n", 
                ir[nz-1]+ 1, i, pr[nz-1], pi[nz-1]);

      v = mxCreateSparse (m, n, nz, mxCOMPLEX);
      pr2 = mxGetPr (v);
      pi2 = mxGetPi (v);
      ir2 = mxGetIr (v);
      jc2 = mxGetJc (v);
      
      for (i = 0; i < nz; i++)
        {
          pr2[i] = 2 * pr[i];
          pi2[i] = 2 * pi[i];
          ir2[i] = ir[i];
        }
      for (i = 0; i < n + 1; i++)
        jc2[i] = jc[i];

      if (nlhs > 0)
        plhs[0] = v;
    }
  else if (mxIsLogical (prhs[0]))
    {
      bool *pbr, *pbr2;
      mexPrintf ("Matrix is %d-by-%d logical sparse matrix", m, n);
      mexPrintf (" with %d elements\n", nz);

      pbr = mxGetLogicals (prhs[0]);
      ir = mxGetIr (prhs[0]);
      jc = mxGetJc (prhs[0]);

      i = n;
      while (jc[i] == jc[i-1] && i != 0) i--;
      mexPrintf ("last non-zero element (%d, %d) = %d\n", ir[nz-1]+ 1, 
                i, pbr[nz-1]);

      v = mxCreateSparseLogicalMatrix (m, n, nz);
      pbr2 = mxGetLogicals (v);
      ir2 = mxGetIr (v);
      jc2 = mxGetJc (v);
      
      for (i = 0; i < nz; i++)
        {
          pbr2[i] = pbr[i];
          ir2[i] = ir[i];
        }
      for (i = 0; i < n + 1; i++)
        jc2[i] = jc[i];

      if (nlhs > 0)
        plhs[0] = v;
    }
  else
    {
      mexPrintf ("Matrix is %d-by-%d real sparse matrix", m, n);
      mexPrintf (" with %d elements\n", nz);

      pr = mxGetPr (prhs[0]);
      ir = mxGetIr (prhs[0]);
      jc = mxGetJc (prhs[0]);

      i = n;
      while (jc[i] == jc[i-1] && i != 0) i--;
      mexPrintf ("last non-zero element (%d, %d) = %g\n", ir[nz-1]+ 1, 
                i, pr[nz-1]);

      v = mxCreateSparse (m, n, nz, mxREAL);
      pr2 = mxGetPr (v);
      ir2 = mxGetIr (v);
      jc2 = mxGetJc (v);
      
      for (i = 0; i < nz; i++)
        {
          pr2[i] = 2 * pr[i];
          ir2[i] = ir[i];
        }
      for (i = 0; i < n + 1; i++)
        jc2[i] = jc[i];

      if (nlhs > 0)
        plhs[0] = v;
    }
}
