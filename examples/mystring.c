#include <string.h>
#include "mex.h"

void
mexFunction (int nlhs, mxArray *plhs[], int nrhs, 
             const mxArray *prhs[])
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
  plhs[0] = mxCreateNumericMatrix (m, n, mxCHAR_CLASS, 
                                   mxREAL);
  po = mxGetChars (plhs[0]);

  for (j = 0; j < n; j++)
    for (i = 0; i < m; i++)
      po [j*m + m - 1 - i] = pi [j*m + i];
}
