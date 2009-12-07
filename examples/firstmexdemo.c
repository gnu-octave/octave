#include "mex.h"

void
mexFunction (int nlhs, mxArray *plhs[], int nrhs, 
             const mxArray *prhs[])
{
  mxArray *v = mxCreateDoubleMatrix (1, 1, mxREAL);
  double *data = mxGetPr (v);
  *data = 1.23456789;
  plhs[0] = v;
}
