#include "mex.h"

void
mexFunction (int nlhs, mxArray* plhs[],
             int nrhs, const mxArray* prhs[])
{
  char *str;
  mxArray *v;

  if (nrhs != 2 || ! mxIsString (prhs[0]))
    mexErrMsgTxt ("expects symbol name and value");

  str = mxArrayToString (prhs[0]);

  v = mexGetArray (str, "global");

  if (v)
    {
      mexPrintf ("%s is a global variable with the following value:\n", str);
      mexCallMATLAB (0, NULL, 1, &v, "disp");
    }

  v = mexGetArray (str, "caller");

  if (v)
    {
      mexPrintf ("%s is a caller variable with the following value:\n", str);
      mexCallMATLAB (0, NULL, 1, &v, "disp");
    }

  // WARNING!! Can't do this in MATLAB!  Must copy variable first.
  mxSetName (prhs[1], str);  
  mexPutArray (prhs[1], "caller");
}
