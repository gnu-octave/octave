#include <mex.h>

// To be called with
//
//   single array
//   complex single array
//   double array
//   complex double array
//
// Will return arrays of the same type, but created internally to test
// the mxArray -> octave_value conversion

void
mexFunction (int nlhs, mxArray *plhs[],
             int nrhs, const mxArray *prhs[])
{
  if (nrhs != 4 || nlhs != 4)
    mexErrMsgTxt ("invalid arguments");

  const mxArray *sngl_ra = prhs[0];
  const mxArray *cplx_sngl_ra = prhs[1];
  const mxArray *dble_ra = prhs[2];
  const mxArray *cplx_dble_ra = prhs[3];

#if MX_HAS_INTERLEAVED_COMPLEX

  mxSingle *sngl_data = mxGetSingles (sngl_ra);
  size_t sngl_data_nr = mxGetM (sngl_ra);
  size_t sngl_data_nc = mxGetN (sngl_ra);

  plhs[0] = mxCreateNumericMatrix (sngl_data_nr, sngl_data_nc, mxSINGLE_CLASS, mxREAL);
  mxSetSingles (plhs[0], sngl_data);

  mxComplexSingle *cplx_sngl_data = mxGetComplexSingles (cplx_sngl_ra);
  size_t cplx_sngl_data_nr = mxGetM (cplx_sngl_ra);
  size_t cplx_sngl_data_nc = mxGetN (cplx_sngl_ra);

  plhs[1] = mxCreateNumericMatrix (cplx_sngl_data_nr, cplx_sngl_data_nc, mxSINGLE_CLASS, mxCOMPLEX);
  mxSetComplexSingles (plhs[1], cplx_sngl_data);

  mxDouble *dble_data = mxGetDoubles (dble_ra);
  size_t dble_data_nr = mxGetM (dble_ra);
  size_t dble_data_nc = mxGetN (dble_ra);

  plhs[2] = mxCreateNumericMatrix (dble_data_nr, dble_data_nc, mxDOUBLE_CLASS, mxREAL);
  mxSetDoubles (plhs[2], dble_data);

  mxComplexDouble *cplx_dble_data = mxGetComplexDoubles (cplx_dble_ra);
  size_t cplx_dble_data_nr = mxGetM (cplx_dble_ra);
  size_t cplx_dble_data_nc = mxGetN (cplx_dble_ra);

  plhs[3] = mxCreateNumericMatrix (cplx_dble_data_nr, cplx_dble_data_nc, mxDOUBLE_CLASS, mxCOMPLEX);
  mxSetComplexDoubles (plhs[3], cplx_dble_data);

#else

  mxSingle *sngl_data = (mxSingle *) mxGetData (sngl_ra);
  size_t sngl_data_nr = mxGetM (sngl_ra);
  size_t sngl_data_nc = mxGetN (sngl_ra);

  mxSingle *cplx_sngl_data_real = (mxSingle *) mxGetData (cplx_sngl_ra);
  mxSingle *cplx_sngl_data_imag = (mxSingle *) mxGetImagData (cplx_sngl_ra);
  size_t cplx_sngl_data_nr = mxGetM (cplx_sngl_ra);
  size_t cplx_sngl_data_nc = mxGetN (cplx_sngl_ra);

  mxDouble *dble_data = (mxDouble *) mxGetData (dble_ra);
  size_t dble_data_nr = mxGetM (dble_ra);
  size_t dble_data_nc = mxGetN (dble_ra);

  mxDouble *cplx_dble_data_real = (mxDouble *) mxGetData (cplx_dble_ra);
  mxDouble *cplx_dble_data_imag = (mxDouble *) mxGetImagData (cplx_dble_ra);
  size_t cplx_dble_data_nr = mxGetM (cplx_dble_ra);
  size_t cplx_dble_data_nc = mxGetN (cplx_dble_ra);

  plhs[0] = mxCreateNumericMatrix (sngl_data_nr, sngl_data_nc, mxSINGLE_CLASS, mxREAL);
  mxSetData (plhs[0], sngl_data);

  plhs[1] = mxCreateNumericMatrix (cplx_sngl_data_nr, cplx_sngl_data_nc, mxSINGLE_CLASS, mxCOMPLEX);
  mxSetData (plhs[1], cplx_sngl_data_real);
  mxSetImagData (plhs[1], cplx_sngl_data_imag);

  plhs[2] = mxCreateNumericMatrix (dble_data_nr, dble_data_nc, mxDOUBLE_CLASS, mxREAL);
  mxSetData (plhs[2], dble_data);

  plhs[3] = mxCreateNumericMatrix (cplx_dble_data_nr, cplx_dble_data_nc, mxDOUBLE_CLASS, mxCOMPLEX);
  mxSetData (plhs[3], cplx_dble_data_real);
  mxSetImagData (plhs[3], cplx_dble_data_imag);

#endif

}
