# include "oct-sundials.h"

#ifdef __cplusplus
extern "C" {
#endif

using namespace octave;

/* --------------------------------------------------------------------------
 * Utility functions
 * -------------------------------------------------------------------------- */
/* Private functions for special cases of vector operations */
static void VCopy_Octave(N_Vector x, N_Vector z); 

/* --------------------------------------------------------------------------
 * Constructors
 * -------------------------------------------------------------------------- */

/* Returns vector type ID. Used to identify vector implementation
  from abstract N_Vector interface. */
N_Vector_ID N_VGetVectorID_Octave([[maybe_unused]] N_Vector v)
{
  return SUNDIALS_NVEC_CUSTOM;
}

/* Function to create a new empty octave vector */

  N_Vector N_VNewEmpty_Octave(SP_ARG_SUNCONTEXT)
{
  N_Vector v;

  /* Create an empty vector object */
  v = NULL;
  v = N_VNewEmpty(SP_OCTAVE_SUNCONTEXT);
  if (v == NULL)
    return (NULL);

  /* Attach operations */

  /* constructors, destructors, and utility operations */
  v->ops->nvgetvectorid     = N_VGetVectorID_Octave;
  v->ops->nvclone           = N_VClone_Octave;
  v->ops->nvcloneempty      = N_VCloneEmpty_Octave;
  v->ops->nvdestroy         = N_VDestroy_Octave;
  v->ops->nvspace           = N_VSpace_Octave;
  v->ops->nvgetarraypointer = N_VGetArrayPointer_Octave;
  v->ops->nvsetarraypointer = NULL;
  v->ops->nvgetlength       = N_VGetLength_Octave;

  /* standard vector operations */
  v->ops->nvlinearsum    = N_VLinearSum_Octave;
  v->ops->nvconst        = N_VConst_Octave;
  v->ops->nvprod         = N_VProd_Octave;
  v->ops->nvdiv          = N_VDiv_Octave;
  v->ops->nvscale        = N_VScale_Octave;
  v->ops->nvabs          = N_VAbs_Octave;
  v->ops->nvinv          = N_VInv_Octave;
  v->ops->nvaddconst     = N_VAddConst_Octave;
  v->ops->nvdotprod      = N_VDotProd_Octave;
  v->ops->nvmaxnorm      = N_VMaxNorm_Octave;
  v->ops->nvwrmsnormmask = N_VWrmsNormMask_Octave;
  v->ops->nvwrmsnorm     = N_VWrmsNorm_Octave;
  v->ops->nvmin          = N_VMin_Octave;
  v->ops->nvwl2norm      = NULL;
  v->ops->nvl1norm       = NULL;
  v->ops->nvcompare      = N_VCompare_Octave;
  v->ops->nvinvtest      = N_VInvTest_Octave;
  v->ops->nvconstrmask   = N_VConstrMask_Octave;
  v->ops->nvminquotient  = N_VMinQuotient_Octave;

  /* fused vector operations (optional) */
  v->ops->nvlinearcombination = NULL;
  v->ops->nvscaleaddmulti = N_VScaleAddMulti_Octave;
  v->ops->nvdotprodmulti = NULL;

  // /* vector array operations (optional) */
  v->ops->nvlinearsumvectorarray = N_VLinearSumVectorArray_Octave;
  v->ops->nvscalevectorarray = N_VScaleVectorArray_Octave;
  v->ops->nvconstvectorarray = NULL;
  v->ops->nvwrmsnormvectorarray = NULL;
  v->ops->nvwrmsnormmaskvectorarray = NULL;
  v->ops->nvscaleaddmultivectorarray = NULL;
  v->ops->nvlinearcombinationvectorarray = NULL;

  /*
   * OPTIONAL operations with no default implementation.
   */

  /* local reduction operations */
  v->ops->nvdotprodlocal     = NULL;
  v->ops->nvmaxnormlocal     = NULL;
  v->ops->nvminlocal         = NULL;
  v->ops->nvl1normlocal      = NULL;
  v->ops->nvinvtestlocal     = NULL;
  v->ops->nvconstrmasklocal  = NULL;
  v->ops->nvminquotientlocal = NULL;
  v->ops->nvwsqrsumlocal     = N_VWSqrSumLocal_Octave;
  v->ops->nvwsqrsummasklocal = N_VWSqrSumMaskLocal_Octave;

  /* debugging functions */
  v->ops->nvprint = N_VPrint_Octave;
  v->ops->nvprintfile = NULL;

  return v;
}

/* constructor to create a new nvector implementation
  with a required length */
N_Vector N_VNew_Octave(int length ARG_SUNCONTEXT)
{
  N_Vector v;
  void *content;
  #  if defined (HAVE_SUNDIALS_SUNCONTEXT)
  if (m_sunContext == NULL)
    return (NULL);
  #endif
  
  /* Create an empty vector object */
  v = NULL;
  v = N_VNewEmpty_Octave(SP_OCTAVE_SUNCONTEXT);
  if (v == NULL)
    return (NULL);

  /* Create data */
  if (length > 0) {

  /* Allocate memory & Create content */
  content = NULL;  
  content = new ColumnVector (length);
  if (content == NULL)
  {
    N_VDestroy(v);
    return (NULL);
  }
  /* Attach content */
  v->content = content;
  if(NV_DATA_C(v) == NULL) { N_VDestroy_Octave(v); return(NULL); }

}
return (v);
}
/* Constructor to create a serial N_Vector
  from existing ColumnVector component */

N_Vector N_VMake_Octave(const ColumnVector& cv ARG_SUNCONTEXT)
{
  N_Vector v;
  void *content;

  /* Create an empty vector object */
  v = NULL;
  v = N_VNewEmpty_Octave(SP_OCTAVE_SUNCONTEXT);
  if (v == NULL)
    return (NULL);

  /* Create content to point to allocated memory */
  content = NULL;
  content = const_cast<ColumnVector *> (&cv);
  
  if (content == NULL)
  {
    N_VDestroy(v);
    return (NULL);
  }

  /* Attach content */
  v->content = content;
  return v;
}

/* --------------------------------------------------------------------------
 * Vector get, set, and utility functions
 * -------------------------------------------------------------------------- */

/* Function to return the global length of the vector.*/
sunindextype N_VGetLength_Octave(N_Vector v)
{
  return NV_LENGTH_C(v);
}

/* Function to print the vector vector to stdout */
void N_VPrint_Octave(N_Vector x)
{
  ColumnVector *xv;
  xv = static_cast<ColumnVector *> NV_CONTENT_C(x);

  std::cout<<(*xv);

  return;
}

/* --------------------------------------------------------------------------
 * Vector operations implementations
 * -------------------------------------------------------------------------- */


N_Vector N_VCloneEmpty_Octave(N_Vector w)
{
  N_Vector v;

  /* Check input */
  if (w == NULL) return(NULL);

  /* Create vector */
  v = NULL;
  #  if defined (HAVE_SUNDIALS_SUNCONTEXT)
    v = N_VNewEmpty_Octave(w->sunctx);
  #else
    v = N_VNewEmpty_Octave();
  #endif
  if (v == NULL) return(NULL);

  /* Attach operations */
  if (N_VCopyOps(w, v)) 
  { 
    N_VDestroy(v); 
    return(NULL); 
  }

  return(v);
}

N_Vector N_VClone_Octave(N_Vector w)
{
  void* content;

  /* check inputs */
  if (w == NULL) return(NULL);

  /* Create an empty clone vector */
  N_Vector v = N_VCloneEmpty_Octave(w);
  if (v == NULL) return(NULL);
  /* Create content */
  content = NULL;
  content = new ColumnVector (NV_LENGTH_C(w));

  if (content == NULL) { N_VDestroy(v); return(NULL); }

  /* Attach content */
  v->content = content;
  
  return(v);
}

void N_VDestroy_Octave(N_Vector v)
{
  if (v == NULL) return;

  /* free content */
  if (v->content != NULL) {
    // our ColumnVector was created with 'new', so use 'delete' to destroy it.
    delete NV_CONTENT_C(v);
    v->content = NULL;
  }

  /* free ops and vector */
  if (v->ops != NULL) { free(v->ops); v->ops = NULL; }
  free(v);
   v = NULL;

  return;
}

void N_VSpace_Octave(N_Vector v, sunindextype *lrw, sunindextype *liw)
{
  *lrw = NV_LENGTH_C(v);
  *liw = 1;

  return;
}

realtype *N_VGetArrayPointer_Octave(N_Vector v)
{
  return((realtype *) NV_DATA_C(v));
}

/*
 * Linear combination of two vectors: z = a*x + b*y
 */
void N_VLinearSum_Octave(realtype a, N_Vector x, realtype b, N_Vector y, N_Vector z)
{

  ColumnVector *xv, *yv, *zv;
  xv = static_cast <ColumnVector *> NV_CONTENT_C(x);
  yv = static_cast <ColumnVector *> NV_CONTENT_C(y);
  zv = static_cast <ColumnVector *> NV_CONTENT_C(z);
  booleantype test;

  /* Case: a == b == 1.0 */

  if ((a == ONE) && (b == ONE)) {
    (*zv) = (*xv) + (*yv);
    return;
  }

  /* Cases: (1) a == 1.0, b = -1.0, (2) a == -1.0, b == 1.0 */

  if ((test = ((a == ONE) && (b == -ONE))) || ((a == -ONE) && (b == ONE))) {
    if(test == 0)
    (*zv) = (*yv) - (*xv);
    else
    (*zv) = (*xv) - (*yv);
    return;
  }

  /* Cases: (1) a == 1.0, b == other or 0.0, (2) a == other or 0.0, b == 1.0 */
  /* if a or b is 0.0, then user should have called N_VScale */

  if ((test = (a == ONE)) || (b == ONE)) {
    if(test == 1)
    (*zv) = (*xv) + b * (*yv);
    else
    (*zv) = a * (*xv) + (*yv);
    return;
  }

  /* Cases: (1) a == -1.0, b != 1.0, (2) a != 1.0, b == -1.0 */

  if ((test = (a == -ONE)) || (b == -ONE)) {
    if(test == 1)
    (*zv) = b * (*yv) - (*xv);
    else
    (*zv) = a * (*xv) - (*yv);
    return;
  }

  /* Case: a == b */
  /* catches case both a and b are 0.0 - user should have called N_VConst */

  if (a == b) {
    (*zv) = a * ((*yv) + (*xv));
    return;
  }

  /* Case: a == -b */

  if (a == -b) {
    (*zv) = b * ((*yv) - (*xv));
    return;
  }

  /* Do all cases not handled above:
    (1) a == other, b == 0.0 - user should have called N_VScale
    (2) a == 0.0, b == other - user should have called N_VScale
    (3) a,b == other, a !=b, a != -b */

  (*zv) = a * (*xv) + b * (*yv);
  return;
}

/*
 * Set all vector elements to a constant: z[i] = c
 */
void N_VConst_Octave(realtype c, N_Vector z)
{
  ColumnVector *zv;
  zv = static_cast<ColumnVector *> NV_CONTENT_C(z);

  zv->fill(c);

  return;
}

/*
 * Elementwise multiply vectors: z[i] = x[i]*y[i]
 */
void N_VProd_Octave(N_Vector x, N_Vector y, N_Vector z)
{

  ColumnVector *xv, *zv, *yv;
  xv = static_cast<ColumnVector *> NV_CONTENT_C(x);
  zv = static_cast<ColumnVector *> NV_CONTENT_C(z);
  yv = static_cast<ColumnVector *> NV_CONTENT_C(y);
  *zv = product((*xv),(*yv));

  return;
}

/*
 * Elementwise divide vectors: z[i] = x[i]/y[i]
 */
void N_VDiv_Octave(N_Vector x, N_Vector y, N_Vector z)
{

  ColumnVector *xv, *zv, *yv;
  xv = static_cast<ColumnVector *> NV_CONTENT_C(x);
  zv = static_cast<ColumnVector *> NV_CONTENT_C(z);
  yv = static_cast<ColumnVector *> NV_CONTENT_C(y);
  *zv = quotient((*xv),(*yv));

  return;
}

/*
 * Scale vector: z = c*x
 */
void N_VScale_Octave(realtype c, N_Vector x, N_Vector z)
{
  ColumnVector *xv = const_cast <ColumnVector *> NV_CONTENT_C(x);
  ColumnVector *zv = const_cast <ColumnVector *> NV_CONTENT_C(z);

  // checks if both NVectors z and x
  // are stored in the same place
  if (zv == xv) {  /* BLAS usage: scale x <- cx */
    (*xv) *= c;
    // Not using this results in poor iterative
    // algorthm performance
    return;
  }
  if (c == ONE) {
    VCopy_Octave(x, z);  // substituting with (*zv) = (*xv) leads to wrong results.
  } else if (c == -ONE) {
    (*zv) -= (*xv);
  } else {
    (*zv) = c * (*xv);
  }

  return;

}

/*
 * Elementwise absolute value: z[i] = |x[i]|
 */
void N_VAbs_Octave(N_Vector x, N_Vector z)
{
  ColumnVector *xv,*zv;
  xv = static_cast <ColumnVector *> NV_CONTENT_C(x);
  zv = static_cast <ColumnVector *> NV_CONTENT_C(z);

  (*zv) = xv->abs();
  return;
}

/*
 * Elementwise inverse: z[i] = 1/x[i]
 */
void N_VInv_Octave(N_Vector x, N_Vector z)
{
  ColumnVector *xv,*zv;
  xv = static_cast <ColumnVector *> NV_CONTENT_C(x);
  zv = static_cast <ColumnVector *> NV_CONTENT_C(z);

  (*zv) = ONE / (*xv);

  return;
}

/*
 * Add constant: z = x + b
 */
void N_VAddConst_Octave(N_Vector x, realtype b, N_Vector z)
{
  ColumnVector *xv,*zv;
  xv = static_cast <ColumnVector *> NV_CONTENT_C(x);
  zv = static_cast <ColumnVector *> NV_CONTENT_C(z);

  (*zv) = (*xv) + b;
  return;
}

/*
 * Scalar product of vectors x and y
 */
realtype N_VDotProd_Octave(N_Vector x, N_Vector y)
{
  ColumnVector *xv,*yv;
  realtype sum;
  xv = static_cast <ColumnVector *> NV_CONTENT_C(x);
  yv = static_cast <ColumnVector *> NV_CONTENT_C(y);
  sum = (*xv).transpose() * (*yv);
  return(sum);
}

/*
 * Max norm (L infinity) of vector x
 */
realtype N_VMaxNorm_Octave(N_Vector x)
{
  ColumnVector *xv, abret;
  realtype ret;
  xv = static_cast <ColumnVector *> NV_CONTENT_C(x);
  abret = static_cast <ColumnVector> (xv->abs()); 
  ColumnVector *abp = &(abret);
  ret = abp->max();
  
  return(ret);
}

/*
 * Weighted RMS norm
 */
realtype N_VWrmsNorm_Octave(N_Vector x, N_Vector w)
{
  octave_value_list ov = ovl((N_VWSqrSumLocal_Octave(x, w)/(NV_LENGTH_C(x))));
  return((octave::Fsqrt(ov,1)(0)).double_value());
}

/*
 * local weighted squared sum
 */
realtype N_VWSqrSumLocal_Octave(N_Vector x, N_Vector w)
{
  N_Vector prod = N_VClone_Octave(x);
  ColumnVector *xv,*yv, *pv;
  realtype sum;
  xv = static_cast <ColumnVector *> NV_CONTENT_C(x);
  yv = static_cast <ColumnVector *> NV_CONTENT_C(w);
  pv = static_cast <ColumnVector *> NV_CONTENT_C(prod);

  (*pv) = product((*xv),(*yv));
  octave_value_list ov = ovl((*pv));
  sum = (octave::Fsumsq(ov,1)(0)).double_value(); 
  return(sum);
}

/*
 * Masked weighted RMS norm
 */
realtype N_VWrmsNormMask_Octave(N_Vector x, N_Vector w, N_Vector id)
{
  octave_value_list ov = ovl((N_VWSqrSumMaskLocal_Octave(x, w, id)/(NV_LENGTH_C(x))));
  return((octave::Fsqrt(ov,1)(0)).double_value());
}

/*
 * local weighted masked squared sum
 */
realtype N_VWSqrSumMaskLocal_Octave(N_Vector x, N_Vector w, N_Vector id)
{
  N_Vector prod = N_VClone_Octave(x);
  N_Vector prod2 = N_VClone_Octave(x);
  ColumnVector *xv,*yv, *mv, *pv, *pv2;
  realtype sum;
  xv = static_cast <ColumnVector *> NV_CONTENT_C(x);
  yv = static_cast <ColumnVector *> NV_CONTENT_C(w);
  mv = static_cast <ColumnVector *> NV_CONTENT_C(id);    //mask vector
  pv = static_cast <ColumnVector *> NV_CONTENT_C(prod);
  pv2 = static_cast <ColumnVector *> NV_CONTENT_C(prod2);

  const octave_value_list ov = octave_value_list({(*mv),(*xv),(*mv)});
  const octave_value_list ov2 = octave_value_list({(*mv),(*yv),(*mv)});
  (*pv) = (octave::Fmerge(ov2,1)(0)).column_vector_value();
  (*pv2) = (octave::Fmerge(ov2,1)(0)).column_vector_value();
  sum = N_VWSqrSumLocal_Octave(prod, prod2);

  return(sum);
}

/* Minimum value in Vector */
realtype N_VMin_Octave(N_Vector x)
{
  ColumnVector *xv;
  realtype min;
  xv = static_cast <ColumnVector *> NV_CONTENT_C(x);

  min = xv->min();
  return(min);
}

/*
 * Elementwise z[i] = |x[i]| >= c ? 1 : 0
 */
void N_VCompare_Octave(realtype c, N_Vector x, N_Vector z)
{
  ColumnVector *xv,*zv;
  xv = static_cast <ColumnVector *> NV_CONTENT_C(x);
  zv = static_cast <ColumnVector *> NV_CONTENT_C(z);
  /*
  * FIXME : The comparisons here shouldn't be low-level,
  * but octave::Fge which should be used gives an interpreter error
  */
  for (int i = 0; i < (xv)->numel(); i++) {
      (*zv)(i) = ((xv->abs())(i) >= c) ? ONE : ZERO;
    }

  return;
}

/*
 * Elementwise inverse with zero checking: z[i] = 1/x[i], x[i] != 0
 */
booleantype N_VInvTest_Octave(N_Vector x, N_Vector z)
{
  N_Vector y = N_VClone_Octave(x);

  ColumnVector *xv, *zv, *yv;
  xv = static_cast <ColumnVector *> NV_CONTENT_C(x);
  zv = static_cast <ColumnVector *> NV_CONTENT_C(z);
  yv = static_cast <ColumnVector *> NV_CONTENT_C(y);
  booleantype no_zero_found = SUNTRUE ;
  (*zv) = ONE /(*xv);
  if(xv->nnz() != xv->numel())
    no_zero_found = SUNFALSE;
  if(no_zero_found==SUNTRUE)
    (*zv) = ONE /(*xv);
  else {
    (*yv) = (*zv);
    const octave_value_list ov = ovl((*zv));
    (*yv) = (octave::Fisfinite(ov,1)(0)).column_vector_value();
    const octave_value_list ov2 = octave_value_list({(*yv),(*zv),(*yv)});
    (*zv) = (octave::Fmerge(ov2,1)(0)).column_vector_value();
  }

  return no_zero_found;
}

/* FIXME : Same problem as Compare */
booleantype N_VConstrMask_Octave(N_Vector c, N_Vector x, N_Vector m)
{
  realtype temp;
  booleantype test;

  ColumnVector *cv, *xv, *mv;
  xv = static_cast <ColumnVector *> NV_CONTENT_C(x);
  cv = static_cast <ColumnVector *> NV_CONTENT_C(c);
  mv = static_cast <ColumnVector *> NV_CONTENT_C(m);

  for (sunindextype i = 0; i < NV_LENGTH_C(x); i++) {
    (*mv)(i) = ZERO;

    /* Continue if no constraints were set for the variable */
    if ((*cv)(i) == ZERO)
      continue;

    /* Check if a set constraint has been violated */
    test = ((cv->abs())(i) > ONEPT5 && (*xv)(i)*(*cv)(i) <= ZERO) ||
           ((cv->abs())(i)  > HALF   && (*xv)(i)*(*cv)(i) <  ZERO);
    if (test) {
      temp = (*mv)(i) = ONE;
    }
  }

  /* Return false if any constraint was violated */
  return (temp == ONE) ? SUNFALSE : SUNTRUE;
}

/*
 * Find minimum quotient: minq  = min ( num[i]/denom[i]), denom[i] != 0.
 */
realtype N_VMinQuotient_Octave(N_Vector num, N_Vector denom)
{
    N_Vector z = N_VClone_Octave(num);
    ColumnVector *xv, *zv, *yv;
    realtype min;
    min = BIG_REAL;

    xv = static_cast<ColumnVector *> NV_CONTENT_C(num);
    yv = static_cast<ColumnVector *> NV_CONTENT_C(denom);
    zv = static_cast<ColumnVector *> NV_CONTENT_C(z);
    *zv = quotient((*xv),(*yv));
    min = (zv)->min();

  return(min);
}

/*
 * -----------------------------------------------------------------
 * fused vector operations
 * -----------------------------------------------------------------
 */

int N_VLinearCombination_Octave(int nvec, realtype* c, N_Vector* X, N_Vector z)
{
  /* invalid number of vectors */
  if (nvec < 1) return(-1);

  /* should have called N_VScale */
  if (nvec == 1) {
    N_VScale_Octave(c[0], X[0], z);
    return(0);
  }

  /* should have called N_VLinearSum */
  if (nvec == 2) {
    N_VLinearSum_Octave(c[0], X[0], c[1], X[1], z);
    return(0);
  }

  ColumnVector *xv, *zv;

  xv = static_cast<ColumnVector *> NV_CONTENT_C(X[0]);
  zv = static_cast<ColumnVector *> NV_CONTENT_C(z);

  /*
   * z = sum{ c[i] * X[i] }, i = 0,...,nvec-1
   */

  (*zv) = (*c)*(*xv);
  return(0);
}


int N_VScaleAddMulti_Octave(int nvec, realtype* a, N_Vector x, N_Vector* Y, N_Vector* Z)
{
  /* invalid number of vectors */
  if (nvec < 1) return(-1);

  /* should have called N_VLinearSum */
  if (nvec == 1) {
    N_VLinearSum_Octave(a[0], x, ONE, Y[0], Z[0]);
    return(0);
  }

  ColumnVector *xv, *zv, *yv;

  yv = static_cast<ColumnVector *> NV_CONTENT_C(Y[0]);
  zv = static_cast<ColumnVector *> NV_CONTENT_C(Z[0]);
  xv = static_cast<ColumnVector *> NV_CONTENT_C(x);

  /*
   * Z[i][j] = Y[i][j] + a[i] * x[j]
   */
  (*zv) = (*yv) + (*a)*(*xv);
  return(0);
}

/*
 * -----------------------------------------------------------------
 * vector array operations
 * -----------------------------------------------------------------
 */

int N_VLinearSumVectorArray_Octave(int nvec,
                                   realtype a, N_Vector* X,
                                   realtype b, N_Vector* Y,
                                   N_Vector* Z)
{

  ColumnVector *xv, *zv, *yv;
  int          i;

  /* invalid number of vectors */
  if (nvec < 1) return(-1);

  /* should have called N_VLinearSum */
  if (nvec == 1) {
    N_VLinearSum_Octave(a, X[0], b, Y[0], Z[0]);
    return(0);
  }                   

  for (i=0; i<nvec; i++) { 
    yv = static_cast<ColumnVector *> NV_CONTENT_C(Y[i]);
    zv = static_cast<ColumnVector *> NV_CONTENT_C(Z[i]);
    xv = static_cast<ColumnVector *> NV_CONTENT_C(X[i]);
    (*zv) = a * (*xv) + b * (*yv);
  }

  return(0);
}


int N_VScaleVectorArray_Octave(int nvec, realtype* c, N_Vector* X, N_Vector* Z)
{
  sunindextype i,j;
  realtype    *xd=NULL;
  ColumnVector *xv, *zv;

  xv = static_cast<ColumnVector *> NV_CONTENT_C(X[0]);
  zv = static_cast<ColumnVector *> NV_CONTENT_C(Z[0]);

  /* invalid number of vectors */
  if (nvec < 1) return(-1);

  /* should have called N_VScale */
  if (nvec == 1) {
    N_VScale_Octave(c[0], X[0], Z[0]);
    return(0);
  }

  /*
   * X[i] *= c[i]
   */
  /* only checking if NVectors X and Z 
   are stored in the same address */
  if (xv == zv) { 
    // (*xv) *= (*c); //this should work but doesn't, in both N_VScale
    //                // instead results in convergence failures and wrong answer
    for (i=0; i<nvec; i++) {
      xd = NV_DATA_C(X[i]);
      for (j=0; j<(zv)->numel(); j++) {
        xd[j] *= c[i];
      }
    }
    return(0);
  }

  /*
   * Z[i] = c[i] * X[i]
   */
  (*zv) = (*c) * (*xv); 
  return(0);
}

/*
 * -----------------------------------------------------------------
 * private functions for special cases of vector operations
 * -----------------------------------------------------------------
 */


static void VCopy_Octave(N_Vector x, N_Vector z) 
// used in N_VScale and if just do (*zv)=(*xv) results in wrong answer.
{
  ColumnVector *xv, *zv;

  xv = static_cast<ColumnVector *> NV_CONTENT_C(x);
  zv = static_cast<ColumnVector *> NV_CONTENT_C(z);

  for (sunindextype i = 0; i < xv->numel(); i++)
    (*zv)(i) = (*xv)(i);

  return;
}

/*
 * -----------------------------------------------------------------
 * Enable / Disable fused and vector array operations
 * -----------------------------------------------------------------
 */

int N_VEnableFusedOps_Octave(N_Vector v, booleantype tf)
{
  /* check that vector is non-NULL */
  if (v == NULL) return(-1);

  /* check that ops structure is non-NULL */
  if (v->ops == NULL) return(-1);

   if (tf) {
    /* enable all fused vector operations */
    v->ops->nvlinearcombination = NULL;
    v->ops->nvscaleaddmulti     = N_VScaleAddMulti_Octave;
    v->ops->nvdotprodmulti      = NULL;
    /* enable all vector array operations */
    v->ops->nvlinearsumvectorarray         = N_VLinearSumVectorArray_Octave;
    v->ops->nvscalevectorarray             = N_VScaleVectorArray_Octave;
    v->ops->nvconstvectorarray             = NULL;
    v->ops->nvwrmsnormvectorarray          = NULL;
    v->ops->nvwrmsnormmaskvectorarray      = NULL;
    v->ops->nvscaleaddmultivectorarray     = NULL;
    v->ops->nvlinearcombinationvectorarray = NULL;
  } else {
    /* disable all fused vector operations */
    v->ops->nvlinearcombination = NULL;
    v->ops->nvscaleaddmulti     = NULL;
    v->ops->nvdotprodmulti      = NULL;
    /* disable all vector array operations */
    v->ops->nvlinearsumvectorarray         = NULL;
    v->ops->nvscalevectorarray             = NULL;
    v->ops->nvconstvectorarray             = NULL;
    v->ops->nvwrmsnormvectorarray          = NULL;
    v->ops->nvwrmsnormmaskvectorarray      = NULL;
    v->ops->nvscaleaddmultivectorarray     = NULL;
    v->ops->nvlinearcombinationvectorarray = NULL;
  }

  /* return success */
  return(0);
}


/*
* -----------------------------------------------------------------
* exported functions for octave implementation of SUNMATRIX_SPARSE
* -----------------------------------------------------------------
*/

/* Constructor function to create a new sparse matrix
  based on dimensions */

SUNMatrix OCTSparseMatrix(sunindextype M, sunindextype N,
                          sunindextype NNZ ARG_SUNCONTEXT)
{
  SUNMatrix A;
  void *content;

  /* return with NULL matrix on illegal input */
  if ((M <= 0) || (N <= 0) || (NNZ < 0))
    return (NULL);

  /* Create an empty matrix object */
  A = NULL;
  A = SUNMatNewEmpty(SP_OCTAVE_SUNCONTEXT);
  if (A == NULL)
    return (NULL);

  // /* Attach required operations */
  A->ops->getid = OCTMatGetID_Sparse;
  A->ops->clone = NULL;
  A->ops->destroy = OCTMatDestroy_Sparse;
  A->ops->zero = OCTMatZero_Sparse;
  A->ops->copy = NULL;
  A->ops->scaleadd = NULL;
  A->ops->scaleaddi = NULL;
  A->ops->matvec = NULL;
  A->ops->space = NULL;

  /* Create content */
  content = NULL;
  // creating a new SparseMatrix Octave data structure with rows and columns as specified
  content = new SparseMatrix(M, N, NNZ);
  if (content == NULL)
  {
    SUNMatDestroy(A);
    return (NULL);
  }

  /* Attach content */
  A->content = content;
  /* Fill content */
  return (A);
}

/* Functions to access the contents 
  of the sparse matrix structure */

sunindextype OCTSparseMatrix_Rows(SUNMatrix A)
{
  if (SUNMatGetID(A) == SUNMATRIX_CUSTOM)
    return SM_ROWS_O(A);
  else
    return SUNMAT_ILL_INPUT;
}

sunindextype OCTSparseMatrix_Columns(SUNMatrix A)
{
  if (SUNMatGetID(A) == SUNMATRIX_CUSTOM)
    return SM_COLUMNS_O(A);
  else
    return SUNMAT_ILL_INPUT;
}

sunindextype OCTSparseMatrix_NNZ(SUNMatrix A)
{
  if (SUNMatGetID(A) == SUNMATRIX_CUSTOM)
    return SM_NNZ_O(A);
  else
    return SUNMAT_ILL_INPUT;
}

sunindextype OCTSparseMatrix_NP(SUNMatrix A)
{
  if (SUNMatGetID(A) == SUNMATRIX_CUSTOM)
    return SM_NP_O(A);
  else
    return SUNMAT_ILL_INPUT;
}

int OCTSparseMatrix_SparseType(SUNMatrix A)
{
  if (SUNMatGetID(A) == SUNMATRIX_CUSTOM)
    return SM_SPARSETYPE_O(A);
  else
    return SUNMAT_ILL_INPUT;
}

realtype *OCTSparseMatrix_Data(SUNMatrix A)
{
  if (SUNMatGetID(A) == SUNMATRIX_CUSTOM)
    return SM_DATA_O(A);
  else
    return NULL;
}

sunindextype *OCTSparseMatrix_IndexValues(SUNMatrix A)
{
  if (SUNMatGetID(A) == SUNMATRIX_CUSTOM)
    return SM_INDEXVALS_O(A);
  else
    return NULL;
}

sunindextype *OCTSparseMatrix_IndexPointers(SUNMatrix A)
{
  if (SUNMatGetID(A) == SUNMATRIX_CUSTOM)
    return SM_INDEXPTRS_O(A);
  else
    return NULL;
}

/*
 * -----------------------------------------------------------------
 * implementation of matrix operations
 * -----------------------------------------------------------------
 */

SUNMatrix_ID OCTMatGetID_Sparse([[maybe_unused]] SUNMatrix A)
{
  return SUNMATRIX_CUSTOM;
}

void OCTMatDestroy_Sparse(SUNMatrix A)
{
  if (A == NULL)
    return;

  //   /* free content */
  if (A->content != NULL)
  { 
    /* deleting object created using new */
    delete SM_CONTENT_O(A);
  }
  A->content = NULL;

  /* free ops and matrix */
  if (A->ops)
  {
    free(A->ops);
    A->ops = NULL;
  }
  free(A);
  A = NULL;

  return;
}

int OCTMatZero_Sparse(SUNMatrix A)
{
  SparseMatrix *ptr = new SparseMatrix(SM_ROWS_O(A),SM_COLS_O(A));
  SparseMatrix *am = static_cast <SparseMatrix *> SM_CONTENT_O(A);
  *am = *ptr;

  return SUNMAT_SUCCESS;
}

/* Function to print the sparse matrix */

void OCTSparseMatrix_Print(SUNMatrix A)
{
  SparseMatrix *am = static_cast <SparseMatrix *>SM_CONTENT_O(A);
  std::cout<<(*am);
  return;
}


/* ----------------------------------------------------------------------------
 * Function to create a new General Octave linear solver
 */

SUNLinearSolver OCTLinSol_Gen(N_Vector y, SUNMatrix A ARG_SUNCONTEXT)
{
  SUNLinearSolver S;
  OCTLinearSolverContent_GEN content;

  if (OCTSparseMatrix_Rows(A) != OCTSparseMatrix_Columns(A)) return(NULL);

  if ( N_VGetVectorID(y) != SUNDIALS_NVEC_CUSTOM ) 
    return(NULL);

  if (OCTSparseMatrix_Rows(A) != N_VGetLength(y)) return(NULL);
 

  /* Create an empty linear solver */
  S = NULL;
  S = SUNLinSolNewEmpty(SP_OCTAVE_SUNCONTEXT);
  if (S == NULL) return(NULL);

  /* Attach operations */
  S->ops->gettype    = OCTLinSolGetType_Gen;
  S->ops->getid      = OCTLinSolGetID_Gen;
  S->ops->initialize = NULL;
  S->ops->setup      = NULL;
  S->ops->solve      = OCTLinSolSolve_Gen;
  S->ops->lastflag   = NULL;
  S->ops->space      = NULL;
  S->ops->free       = OCTLinSolFree_Gen;

    /* Create content */
  content = NULL;
  content = (OCTLinearSolverContent_GEN) malloc(sizeof *content);
  if (content == NULL) { SUNLinSolFree(S); return(NULL); }

  /* Attach content */
  S->content = content;

  /* Fill content */
  content->last_flag       = 0;
  content->first_factorize = 1;

  return(S);
}

/*
 * -----------------------------------------------------------------
 * implementation of linear solver operations
 * -----------------------------------------------------------------
 */

SUNLinearSolver_Type OCTLinSolGetType_Gen([[maybe_unused]] SUNLinearSolver S)
{
  return(SUNLINEARSOLVER_DIRECT);
}


SUNLinearSolver_ID OCTLinSolGetID_Gen([[maybe_unused]] SUNLinearSolver S)
{
  return(SUNLINEARSOLVER_CUSTOM);
}


int OCTLinSolSolve_Gen(SUNLinearSolver S, SUNMatrix A, N_Vector x,
                       N_Vector b, [[maybe_unused]] realtype tol)
{
  /* check for valid inputs */
  if ( (A == NULL) || (S == NULL) || (x == NULL) || (b == NULL) )
    return(SUNLS_MEM_NULL);
  ColumnVector *xv = static_cast <ColumnVector *> NV_CONTENT_C(x);
  ColumnVector *zv = static_cast <ColumnVector *> NV_CONTENT_C(b);

  SparseMatrix *am = static_cast<SparseMatrix *> SM_CONTENT_O(A);

  (*xv) = am->solve((*zv));

  return 1;
}

int OCTLinSolFree_Gen(SUNLinearSolver S)
{
  /* return with success if already freed */
  if (S == NULL) return(SUNLS_SUCCESS);

  /* delete generic structures */
  if (S->ops) {
    free(S->ops);
    S->ops = NULL;
  }
  free(S); S = NULL;
  return(SUNLS_SUCCESS);
}

#ifdef __cplusplus
} /* extern "C" */
#endif