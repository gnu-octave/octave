#if !defined (octave_MArray_defs_h)
#define octave_MArray_defs_h 1

// Nothing like a little CPP abuse to brighten everyone's day.

#define DO_VS_OP(r, l, v, OP, s) \
  if (l > 0) \
    { \
      for (octave_idx_type i = 0; i < l; i++) \
	r[i] = v[i] OP s; \
    }

#define DO_SV_OP(r, l, s, OP, v) \
  if (l > 0) \
    { \
      for (octave_idx_type i = 0; i < l; i++) \
	r[i] = s OP v[i]; \
    }

#define DO_VV_OP(r, l, x, OP, y) \
  if (l > 0) \
    { \
      for (octave_idx_type i = 0; i < l; i++) \
	r[i] = x[i] OP y[i]; \
    }

#define NEG_V(r, l, x) \
  if (l > 0) \
    { \
      for (octave_idx_type i = 0; i < l; i++) \
	r[i] = -x[i]; \
    }

#define DO_VS_OP2(T, a, OP, s) \
  octave_idx_type l = a.length (); \
  if (l > 0) \
    { \
      T *tmp = a.fortran_vec (); \
      for (octave_idx_type i = 0; i < l; i++) \
	tmp[i] OP s; \
    }

#define DO_VV_OP2(T, a, OP, b) \
  do \
    { \
      T *a_tmp = a.fortran_vec (); \
      const T *b_tmp = b.data (); \
      for (octave_idx_type i = 0; i < l; i++) \
	a_tmp[i] OP b_tmp[i]; \
    } \
  while (0)

// A macro that can be used to declare and instantiate OP= operators.
#define MARRAY_OP_ASSIGN_DECL(A_T, E_T, OP, PFX, LTGT, RHS_T) \
  PFX A_T<E_T>& \
  operator OP LTGT (A_T<E_T>&, const RHS_T&)

// All the OP= operators that we care about.
#define MARRAY_OP_ASSIGN_DECLS(A_T, E_T, PFX, LTGT, RHS_T) \
  MARRAY_OP_ASSIGN_DECL (A_T, E_T, +=, PFX, LTGT, RHS_T); \
  MARRAY_OP_ASSIGN_DECL (A_T, E_T, -=, PFX, LTGT, RHS_T);

// Generate forward declarations for OP= operators.
#define MARRAY_OP_ASSIGN_FWD_DECLS(A_T, RHS_T) \
  MARRAY_OP_ASSIGN_DECLS (A_T, T, template <typename T> OCTAVE_API, , RHS_T)

// Generate friend declarations for the OP= operators.
#define MARRAY_OP_ASSIGN_FRIENDS(A_T, RHS_T) \
  MARRAY_OP_ASSIGN_DECLS (A_T, T, friend, <>, RHS_T)

// Instantiate the OP= operators.
#define MARRAY_OP_ASSIGN_DEFS(A_T, E_T, RHS_T) \
  MARRAY_OP_ASSIGN_DECLS (A_T, E_T, template OCTAVE_API, , RHS_T)

// A function that can be used to forward OP= operations from derived
// classes back to us.
#define MARRAY_OP_ASSIGN_FWD_FCN(R, F, T, C_X, X_T, C_Y, Y_T) \
  inline R \
  F (X_T& x, const Y_T& y) \
  { \
    return R (F (C_X (x), C_Y (y))); \
  }

// All the OP= operators that we care about forwarding.
#define MARRAY_OP_ASSIGN_FWD_DEFS(R, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_OP_ASSIGN_FWD_FCN (R, operator +=, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_OP_ASSIGN_FWD_FCN (R, operator -=, T, C_X, X_T, C_Y, Y_T)

// A macro that can be used to declare and instantiate unary operators.
#define MARRAY_UNOP(A_T, E_T, F, PFX, LTGT) \
  PFX A_T<E_T> \
  F LTGT (const A_T<E_T>&)

// All the unary operators that we care about.
#define MARRAY_UNOP_DECLS(A_T, E_T, PFX, LTGT) \
  MARRAY_UNOP (A_T, E_T, operator +, PFX, LTGT); \
  MARRAY_UNOP (A_T, E_T, operator -, PFX, LTGT);

// Generate forward declarations for unary operators.
#define MARRAY_UNOP_FWD_DECLS(A_T) \
  MARRAY_UNOP_DECLS (A_T, T, template <typename T> OCTAVE_API, )

// Generate friend declarations for the unary operators.
#define MARRAY_UNOP_FRIENDS(A_T) \
  MARRAY_UNOP_DECLS (A_T, T, friend, <>)

// Instantiate the unary operators.
#define MARRAY_UNOP_DEFS(A_T, E_T) \
  MARRAY_UNOP_DECLS (A_T, E_T, template OCTAVE_API, )

// A function that can be used to forward unary operations from derived
// classes back to us.
#define MARRAY_UNOP_FWD_FCN(R, F, T, C_X, X_T) \
  inline R \
  F (const X_T& x) \
  { \
    return R (F (C_X (x))); \
  }

// All the unary operators that we care about forwarding.
#define MARRAY_UNOP_FWD_DEFS(R, T, C_X, X_T) \
  MARRAY_UNOP_FWD_FCN (R, operator +, T, C_X, X_T) \
  MARRAY_UNOP_FWD_FCN (R, operator -, T, C_X, X_T)

// A macro that can be used to declare and instantiate binary operators.
#define MARRAY_BINOP_DECL(A_T, E_T, F, PFX, LTGT, X_T, Y_T) \
  PFX A_T<E_T> \
  F LTGT (const X_T&, const Y_T&)

// All the binary operators that we care about.  We have two
// sets of macros since the MArray OP MArray operations use functions
// (product and quotient) instead of operators (*, /).
#define MARRAY_BINOP_DECLS(A_T, E_T, PFX, LTGT, X_T, Y_T) \
  MARRAY_BINOP_DECL (A_T, E_T, operator +, PFX, LTGT, X_T, Y_T); \
  MARRAY_BINOP_DECL (A_T, E_T, operator -, PFX, LTGT, X_T, Y_T); \
  MARRAY_BINOP_DECL (A_T, E_T, operator *, PFX, LTGT, X_T, Y_T); \
  MARRAY_BINOP_DECL (A_T, E_T, operator /, PFX, LTGT, X_T, Y_T);

#define MARRAY_AA_BINOP_DECLS(A_T, E_T, PFX, LTGT) \
  MARRAY_BINOP_DECL (A_T, E_T, operator +, PFX, LTGT, A_T<E_T>, A_T<E_T>); \
  MARRAY_BINOP_DECL (A_T, E_T, operator -, PFX, LTGT, A_T<E_T>, A_T<E_T>); \
  MARRAY_BINOP_DECL (A_T, E_T, quotient,   PFX, LTGT, A_T<E_T>, A_T<E_T>); \
  MARRAY_BINOP_DECL (A_T, E_T, product,    PFX, LTGT, A_T<E_T>, A_T<E_T>);

#define MDIAGARRAY2_DAS_BINOP_DECLS(A_T, E_T, PFX, LTGT, X_T, Y_T) \
  MARRAY_BINOP_DECL (A_T, E_T, operator *, PFX, LTGT, X_T, Y_T); \
  MARRAY_BINOP_DECL (A_T, E_T, operator /, PFX, LTGT, X_T, Y_T);

#define MDIAGARRAY2_SDA_BINOP_DECLS(A_T, E_T, PFX, LTGT, X_T, Y_T) \
  MARRAY_BINOP_DECL (A_T, E_T, operator *, PFX, LTGT, X_T, Y_T);

#define MDIAGARRAY2_DADA_BINOP_DECLS(A_T, E_T, PFX, LTGT) \
  MARRAY_BINOP_DECL (A_T, E_T, operator +, PFX, LTGT, A_T<E_T>, A_T<E_T>); \
  MARRAY_BINOP_DECL (A_T, E_T, operator -, PFX, LTGT, A_T<E_T>, A_T<E_T>); \
  MARRAY_BINOP_DECL (A_T, E_T, product,    PFX, LTGT, A_T<E_T>, A_T<E_T>);

// Generate forward declarations for binary operators.
#define MARRAY_BINOP_FWD_DECLS(A_T) \
  MARRAY_BINOP_DECLS (A_T, T, template <typename T> OCTAVE_API, , A_T<T>, T) \
  MARRAY_BINOP_DECLS (A_T, T, template <typename T> OCTAVE_API, , T, A_T<T>) \
  MARRAY_AA_BINOP_DECLS (A_T, T, template <typename T> OCTAVE_API, )

#define MDIAGARRAY2_BINOP_FWD_DECLS(A_T) \
  MDIAGARRAY2_DAS_BINOP_DECLS (A_T, T, template <typename T>, , A_T<T>, T) \
  MDIAGARRAY2_SDA_BINOP_DECLS (A_T, T, template <typename T>, , T, A_T<T>) \
  MDIAGARRAY2_DADA_BINOP_DECLS (A_T, T, template <typename T>, )

// Generate friend declarations for the binary operators.
#define MARRAY_BINOP_FRIENDS(A_T) \
  MARRAY_BINOP_DECLS (A_T, T, friend, <>, A_T<T>, T) \
  MARRAY_BINOP_DECLS (A_T, T, friend, <>, T, A_T<T>) \
  MARRAY_AA_BINOP_DECLS (A_T, T, friend, <>)

#define MDIAGARRAY2_BINOP_FRIENDS(A_T) \
  MDIAGARRAY2_DAS_BINOP_DECLS (A_T, T, friend, <>, A_T<T>, T) \
  MDIAGARRAY2_SDA_BINOP_DECLS (A_T, T, friend, <>, T, A_T<T>) \
  MDIAGARRAY2_DADA_BINOP_DECLS (A_T, T, friend, <>)

// Instantiate the binary operators.
#define MARRAY_BINOP_DEFS(A_T, E_T) \
  MARRAY_BINOP_DECLS (A_T, E_T, template OCTAVE_API, , A_T<E_T>, E_T) \
  MARRAY_BINOP_DECLS (A_T, E_T, template OCTAVE_API, , E_T, A_T<E_T>) \
  MARRAY_AA_BINOP_DECLS (A_T, E_T, template OCTAVE_API, )

#define MDIAGARRAY2_BINOP_DEFS(A_T, E_T) \
  MDIAGARRAY2_DAS_BINOP_DECLS (A_T, E_T, template OCTAVE_API, , A_T<E_T>, E_T) \
  MDIAGARRAY2_SDA_BINOP_DECLS (A_T, E_T, template OCTAVE_API, , E_T, A_T<E_T>) \
  MDIAGARRAY2_DADA_BINOP_DECLS (A_T, E_T, template OCTAVE_API, )

// A function that can be used to forward binary operations from derived
// classes back to us.
#define MARRAY_BINOP_FWD_FCN(R, F, T, C_X, X_T, C_Y, Y_T) \
  inline R \
  F (const X_T& x, const Y_T& y) \
  { \
    return R (F (C_X (x), C_Y (y))); \
  }

// The binary operators that we care about forwarding.  We have two
// sets of macros since the MArray OP MArray operations use functions
// (product and quotient) instead of operators (*, /).
#define MARRAY_BINOP_FWD_DEFS(R, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator +, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator -, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator *, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator /, T, C_X, X_T, C_Y, Y_T)

#define MARRAY_AA_BINOP_FWD_DEFS(R, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator +, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator -, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, product,    T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, quotient,   T, C_X, X_T, C_Y, Y_T)

#define MDIAGARRAY2_DAS_BINOP_FWD_DEFS(R, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator *, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator /, T, C_X, X_T, C_Y, Y_T)

#define MDIAGARRAY2_SDA_BINOP_FWD_DEFS(R, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator *, T, C_X, X_T, C_Y, Y_T)

#define MDIAGARRAY2_DADA_BINOP_FWD_DEFS(R, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator +, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator -, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, product,    T, C_X, X_T, C_Y, Y_T)

// Forward declarations for the MArray operators.
#define MARRAY_OPS_FORWARD_DECLS(A_T) \
  template <class T> \
  class A_T; \
 \
  MARRAY_OP_ASSIGN_FWD_DECLS (A_T, T) \
  MARRAY_OP_ASSIGN_FWD_DECLS (A_T, A_T<T>) \
  MARRAY_UNOP_FWD_DECLS (A_T) \
  MARRAY_BINOP_FWD_DECLS (A_T)

#define MDIAGARRAY2_OPS_FORWARD_DECLS(A_T) \
  template <class T> \
  class A_T; \
 \
  MARRAY_OP_ASSIGN_FWD_DECLS (A_T, A_T<T>) \
  MARRAY_UNOP_FWD_DECLS (A_T) \
  MDIAGARRAY2_BINOP_FWD_DECLS (A_T)

// Friend declarations for the MArray operators.
#define MARRAY_OPS_FRIEND_DECLS(A_T) \
  MARRAY_OP_ASSIGN_FRIENDS (A_T, T) \
  MARRAY_OP_ASSIGN_FRIENDS (A_T, A_T<T>) \
  MARRAY_UNOP_FRIENDS (A_T) \
  MARRAY_BINOP_FRIENDS (A_T)

#define MDIAGARRAY2_OPS_FRIEND_DECLS(A_T) \
  MARRAY_OP_ASSIGN_FRIENDS (A_T, A_T<T>) \
  MARRAY_UNOP_FRIENDS (A_T) \
  MDIAGARRAY2_BINOP_FRIENDS (A_T)

// The following macros are for external use.

// Instantiate all the MArray friends for MArray element type T.
#define INSTANTIATE_MARRAY_FRIENDS(T) \
  MARRAY_OP_ASSIGN_DEFS (MArray, T, T) \
  MARRAY_OP_ASSIGN_DEFS (MArray, T, MArray<T>) \
  MARRAY_UNOP_DEFS (MArray, T) \
  MARRAY_BINOP_DEFS (MArray, T)

// Instantiate all the MArray2 friends for MArray2 element type T.
#define INSTANTIATE_MARRAY2_FRIENDS(T) \
  MARRAY_OP_ASSIGN_DEFS (MArray2, T, T) \
  MARRAY_OP_ASSIGN_DEFS (MArray2, T, MArray2<T>) \
  MARRAY_UNOP_DEFS (MArray2, T) \
  MARRAY_BINOP_DEFS (MArray2, T)

// Instantiate all the MArrayN friends for MArrayN element type T.
#define INSTANTIATE_MARRAYN_FRIENDS(T) \
  MARRAY_OP_ASSIGN_DEFS (MArrayN, T, T) \
  MARRAY_OP_ASSIGN_DEFS (MArrayN, T, MArrayN<T>) \
  MARRAY_UNOP_DEFS (MArrayN, T) \
  MARRAY_BINOP_DEFS (MArrayN, T)

// Instantiate all the MDiagArray2 friends for MDiagArray2 element type T.
#define INSTANTIATE_MDIAGARRAY2_FRIENDS(T) \
  MARRAY_OP_ASSIGN_DEFS (MDiagArray2, T, MDiagArray2<T>) \
  MARRAY_UNOP_DEFS (MDiagArray2, T) \
  MDIAGARRAY2_BINOP_DEFS (MDiagArray2, T)

// Define all the MArray forwarding functions for return type R and
// MArray element type T
#define MARRAY_FORWARD_DEFS(B, R, T) \
  MARRAY_OP_ASSIGN_FWD_DEFS \
    (R, T, dynamic_cast<B<T>&>, R, , T) \
 \
  MARRAY_OP_ASSIGN_FWD_DEFS \
    (R, T, \
     dynamic_cast<B<T>&>, R, dynamic_cast<const B<T>&>, R) \
 \
  MARRAY_UNOP_FWD_DEFS \
    (R, T, dynamic_cast<const B<T>&>, R) \
 \
  MARRAY_BINOP_FWD_DEFS \
    (R, T, dynamic_cast<const B<T>&>, R, , T) \
 \
  MARRAY_BINOP_FWD_DEFS \
    (R, T, , T, dynamic_cast<const B<T>&>, R) \
 \
  MARRAY_AA_BINOP_FWD_DEFS \
    (R, T, dynamic_cast<const B<T>&>, R, dynamic_cast<const B<T>&>, R)

#define MDIAGARRAY2_FORWARD_DEFS(B, R, T) \
  MARRAY_OP_ASSIGN_FWD_DEFS \
    (R, T, \
     dynamic_cast<B<T>&>, R, dynamic_cast<const B<T>&>, R) \
 \
  MARRAY_UNOP_FWD_DEFS \
    (R, T, dynamic_cast<const B<T>&>, R) \
 \
  MDIAGARRAY2_DAS_BINOP_FWD_DEFS \
    (R, T, dynamic_cast<const B<T>&>, R, , T) \
 \
  MDIAGARRAY2_SDA_BINOP_FWD_DEFS \
    (R, T, , T, dynamic_cast<const B<T>&>, R) \
 \
  MDIAGARRAY2_DADA_BINOP_FWD_DEFS \
    (R, T, dynamic_cast<const B<T>&>, R, dynamic_cast<const B<T>&>, R)

#define MARRAY_NORM_BODY(TYPE, blas_norm, BLAS_NORM)	\
 \
  double retval = octave_NaN; \
 \
  octave_idx_type len = length (); \
 \
  if (len > 0) \
    { \
      const TYPE *d = data (); \
 \
      if (p == -1) \
	{ \
	  /* Frobenius norm.  */ \
	  retval = 0; \
 \
	  for (octave_idx_type i = 0; i < len; i++) \
	    { \
	      double d_abs = std::abs (d[i]); \
	      retval += d_abs * d_abs; \
	    } \
 \
	  retval = ::sqrt (retval); \
	} \
      else if (p == 2) \
	F77_FCN (blas_norm, BLAS_NORM) (len, d, 1, retval); \
      else if (xisinf (p)) \
	{ \
	  octave_idx_type i = 0; \
 \
	  while (i < len && xisnan (d[i])) \
	    i++; \
 \
	  if (i < len) \
	    retval = std::abs (d[i]); \
 \
	  if (p > 0) \
	    { \
	      while (i < len) \
		{ \
		  double d_abs = std::abs (d[i++]); \
 \
		  if (d_abs > retval) \
		    retval = d_abs; \
		} \
	    } \
	  else \
	    { \
	      while (i < len) \
		{ \
		  double d_abs = std::abs (d[i++]); \
 \
		  if (d_abs < retval) \
		    retval = d_abs; \
		} \
	    } \
	} \
      else \
	{ \
	  retval = 0; \
 \
	  for (octave_idx_type i = 0; i < len; i++) \
	    { \
	      double d_abs = std::abs (d[i]); \
	      retval += pow (d_abs, p); \
	    } \
 \
	  retval = pow (retval, 1/p); \
	} \
    } \
 \
  return retval

// Now we have all the definitions we need.

#endif
