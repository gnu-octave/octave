// Nothing like a little CPP abuse to brighten everyone's day.  Would
// have been nice to do this with template functions but as of 2.5.x,
// g++ seems to fail to resolve them properly.

#define DO_VS_OP(r, l, v, OP, s) \
  if (l > 0) \
    { \
      for (int i = 0; i < l; i++) \
	r[i] = v[i] OP s; \
    }

#define DO_SV_OP(r, l, s, OP, v) \
  if (l > 0) \
    { \
      for (int i = 0; i < l; i++) \
	r[i] = s OP v[i]; \
    }

#define DO_VV_OP(r, l, x, OP, y) \
  if (l > 0) \
    { \
      for (int i = 0; i < l; i++) \
	r[i] = x[i] OP y[i]; \
    }

#define NEG_V(r, l, x) \
  if (l > 0) \
    { \
      for (int i = 0; i < l; i++) \
	r[i] = -x[i]; \
    }

#define DO_VS_OP2(OP) \
  int l = a.length (); \
  if (l > 0) \
    { \
      T *tmp = a.fortran_vec (); \
      for (int i = 0; i < l; i++) \
	tmp[i] OP s; \
    }

#define DO_VV_OP2(OP) \
  do \
    { \
      T *a_tmp = a.fortran_vec (); \
      const T *b_tmp = b.data (); \
      for (int i = 0; i < l; i++) \
	a_tmp[i] OP b_tmp[i]; \
    } \
  while (0)

