#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun-dld.h"
#include "error.h"
#include "f77-fcn.h"
#include "oct-obj.h"
#include "procstream.h"

#undef F77_XFCN_ERROR
#if defined (F77_UPPERCASE_NAMES)
#define F77_XFCN_ERROR(f, F) \
  (*current_liboctave_error_handler) \
    ("exception encountered in Fortran subroutine %s", #F)
#else
#define F77_XFCN_ERROR(f, F) \
  (*current_liboctave_error_handler) \
    ("exception encountered in Fortran subroutine %s", #f)
#endif

extern "C"
{
  int F77_FCN (dgemv, DGEMV) (const char*, const int&, const int&,
			      const double&, const double*,
			      const int&, const double*, const int&,
			      const double&, double*, const int&,
			      long);
}

DEFUN_DLD_BUILTIN (bogus, , ,
  "bogus (): bogus function")
{
  octave_value_list retval;

  double *x;
  F77_XFCN (dgemv, DGEMV, ("x", 1, 2, 1.0, x, 5, x, 7, 8.0, x, 10, 1L));

  if (error_state)
    error ("error in bogus");

  iostream *s = new procstream ();
  s.tellg ();
  s.seekg (0, ios::beg);

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
