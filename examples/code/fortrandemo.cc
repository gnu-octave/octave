#include <octave/oct.h>
#include <octave/f77-fcn.h>

extern "C"
{
  F77_RET_T
  F77_FUNC (fortransub, FORTSUB)
    (const F77_INT&, F77_DBLE*, F77_CHAR_ARG_DECL F77_CHAR_ARG_LEN_DECL);
}

DEFUN_DLD (fortrandemo, args, , "Fortran Demo")
{
  if (args.length () != 1)
    print_usage ();

  NDArray a = args(0).array_value ();

  double *av = a.fortran_vec ();
  octave_idx_type na = a.numel ();

  OCTAVE_LOCAL_BUFFER (char, ctmp, 128);

  F77_XFCN (fortransub, FORTSUB,
            (na, av, ctmp F77_CHAR_ARG_LEN (128)));

  return ovl (a, std::string (ctmp));
}
