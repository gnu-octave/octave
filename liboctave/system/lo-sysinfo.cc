////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2018-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <sstream>
#include <string>

#include "f77-fcn.h"
#include "lo-sysinfo.h"
#include "oct-shlib.h"

// Hack to stringize macro results.
#define xSTRINGIZE(x) #x
#define STRINGIZE(x) xSTRINGIZE(x)

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(sys)

std::string blas_version (void)
{
  dynamic_library dyn_libs ("");

  if (! dyn_libs)
    return "unknown BLAS";

  std::string retval;

  // Check for functions that are specific to certain BLAS implementations.

  // FlexiBLAS
  typedef void (*flexi_f_type) (int *, int *, int *);
  flexi_f_type flexi_f_ptr = reinterpret_cast<flexi_f_type>
                             (dyn_libs.search ("flexiblas_get_version"));

  if (flexi_f_ptr)
    {
      int v_major = 0;
      int v_minor = 0;
      int v_patch = 0;
      flexi_f_ptr (&v_major, &v_minor, &v_patch);

      std::ostringstream s;
      s << "FlexiBLAS Version "
        << v_major << "." << v_minor << "." << v_patch;

      retval = s.str ();
    }

  // OpenBLAS
  typedef char *(*open_fcn_type) (void);
  open_fcn_type open_f_ptr = reinterpret_cast<open_fcn_type>
                             (dyn_libs.search ("openblas_get_config"));

  if (open_f_ptr)
    {
      if (! retval.empty ())
        retval += "\n";

      retval += "OpenBLAS (config: " + std::string (open_f_ptr ()) + ")";
    }

  // OpenBLAS with minimal extension functions included in the library
  else if (dyn_libs.search ("openblas_get_num_threads"))
    {
      if (! retval.empty ())
        retval += "\n";

      retval += "OpenBLAS (config: unknown)";
    }

  // GotoBLAS(2)
  if (dyn_libs.search ("gotoblas_profile_init"))
    {
      if (! retval.empty ())
        retval += "\n";

      retval += "GotoBLAS(2)";
    }

  // ATLAS
  // FIXME: If we are really interested, we could use a pipe to
  // redirect the output of "ATL_buildinfo".
  if (dyn_libs.search ("ATL_buildinfo"))
    {
      if (! retval.empty ())
        retval += "\n";

      retval += "ATLAS";
    }

  // ACML
  typedef void (*acml_f_type) (int *, int *, int *);
  acml_f_type acml_f_ptr = reinterpret_cast<acml_f_type>
                           (dyn_libs.search ("acmlversion"));

  if (acml_f_ptr)
    {
      int v_major = 0;
      int v_minor = 0;
      int v_patch = 0;
      acml_f_ptr (&v_major, &v_minor, &v_patch);

      std::ostringstream s;
      s << "ACML BLAS Version "
        << v_major << "." << v_minor << "." << v_patch;

      if (! retval.empty ())
        retval += "\n";

      retval += s.str ();
    }

  // Intel MKL
  typedef void (*mkl_f_type) (char *, int);
  mkl_f_type mkl_f_ptr = reinterpret_cast<mkl_f_type>
                         (dyn_libs.search ("mkl_get_version_string"));

  if (mkl_f_ptr)
    {
      char buf[198];
      int len = 198;
      mkl_f_ptr (buf, len);

      if (! retval.empty ())
        retval += "\n";

      retval += std::string (buf);
    }

  // Otherwise
  if (retval.empty ())
    retval = "unknown or reference BLAS";

  return retval;
}

std::string lapack_version (void)
{
  std::string retval = "unknown LAPACK";

  dynamic_library dyn_libs ("");

  if (! dyn_libs)
    return retval;

  // query LAPACK version
  typedef F77_RET_T
    (*ilaver_fcn_type) (const F77_INT&, const F77_INT&, const F77_INT&);
  ilaver_fcn_type f_ptr = reinterpret_cast<ilaver_fcn_type>
                          (dyn_libs.search (STRINGIZE (F77_FUNC (ilaver, ILAVER))));

  if (f_ptr)
    {
      int v_major = 0;
      int v_minor = 0;
      int v_patch = 0;
      f_ptr (v_major, v_minor, v_patch);

      std::ostringstream s;
      s << "Linear Algebra PACKage Version "
        << v_major << "." << v_minor << "." << v_patch;

      retval = s.str ();
    }

  return retval;
}

OCTAVE_END_NAMESPACE(sys)
OCTAVE_END_NAMESPACE(octave)
