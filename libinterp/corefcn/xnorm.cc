////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2023 The Octave Project Developers
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

#include "oct-norm.h"

#include "errwarn.h"
#include "ov.h"
#include "xnorm.h"

OCTAVE_BEGIN_NAMESPACE(octave)

octave_value xnorm (const octave_value& x, const octave_value& p)
{
  octave_value retval;

  bool isvector = (x.columns () == 1 || x.rows () == 1);
  bool iscomplex = x.iscomplex ();
  bool issparse = x.issparse ();
  bool isfloat = x.is_single_type ();

  if (! isfloat && ! x.is_double_type ())
    err_wrong_type_arg ("xnorm", x);

  if (x.isempty ())
    retval = octave_value (0);
  else if (isvector)
    {
      if (isfloat & iscomplex)
        retval = octave::xnorm (x.float_complex_column_vector_value (),
                                p.float_value ());
      else if (isfloat)
        retval = octave::xnorm (x.float_column_vector_value (),
                                p.float_value ());
      else if (iscomplex)
        retval = octave::xnorm (x.complex_column_vector_value (),
                                p.double_value ());
      else
        retval = octave::xnorm (x.column_vector_value (),
                                p.double_value ());
    }
  else if (issparse)
    {
      if (iscomplex)
        retval = octave::xnorm (x.sparse_complex_matrix_value (),
                                p.double_value ());
      else
        retval = octave::xnorm (x.sparse_matrix_value (),
                                p.double_value ());
    }
  else
    {
      if (isfloat & iscomplex)
        retval = octave::xnorm (x.float_complex_matrix_value (),
                                p.float_value ());
      else if (isfloat)
        retval = octave::xnorm (x.float_matrix_value (),
                                p.float_value ());
      else if (iscomplex)
        retval = octave::xnorm (x.complex_matrix_value (),
                                p.double_value ());
      else
        retval = octave::xnorm (x.matrix_value (),
                                p.double_value ());
    }

  return retval;
}

octave_value xcolnorms (const octave_value& x, const octave_value& p)
{
  octave_value retval;

  bool iscomplex = x.iscomplex ();
  bool issparse = x.issparse ();
  bool isfloat = x.is_single_type ();

  if (! isfloat && ! x.is_double_type ())
    err_wrong_type_arg ("xcolnorms", x);

  if (issparse)
    {
      if (iscomplex)
        retval = octave::xcolnorms (x.sparse_complex_matrix_value (),
                                    p.double_value ());
      else
        retval = octave::xcolnorms (x.sparse_matrix_value (),
                                    p.double_value ());
    }
  else
    {
      if (isfloat & iscomplex)
        retval = octave::xcolnorms (x.float_complex_matrix_value (),
                                    p.float_value ());
      else if (isfloat)
        retval = octave::xcolnorms (x.float_matrix_value (),
                                    p.float_value ());
      else if (iscomplex)
        retval = octave::xcolnorms (x.complex_matrix_value (),
                                    p.double_value ());
      else
        retval = octave::xcolnorms (x.matrix_value (),
                                    p.double_value ());
    }

  return retval;
}

octave_value xrownorms (const octave_value& x, const octave_value& p)
{
  octave_value retval;

  bool iscomplex = x.iscomplex ();
  bool issparse = x.issparse ();
  bool isfloat = x.is_single_type ();

  if (! isfloat && ! x.is_double_type ())
    err_wrong_type_arg ("xrownorms", x);

  if (issparse)
    {
      if (iscomplex)
        retval = octave::xrownorms (x.sparse_complex_matrix_value (),
                                    p.double_value ());
      else
        retval = octave::xrownorms (x.sparse_matrix_value (),
                                    p.double_value ());
    }
  else
    {
      if (isfloat & iscomplex)
        retval = octave::xrownorms (x.float_complex_matrix_value (),
                                    p.float_value ());
      else if (isfloat)
        retval = octave::xrownorms (x.float_matrix_value (),
                                    p.float_value ());
      else if (iscomplex)
        retval = octave::xrownorms (x.complex_matrix_value (),
                                    p.double_value ());
      else
        retval = octave::xrownorms (x.matrix_value (),
                                    p.double_value ());
    }

  return retval;
}

octave_value xfrobnorm (const octave_value& x)
{
  octave_value retval;

  bool iscomplex = x.iscomplex ();
  bool issparse = x.issparse ();
  bool isfloat = x.is_single_type ();

  if (! isfloat && ! x.is_double_type ())
    err_wrong_type_arg ("xfrobnorm", x);

  if (issparse)
    {
      if (iscomplex)
        retval = octave::xfrobnorm (x.sparse_complex_matrix_value ());
      else
        retval = octave::xfrobnorm (x.sparse_matrix_value ());
    }
  else
    {
      if (isfloat & iscomplex)
        retval = octave::xfrobnorm (x.float_complex_matrix_value ());
      else if (isfloat)
        retval = octave::xfrobnorm (x.float_matrix_value ());
      else if (iscomplex)
        retval = octave::xfrobnorm (x.complex_matrix_value ());
      else
        retval = octave::xfrobnorm (x.matrix_value ());
    }

  return retval;
}

OCTAVE_END_NAMESPACE(octave)
