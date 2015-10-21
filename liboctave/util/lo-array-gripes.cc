/*

Copyright (C) 2003-2015 John W. Eaton
Copyright (C) 2009 VZLU Prague

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include "lo-array-gripes.h"
#include "lo-error.h"

const char *error_id_nonconformant_args = "Octave:nonconformant-args";

const char *error_id_index_out_of_bounds = "Octave:index-out-of-bounds";

const char *error_id_invalid_index = "Octave:invalid-index";

const char *warning_id_nearly_singular_matrix = "Octave:nearly-singular-matrix";

const char *warning_id_singular_matrix = "Octave:singular-matrix";

void
gripe_nan_to_logical_conversion (void)
{
  (*current_liboctave_error_handler)
    ("invalid conversion from NaN to logical");
}

void
gripe_nan_to_character_conversion (void)
{
  (*current_liboctave_error_handler)
    ("invalid conversion from NaN to character");
}

void
gripe_nonconformant (const char *op, octave_idx_type op1_len,
                     octave_idx_type op2_len)
{
  const char *err_id = error_id_nonconformant_args;

  (*current_liboctave_error_with_id_handler)
    (err_id, "%s: nonconformant arguments (op1 len: %d, op2 len: %d)",
     op, op1_len, op2_len);
}

void
gripe_nonconformant (const char *op,
                     octave_idx_type op1_nr, octave_idx_type op1_nc,
                     octave_idx_type op2_nr, octave_idx_type op2_nc)
{
  const char *err_id = error_id_nonconformant_args;

  (*current_liboctave_error_with_id_handler)
    (err_id, "%s: nonconformant arguments (op1 is %dx%d, op2 is %dx%d)",
     op, op1_nr, op1_nc, op2_nr, op2_nc);
}

void
gripe_nonconformant (const char *op, const dim_vector& op1_dims,
                     const dim_vector& op2_dims)
{
  const char *err_id = error_id_nonconformant_args;

  std::string op1_dims_str = op1_dims.str ();
  std::string op2_dims_str = op2_dims.str ();

  (*current_liboctave_error_with_id_handler)
    (err_id, "%s: nonconformant arguments (op1 is %s, op2 is %s)",
     op, op1_dims_str.c_str (), op2_dims_str.c_str ());
}

void
gripe_del_index_out_of_range (bool is1d, octave_idx_type idx,
                              octave_idx_type ext)
{
  const char *err_id = error_id_index_out_of_bounds;

  (*current_liboctave_error_with_id_handler)
    (err_id, "A(%s) = []: index out of bounds; value %d out of bound %d",
     is1d ? "I" : "..,I,..", idx, ext);
}



// Common procedures of base class index_exception, thrown whenever an
// object is indexed incorrectly, such as by an index that is out of
// range, negative, fractional, complex, or of a non-numeric type.

const char *
index_exception::err (void) throw ()
{
  msg = access () + "; " + explain ();
  return msg.c_str ();
}

// Show what was illegally accessed, e.g.,  "A(-1,_)", "A(0+1i)", "A(_,3)"
// Show how many indices come before/after the offending one,
// e.g., (<error>), (<error>,_), or (_,<error>,...[x5]...)

std::string
index_exception:: access (void) const
{
  // FIXME: don't use a fixed size buffer!
  const int buf_len = 300;

  char output [buf_len];
  char pre [buf_len];
  char post [buf_len];

  // dim == 0 if position not yet given, or
  // <static_cast unsigned int>(-1) if explicitly shown to be unknown
  // both are caught by this condition

  if (static_cast <unsigned int> (dim-1) > 100000)
    {
      // No parentheses are given if the dimension is not known.
      pre[0] = post[0] = '\0';
    }
  else
    {
      if (dim < 5)
        {
          pre[0] = '(';
          octave_idx_type i;

          for (i = 1; i < dim; i++)
            {
              pre[2*i-1] = '_';
              pre[2*i]   = ',';
            }

          pre[2*i-1] = '\0';    // i == min (1, dim)
        }
      else
        {
          sprintf (pre, "(...[x%d]...", dim-1);
        }

      if (static_cast <unsigned int> (nd-dim) < 5)
        {
          for (octave_idx_type i = 0; i < nd-dim; i++)
            {
              post[2*i]   = ',';
              post[2*i+1] = '_';
            }

          if (nd >= dim)
            {
              post[2*(nd-dim)] = ')';
              post[2*(nd-dim)+1] = '\0';
            }
        }
      else
        sprintf (post, "...[x%d]...)", nd-dim);
    }

  const char *v;

  if (var[0] == '\0' || var == "<unknown>")
    v = "index ";
  else
    v = var.c_str ();

  snprintf (output, buf_len, "%s%s%s%s", v, pre, idx(), post);

  return output;
}

class invalid_index : public index_exception
{
public:

  invalid_index (const char *value, octave_idx_type ndim,
                 octave_idx_type dimen)
    : index_exception (value, ndim, dimen)
  { }

  const char* explain (void) const
  {
#ifdef USE_64_BIT_IDX_T
    return "subscripts must be either integers 1 to (2^63)-1 or logicals";
#else
    return "subscripts must be either integers 1 to (2^31)-1 or logicals";
#endif
  }

  // ID of error to throw
  const char* id (void) const
  {
    return error_id_invalid_index;
  }
};

// Complain of an index that is: negative, fractional, or too big.

void
gripe_invalid_index (const char *idx, octave_idx_type nd,
                     octave_idx_type dim, const char * /* var */)
{
    invalid_index e (idx, nd, dim);

    throw e;
}

void
gripe_invalid_index (octave_idx_type n, octave_idx_type nd,
                     octave_idx_type dim, const char *var)
{
  // Note: log10 (2^63) = 19 digits.  Use 64 for ease of memory alignment. 
  char buf[64];

  sprintf (buf, "%d", n+1);

  gripe_invalid_index (buf, nd, dim, var);
}

void
gripe_invalid_index (double n, octave_idx_type nd, octave_idx_type dim,
                     const char *var)
{
  char buf[64];

  sprintf (buf, "%g", n+1);

  gripe_invalid_index (buf, nd, dim, var);
}


// Gripe and exception for read access beyond the bounds of an array.

class out_of_range : public index_exception
{
public:

  out_of_range (const char *value, octave_idx_type nd_in,octave_idx_type dim_in)
        : index_exception (value, nd_in, dim_in), extent(0)
    { }

  const char* explain (void) const
  {
    static std::string expl;    // should probably be member variable, but
                                // then explain can't be const.

    if (nd >= size.length ())   // if not an index slice
      {
        if (var != "")
          expl = "but " + var + " has size ";
        else
          expl = "but object has size ";

        expl = expl + size.str ('x');
      }
    else
      {
        char buf[64];
        sprintf (buf, "%d", extent);
        expl = "out of bound " + std::string (buf);
      }

    return expl.c_str ();
  }

  // ID of error to throw.
  const char* id (void) const
  {
    return (error_id_index_out_of_bounds);
  }

  void set_size (const dim_vector& size_in) { size = size_in; }

  void set_extent (octave_idx_type ext) { extent = ext; }

private:

  dim_vector size;          // dimension of object being accessed

  octave_idx_type extent;   // length of dimension being accessed
};

// Complain of an index that is out of range, but we don't know matrix size
void
gripe_index_out_of_range (int nd, int dim, octave_idx_type idx,
                          octave_idx_type ext)
{
    char buf[64];
    sprintf (buf, "%d", idx);
    out_of_range e (buf, nd, dim);

    e.set_extent (ext);
    dim_vector d (1,1,1,1,1,1,1);   // make explain give extent not size
    e.set_size (d);
    throw e;
}

// Complain of an index that is out of range
void
gripe_index_out_of_range (int nd, int dim, octave_idx_type idx,
                          octave_idx_type ext, const dim_vector& d)
{
    char buf[64];
    sprintf (buf, "%d", idx);
    out_of_range e (buf, nd, dim);

    e.set_extent (ext);
    e.set_size (d);
    throw e;
}

void
gripe_invalid_resize (void)
{
  (*current_liboctave_error_with_id_handler)
    ("Octave:invalid-resize",
     "Invalid resizing operation or ambiguous assignment to an out-of-bounds array element");
}

void
gripe_singular_matrix (double rcond)
{
  if (rcond == 0.0)
    {
      (*current_liboctave_warning_with_id_handler)
        (warning_id_singular_matrix,
         "matrix singular to machine precision");
    }
  else
    {
      (*current_liboctave_warning_with_id_handler)
        (warning_id_nearly_singular_matrix,
         "matrix singular to machine precision, rcond = %g", rcond);
    }
}

/* Tests in test/index.tst */
