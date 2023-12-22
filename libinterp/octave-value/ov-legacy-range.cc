////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2024 The Octave Project Developers
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

#include <istream>
#include <ostream>
#include <sstream>

#include "Range.h"
#include "lo-ieee.h"
#include "lo-utils.h"

#include "variables.h"
#include "error.h"
#include "ovl.h"
#include "oct-hdf5.h"
#include "ov-legacy-range.h"
#include "ov-range.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "pr-output.h"

#include "byte-swap.h"
#include "ls-ascii-helper.h"
#include "ls-hdf5.h"
#include "ls-utils.h"

class Range
{
public:

  Range ()
    : m_base (0), m_limit (0), m_inc (0), m_numel (0)
  { }

  // Assume range is already properly constructed, so just copy internal
  // values.  However, we set LIMIT to the computed final value because
  // that mimics the behavior of the other Range class constructors that
  // reset limit to the computed final value.

  Range (const octave::range<double>& r)
    : m_base (r.base ()), m_limit (r.final_value ()), m_inc (r.increment ()),
      m_numel (r.numel ())
  { }

  Range (const Range& r) = default;

  Range& operator = (const Range& r) = default;

  ~Range () = default;

  Range (double b, double l)
    : m_base (b), m_limit (l), m_inc (1), m_numel (numel_internal ())
  {
    if (! octave::math::isinf (m_limit))
      m_limit = limit_internal ();
  }

  Range (double b, double l, double i)
    : m_base (b), m_limit (l), m_inc (i), m_numel (numel_internal ())
  {
    if (! octave::math::isinf (m_limit))
      m_limit = limit_internal ();
  }

  // The range has a finite number of elements.
  bool ok () const
  {
    return (octave::math::isfinite (m_limit)
            && (m_numel >= 0 || m_numel == -2));
  }

  double base () const { return m_base; }
  double limit () const { return m_limit; }
  double increment () const { return m_inc; }

  octave_idx_type numel () const { return m_numel; }

  bool all_elements_are_ints () const;

  Matrix matrix_value () const;

  double min () const;
  double max () const;

private:

  double m_base;
  double m_limit;
  double m_inc;

  octave_idx_type m_numel;

  octave_idx_type numel_internal () const;

  double limit_internal () const;

  void init ();
};

bool
Range::all_elements_are_ints () const
{
  // If the base and increment are ints, the final value in the range will also
  // be an integer, even if the limit is not.  If there is one or fewer
  // elements only the base needs to be an integer.

  return (! (octave::math::isnan (m_base) || octave::math::isnan (m_inc))
          && (octave::math::nint_big (m_base) == m_base || m_numel < 1)
          && (octave::math::nint_big (m_inc) == m_inc || m_numel <= 1));
}

Matrix
Range::matrix_value () const
{
  Matrix retval (1, m_numel);

  if (m_numel > 0)
    {
      // The first element must always be *exactly* the base.
      // E.g, -0 would otherwise become +0 in the loop (-0 + 0*increment).
      retval(0) = m_base;

      double b = m_base;
      double increment = m_inc;
      for (octave_idx_type i = 1; i < m_numel - 1; i++)
        retval.xelem (i) = b + i * increment;

      retval.xelem (m_numel - 1) = m_limit;
    }

  return retval;
}

// NOTE: max and min only return useful values if numel > 0.
//       do_minmax_body() in max.cc avoids calling Range::min/max if numel == 0.

double
Range::min () const
{
  double retval = 0.0;
  if (m_numel > 0)
    {
      if (m_inc > 0)
        retval = m_base;
      else
        {
          retval = m_base + (m_numel - 1) * m_inc;

          // Require '<=' test.  See note in max ().
          if (retval <= m_limit)
            retval = m_limit;
        }

    }
  return retval;
}

double
Range::max () const
{
  double retval = 0.0;
  if (m_numel > 0)
    {
      if (m_inc > 0)
        {
          retval = m_base + (m_numel - 1) * m_inc;

          // On some machines (x86 with extended precision floating point
          // arithmetic, for example) it is possible that we can overshoot the
          // limit by approximately the machine precision even though we were
          // very careful in our calculation of the number of elements.
          // Therefore, we clip the result to the limit if it overshoots.
          // The test also includes equality (>= m_limit) to have expressions
          // such as -5:1:-0 result in a -0 endpoint.
          if (retval >= m_limit)
            retval = m_limit;
        }
      else
        retval = m_base;
    }
  return retval;
}

// C  See Knuth, Art Of Computer Programming, Vol. 1, Problem 1.2.4-5.
// C
// C===Tolerant FLOOR function.
// C
// C    X  -  is given as a Double Precision argument to be operated on.
// C          It is assumed that X is represented with M mantissa bits.
// C    CT -  is   given   as   a   Comparison   Tolerance   such   that
// C          0.LT.CT.LE.3-SQRT(5)/2. If the relative difference between
// C          X and A whole number is  less  than  CT,  then  TFLOOR  is
// C          returned   as   this   whole   number.   By  treating  the
// C          floating-point numbers as a finite ordered set  note  that
// C          the  heuristic  EPS=2.**(-(M-1))   and   CT=3*EPS   causes
// C          arguments  of  TFLOOR/TCEIL to be treated as whole numbers
// C          if they are  exactly  whole  numbers  or  are  immediately
// C          adjacent to whole number representations.  Since EPS,  the
// C          "distance"  between  floating-point  numbers  on  the unit
// C          interval, and M, the number of bits in X'S mantissa, exist
// C          on  every  floating-point   computer,   TFLOOR/TCEIL   are
// C          consistently definable on every floating-point computer.
// C
// C          For more information see the following references:
// C    (1) P. E. Hagerty, "More On Fuzzy Floor And Ceiling," APL  QUOTE
// C        QUAD 8(4):20-24, June 1978. Note that TFLOOR=FL5.
// C    (2) L. M. Breed, "Definitions For Fuzzy Floor And Ceiling",  APL
// C        QUOTE QUAD 8(3):16-23, March 1978. This paper cites FL1 through
// C        FL5, the history of five years of evolutionary development of
// C        FL5 - the seven lines of code below - by open collaboration
// C        and corroboration of the mathematical-computing community.
// C
// C  Penn State University Center for Academic Computing
// C  H. D. Knoble - August, 1978.

static inline double
tfloor (double x, double ct)
{
// C---------FLOOR(X) is the largest integer algebraically less than
// C         or equal to X; that is, the unfuzzy FLOOR function.

//  DINT (X) = X - DMOD (X, 1.0);
//  FLOOR (X) = DINT (X) - DMOD (2.0 + DSIGN (1.0, X), 3.0);

// C---------Hagerty's FL5 function follows...

  double q = 1.0;

  if (x < 0.0)
    q = 1.0 - ct;

  double rmax = q / (2.0 - ct);

  double t1 = 1.0 + std::floor (x);
  t1 = (ct / q) * (t1 < 0.0 ? -t1 : t1);
  t1 = (rmax < t1 ? rmax : t1);
  t1 = (ct > t1 ? ct : t1);
  t1 = std::floor (x + t1);

  if (x <= 0.0 || (t1 - x) < rmax)
    return t1;
  else
    return t1 - 1.0;
}

static inline bool
teq (double u, double v,
     double ct = 3.0 * std::numeric_limits<double>::epsilon ())
{
  double tu = std::abs (u);
  double tv = std::abs (v);

  return std::abs (u - v) < ((tu > tv ? tu : tv) * ct);
}

octave_idx_type
Range::numel_internal () const
{
  octave_idx_type retval = -1;

  if (! octave::math::isfinite (m_base) || ! octave::math::isfinite (m_inc)
      || octave::math::isnan (m_limit))
    retval = -2;
  else if (octave::math::isinf (m_limit)
           && ((m_inc > 0 && m_limit > 0)
               || (m_inc < 0 && m_limit < 0)))
    retval = std::numeric_limits<octave_idx_type>::max () - 1;
  else if (m_inc == 0
           || (m_limit > m_base && m_inc < 0)
           || (m_limit < m_base && m_inc > 0))
    {
      retval = 0;
    }
  else
    {
      double ct = 3.0 * std::numeric_limits<double>::epsilon ();

      double tmp = tfloor ((m_limit - m_base + m_inc) / m_inc, ct);

      octave_idx_type n_elt = (tmp > 0.0
                               ? static_cast<octave_idx_type> (tmp) : 0);

      // If the final element that we would compute for the range is equal to
      // the limit of the range, or is an adjacent floating point number,
      // accept it.  Otherwise, try a range with one fewer element.  If that
      // fails, try again with one more element.
      //
      // I'm not sure this is very good, but it seems to work better than just
      // using tfloor as above.  For example, without it, the expression
      // 1.8:0.05:1.9 fails to produce the expected result of [1.8, 1.85, 1.9].

      if (! teq (m_base + (n_elt - 1) * m_inc, m_limit))
        {
          if (teq (m_base + (n_elt - 2) * m_inc, m_limit))
            n_elt--;
          else if (teq (m_base + n_elt * m_inc, m_limit))
            n_elt++;
        }

      retval = ((n_elt < std::numeric_limits<octave_idx_type>::max ())
                ? n_elt : -1);
    }

  return retval;
}

double
Range::limit_internal () const
{
  double new_limit = m_inc > 0 ? max () : min ();

  // If result must be an integer then force the new_limit to be one.
  if (all_elements_are_ints ())
    new_limit = std::round (new_limit);

  return new_limit;
}

void
Range::init ()
{
  m_numel = numel_internal ();

  if (! octave::math::isinf (m_limit))
    m_limit = limit_internal ();
}

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_legacy_range, "range", "double");

octave_legacy_range::octave_legacy_range ()
  : octave_base_value (), m_range (new Range ()) { }

octave_legacy_range::octave_legacy_range (const Range& r)
  : octave_base_value (), m_range (new Range (r))
{
  if (m_range->numel () < 0 && m_range->numel () != -2)
    error ("invalid range");
}

octave_legacy_range::octave_legacy_range (const octave_legacy_range& r)
  : octave_base_value (r), m_range ()
{
  m_range.reset (new Range (*(r.m_range)));
}

static octave_base_value *
default_numeric_conversion_function (const octave_base_value& a)
{
  const octave_legacy_range& v = dynamic_cast<const octave_legacy_range&> (a);

  return new octave_matrix (v.matrix_value ());
}

octave_base_value::type_conv_info
octave_legacy_range::numeric_conversion_function () const
{
  return octave_base_value::type_conv_info (default_numeric_conversion_function,
                                            octave_matrix::static_type_id ());
}

octave_base_value *
octave_legacy_range::try_narrowing_conversion ()
{
  octave_base_value *retval = nullptr;

  switch (m_range->numel ())
    {
    case 1:
      retval = new octave_scalar (m_range->base ());
      break;

    case 0:
      retval = new octave_matrix (Matrix (1, 0));
      break;

    case -2:
      retval = new octave_matrix (m_range->matrix_value ());
      break;

    default:
      {
        if (m_range->increment () == 0)
          retval = new octave_matrix (m_range->matrix_value ());
        else
          retval = new octave_range
            (octave::range<double> (m_range->base (), m_range->increment (),
                                    m_range->limit (), m_range->numel ()));
      }
      break;
    }

  return retval;
}

// Skip white space and comments on stream IS.

static void
skip_comments (std::istream& is)
{
  char c = '\0';
  while (is.get (c))
    {
      if (c == ' ' || c == '\t' || c == '\n')
        ; // Skip whitespace on way to beginning of next line.
      else
        break;
    }

  octave::skip_until_newline (is, false);
}

bool
octave_legacy_range::load_ascii (std::istream& is)
{
  // # base, limit, range comment added by save ().
  skip_comments (is);

  double base, limit, inc;
  is >> base >> limit >> inc;

  if (! is)
    error ("load: failed to load range constant");

  if (inc != 0)
    m_range.reset (new Range (base, limit, inc));
  else
    m_range.reset (new Range (base, inc, static_cast<octave_idx_type> (limit)));

  return true;
}

bool
octave_legacy_range::load_binary (std::istream& is, bool swap,
                                  octave::mach_info::float_format /* fmt */)
{
  char tmp;
  if (! is.read (reinterpret_cast<char *> (&tmp), 1))
    return false;
  double bas, lim, inc;
  if (! is.read (reinterpret_cast<char *> (&bas), 8))
    return false;
  if (swap)
    swap_bytes<8> (&bas);
  if (! is.read (reinterpret_cast<char *> (&lim), 8))
    return false;
  if (swap)
    swap_bytes<8> (&lim);
  if (! is.read (reinterpret_cast<char *> (&inc), 8))
    return false;
  if (swap)
    swap_bytes<8> (&inc);
  if (inc != 0)
    m_range.reset (new Range (bas, lim, inc));
  else
    m_range.reset (new Range (bas, inc, static_cast<octave_idx_type> (lim)));

  return true;
}

#if defined (HAVE_HDF5)

// The following subroutines creates an HDF5 representation of the way
// we will store Octave range types (triplets of floating-point numbers).
// NUM_TYPE is the HDF5 numeric type to use for storage (e.g.
// H5T_NATIVE_DOUBLE to save as 'double').  Note that any necessary
// conversions are handled automatically by HDF5.

static hid_t
hdf5_make_range_type (hid_t num_type)
{
  hid_t type_id = H5Tcreate (H5T_COMPOUND, sizeof (double) * 3);

  H5Tinsert (type_id, "base", 0 * sizeof (double), num_type);
  H5Tinsert (type_id, "limit", 1 * sizeof (double), num_type);
  H5Tinsert (type_id, "increment", 2 * sizeof (double), num_type);

  return type_id;
}

#endif

bool
octave_legacy_range::load_hdf5 (octave_hdf5_id loc_id, const char *name)
{
  bool retval = false;

#if defined (HAVE_HDF5)

#if defined (HAVE_HDF5_18)
  hid_t data_hid = H5Dopen (loc_id, name, octave_H5P_DEFAULT);
#else
  hid_t data_hid = H5Dopen (loc_id, name);
#endif
  hid_t type_hid = H5Dget_type (data_hid);

  hid_t range_type = hdf5_make_range_type (H5T_NATIVE_DOUBLE);

  if (! hdf5_types_compatible (type_hid, range_type))
    {
      H5Tclose (range_type);
      H5Dclose (data_hid);
      return false;
    }

  hid_t space_hid = H5Dget_space (data_hid);
  hsize_t rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 0)
    {
      H5Tclose (range_type);
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      return false;
    }

  double rangevals[3];
  if (H5Dread (data_hid, range_type, octave_H5S_ALL, octave_H5S_ALL,
               octave_H5P_DEFAULT, rangevals)
      >= 0)
    {
      retval = true;
      octave_idx_type nel;
      if (hdf5_get_scalar_attr (data_hid, H5T_NATIVE_IDX,
                                "OCTAVE_RANGE_NELEM", &nel))
        m_range.reset (new Range (rangevals[0], rangevals[2], nel));
      else
        {
          if (rangevals[2] != 0)
            m_range.reset (new Range (rangevals[0], rangevals[1], rangevals[2]));
          else
            m_range.reset (new Range (rangevals[0], rangevals[2],
                                      static_cast<octave_idx_type> (rangevals[1])));
        }
    }

  H5Tclose (range_type);
  H5Sclose (space_hid);
  H5Dclose (data_hid);

#else
  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);

  warn_load ("hdf5");
#endif

  return retval;
}
