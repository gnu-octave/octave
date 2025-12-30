////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2025 The Octave Project Developers
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

#include <cmath>

#include "lo-ieee.h"
#include "mappers.h"
#include "dNDArray.h"
#include "CNDArray.h"
#include "quit.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"

#include "ov-cx-mat.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static void
get_dim_vecdim_all (const octave_value& dimarg, octave_value& arg,
                    int& dim, Array<int>& perm_vec, bool& do_perm,
                    bool& allflag, const char *fcn)
{
  if (dimarg.is_scalar_type ())
    {
      dim = dimarg.int_value () - 1;
      if (dim < 0)
        error ("%s: invalid dimension DIM = %d", fcn, dim + 1);
    }
  else
    {
      Array<int> vec = dimarg.int_vector_value ();
      std::vector<int> vecdim;
      dim_vector sz = arg.dims ();
      int ndims = arg.ndims ();
      // Check for invalid dims and ignore any dims larger than actual ndims
      for (int i = 0; i < vec.numel (); i++)
        {
          vec(i)--;
          if (vec(i) < 0)
            error ("%s: invalid dimension in VECDIM = %d", fcn, vec(i)+1);
          if (vec(i) < ndims)
            vecdim.push_back (vec(i));
        }
      int n = vecdim.size ();
      // If no dimensions left, set DIM = ndims to return input as is
      if (n == 0)
        {
          dim = ndims;
          return;
        }
      octave_idx_type szvecdim = 1;
      // Check for duplicate dims and add VECDIM to permutation vector
      perm_vec.resize (dim_vector (1, ndims));
      std::sort (vecdim.begin (), vecdim.end ());
      // Check for duplicates FIRST before any array writes
      auto dup = std::adjacent_find (vecdim.begin (), vecdim.end ());
      if (dup != vecdim.end ())
        error ("%s: duplicate dimension in VECDIM = %d", fcn, *dup + 1);

       // Verified vecdim has unique entries in [0, ndims-1], hence n <= ndims
       int out_pos = ndims - n;
       for (int d : vecdim)
         {
           szvecdim *= sz (d);
           perm_vec(out_pos++) = d;
         }

      // Parse vecdim
      if (n == 1)
        // Only one dimension given, treat as if dim were specified instead
        dim = vecdim[0];
      else if (ndims == n)
        // vecdim contains all dimensions, treat as if "all" flag given.
        allflag = true;
      else
        {
          dim_vector new_sz;
          new_sz.resize (ndims - n + 1);
          int idx = 0;
          // Add remaining dims to permutation vector
          for (int i = 0; i < ndims; i++)
            {
              if (std::find (vecdim.begin (), vecdim.end (), i)
                  == vecdim.end ())
                {
                  perm_vec(idx) = i;
                  new_sz(idx) = sz(i);
                  idx++;
                }
            }
          new_sz(idx) = szvecdim;
          arg = arg.permute (perm_vec, false);
          arg = arg.reshape (new_sz);
          do_perm = true;
          dim = idx;
        }
    }
}

static dim_vector
custom_ind2sub (const dim_vector szx, octave_idx_type idx, int ndims)
{
  Array<octave_idx_type> szc;
  szc.resize (dim_vector (1, ndims));
  for (int i = 0; i < ndims; i++)
    szc(i) = szx(i);
  if (ndims > 1)
    for (int i = 1; i < ndims; i++)
      szc(i) *= szc(i-1);

  dim_vector sub;
  sub.resize (ndims);
  for (int i = 0; i < ndims; i++)
    sub(i) = 1;

  for (octave_idx_type i = ndims - 1; i >= 0; i--)
    {
      int dsz = szc(i);
      if (idx > dsz)
        {
          int tmp = idx / dsz; // dzc is never 0 at this point
          if (idx % dsz > 0)
            {
              sub(i+1) = tmp + 1;
              idx -= tmp * dsz;
            }
          else
            {
              sub(i+1) = tmp;
              idx -= (tmp - 1) * dsz;
            }
        }
      if (i == 0)
        sub(i) = idx;
    }
  return sub;
}

static octave_idx_type
custom_sub2ind (const dim_vector& szx, const dim_vector& dims, int ndims)
{
  octave_idx_type ind = 1;
  Array<octave_idx_type> szc;
  szc.resize (dim_vector (1, ndims));
  for (int i = 0; i < ndims; i++)
    szc(i) = szx(i);
  for (octave_idx_type i = 1; i < ndims; i++)
    szc(i) *= szc(i-1);

  Array<octave_idx_type> szce;
  szce.resize (dim_vector (1, ndims + 1));
  szce(0) = 1;
  for (octave_idx_type i = 0; i < ndims; i++)
    szce(i+1) = szc(i);

  for (octave_idx_type i = 0; i < ndims; i++)
    if (szx(i) > 1)
      ind += szce(i) * (dims(i) - 1);

  return ind;
}

static octave_value
get_linear_index (const octave_value& index, const octave_value& arg,
                  const Array<int>& vecdim, bool cumop)
{
  // Get index values and original index size
  Array<octave_idx_type> idx = index.int_vector_value ();
  dim_vector sz_idx = index.dims ();

  // Get size and number of dimensions of input argument
  dim_vector sz_arg = arg.dims ();
  int nd_arg = arg.ndims ();

  // Invalid input in VECDIM has already been ruled out
  // Ignore any exceeding dimensions and get usable dimensions in VECDIM
  std::vector<int> dim_page;
  for (octave_idx_type i = 0; i < vecdim.numel (); i++)
    if (vecdim(i) <= nd_arg)
      dim_page.push_back (vecdim(i));
  int nd_page = dim_page.size ();

  // If no dimensions left, return linear index as [1:numel(x)]
  if (nd_page == 0)
    for (octave_idx_type i = 0; i < idx.numel (); i++)
      idx(i) += i;
  else
    {
      // Sort VECDIM
      std::sort (dim_page.begin (), dim_page.end ());

      // Calculate page size
      dim_vector sz_page;
      sz_page.resize (nd_page);
      for (int i = 0; i < nd_page; i++)
        sz_page(i) = sz_arg(dim_page[i]-1);

      // Calculate operating index vector and reduced index dimensions
      std::vector<int> dim_index;
      dim_vector dim_vec;
      dim_vec.resize (nd_arg);
      for (int i = 0; i < nd_arg; i++)
        {
          dim_vec(i) = 1;
          bool addindex = true;
          for (int j = 0; j < nd_page; j++)
            if (i+1 == dim_page[j])
              addindex = false;
          // Ignore singleton indexing dimensions
          if (addindex && sz_arg(i) > 1)
            dim_index.push_back (i + 1);
        }
      // Calculate reduced indexing size
      int nd_index = dim_index.size ();
      dim_vector sz_index;
      sz_index.resize (nd_index);
      for (int i = 0; i < nd_index; i++)
        sz_index(i) = sz_arg(dim_index[i]-1);

      // In cumulative operations, the first dimension of the operating page is
      // expanded to 'prod (vecdim)'.  If the first dimension of the operating
      // page is less than the first dimension in dim_index, every prod_vecdim
      // number of index elements reuse the corresponding index of the first
      // element.  Otherwise, they get reused after indexing the elements whose
      // dim_index is less than first_vecdim.
      octave_idx_type prod_vecdim = 1;
      octave_idx_type group_index = 1;
      if (cumop)
        {
          for (octave_idx_type i = 0; i < nd_page; i++)
            prod_vecdim *= sz_page(i);
          if (dim_page[0] > dim_index[0])
            for (int i = 0; i < nd_index; i++)
              if (dim_page[0] > dim_index[i])
                group_index *= sz_index(i);
        }

      // Process each index value
      dim_vector tmp;
      octave_idx_type ii;
      for (octave_idx_type i = 0; i < idx.numel (); i++)
        {
          if (cumop)
            {
              if (group_index == 1)
                ii = i / prod_vecdim;
              else
                {
                  octave_idx_type igm = i % group_index;
                  octave_idx_type igf = i / group_index;
                  ii = igm + ((igf / prod_vecdim) * group_index);
                }
            }
          else
            ii = i;
          // Get index position and add it to corresponding dimensions
          tmp = custom_ind2sub (sz_index, ii+1, nd_index);
          for (octave_idx_type j = 0; j < nd_index; j++)
            dim_vec(dim_index[j]-1) = tmp(j);

          // Get page position and add it to corresponding dimensions
          tmp = custom_ind2sub (sz_page, idx(i), nd_page);
          for (octave_idx_type j = 0; j < nd_page; j++)
            dim_vec(dim_page[j]-1) = tmp(j);

          // Calculate linear index
          idx(i) = custom_sub2ind (sz_arg, dim_vec, nd_arg);
        }
    }
  return octave_value (idx).reshape (sz_idx);
}

template <typename ArrayType>
static octave_value_list
do_minmax_red_op (const octave_value& arg,
                  int nargout, int dim, bool ismin)
{
  octave_value_list retval (nargout > 1 ? 2 : 1);
  ArrayType array = octave_value_extract<ArrayType> (arg);

  if (nargout <= 1)
    {
      if (ismin)
        retval(0) = array.min (dim);
      else
        retval(0) = array.max (dim);
    }
  else
    {
      Array<octave_idx_type> idx;
      if (ismin)
        retval(0) = array.min (idx, dim);
      else
        retval(0) = array.max (idx, dim);

      retval(1) = octave_value (idx, true, true);
    }

  return retval;
}

template <typename ArrayType>
static octave_value_list
do_minmax_red_op (const octave_value& arg, int nargout,
                  int dim, bool nanflag, bool ismin)
{
  octave_value_list retval (nargout > 1 ? 2 : 1);
  ArrayType array = octave_value_extract<ArrayType> (arg);

  if (nargout <= 1)
    {
      if (ismin)
        retval(0) = array.min (dim, nanflag);
      else
        retval(0) = array.max (dim, nanflag);
    }
  else
    {
      Array<octave_idx_type> idx;
      if (ismin)
        retval(0) = array.min (idx, dim, nanflag);
      else
        retval(0) = array.max (idx, dim, nanflag);

      retval(1) = octave_value (idx, true, true);
    }

  return retval;
}

template <typename ArrayType>
static octave_value_list
do_minmax_red_op (const octave_value& arg, int nargout,
                  int dim, bool nanflag, bool realabs, bool ismin)
{
  octave_value_list retval (nargout > 1 ? 2 : 1);
  ArrayType array = octave_value_extract<ArrayType> (arg);

  if (nargout <= 1)
    {
      if (ismin)
        retval(0) = array.min (dim, nanflag, realabs);
      else
        retval(0) = array.max (dim, nanflag, realabs);
    }
  else
    {
      Array<octave_idx_type> idx;
      if (ismin)
        retval(0) = array.min (idx, dim, nanflag, realabs);
      else
        retval(0) = array.max (idx, dim, nanflag, realabs);

      retval(1) = octave_value (idx, true, true);
    }

  return retval;
}

// Matlab returns double arrays for min/max operations on character
// arrays, so we specialize here to get that behavior.  Other possible
// solutions are to convert the argument to double here and call the
// code for double, but that could waste memory, or to have the
// underlying charNDArray::min/max functions return NDArray instead of
// charNDArray, but that is inconsistent with the way other min/max
// functions work.

template <>
octave_value_list
do_minmax_red_op<charNDArray> (const octave_value& arg,
                               int nargout, int dim, bool ismin)
{
  octave_value_list retval (nargout > 1 ? 2 : 1);
  charNDArray array = octave_value_extract<charNDArray> (arg);

  if (nargout <= 1)
    {
      if (ismin)
        retval(0) = NDArray (array.min (dim));
      else
        retval(0) = NDArray (array.max (dim));
    }
  else
    {
      Array<octave_idx_type> idx;
      if (ismin)
        retval(0) = NDArray (array.min (idx, dim));
      else
        retval(0) = NDArray (array.max (idx, dim));

      retval(1) = octave_value (idx, true, true);
    }

  return retval;
}

// Specialization for bool arrays (dense or sparse).
template <>
octave_value_list
do_minmax_red_op<boolNDArray> (const octave_value& arg,
                               int nargout, int dim, bool ismin)
{
  octave_value_list retval;

  if (! arg.issparse ())
    {
      if (nargout <= 1)
        {
          // This case can be handled using any/all.
          boolNDArray array = arg.bool_array_value ();

          if (array.isempty ())
            retval(0) = array;
          else if (ismin)
            retval(0) = array.all (dim);
          else
            retval(0) = array.any (dim);
        }
      else
        {
          // any/all don't have indexed versions, so do it via a conversion.
          retval = do_minmax_red_op<int8NDArray> (arg, nargout, dim, ismin);

          retval(0) = retval(0).bool_array_value ();
        }
    }
  else
    {
      // Sparse: Don't use any/all trick, as full matrix could exceed memory.
      // Instead, convert to double.
      retval = do_minmax_red_op<SparseMatrix> (arg, nargout, dim, ismin);

      retval(0) = retval(0).sparse_bool_matrix_value ();
    }

  return retval;
}

template <typename ArrayType>
static octave_value
do_minmax_bin_op (const octave_value& argx, const octave_value& argy,
                  bool ismin)
{
  typedef typename ArrayType::element_type ScalarType;

  octave_value retval;

  if (argx.is_scalar_type ())
    {
      ScalarType x = octave_value_extract<ScalarType> (argx);
      ArrayType y = octave_value_extract<ArrayType> (argy);

      if (ismin)
        retval = min (x, y);
      else
        retval = max (x, y);
    }
  else if (argy.is_scalar_type ())
    {
      ArrayType x = octave_value_extract<ArrayType> (argx);
      ScalarType y = octave_value_extract<ScalarType> (argy);

      if (ismin)
        retval = min (x, y);
      else
        retval = max (x, y);
    }
  else
    {
      ArrayType x = octave_value_extract<ArrayType> (argx);
      ArrayType y = octave_value_extract<ArrayType> (argy);

      if (ismin)
        retval = min (x, y);
      else
        retval = max (x, y);
    }

  return retval;
}

template <typename ArrayType>
static octave_value
do_minmax_bin_op (const octave_value& argx, const octave_value& argy,
                  bool nanflag, bool ismin)
{
  typedef typename ArrayType::element_type ScalarType;

  octave_value retval;

  if (argx.is_scalar_type ())
    {
      ScalarType x = octave_value_extract<ScalarType> (argx);
      ArrayType y = octave_value_extract<ArrayType> (argy);

      if (ismin)
        retval = min (x, y, nanflag);
      else
        retval = max (x, y, nanflag);
    }
  else if (argy.is_scalar_type ())
    {
      ArrayType x = octave_value_extract<ArrayType> (argx);
      ScalarType y = octave_value_extract<ScalarType> (argy);

      if (ismin)
        retval = min (x, y, nanflag);
      else
        retval = max (x, y, nanflag);
    }
  else
    {
      ArrayType x = octave_value_extract<ArrayType> (argx);
      ArrayType y = octave_value_extract<ArrayType> (argy);

      if (ismin)
        retval = min (x, y, nanflag);
      else
        retval = max (x, y, nanflag);
    }

  return retval;
}

template <typename ArrayType>
static octave_value
do_minmax_bin_op (const octave_value& argx, const octave_value& argy,
                  bool nanflag, bool realabs, bool ismin)
{
  typedef typename ArrayType::element_type ScalarType;

  octave_value retval;

  if (argx.is_scalar_type ())
    {
      ScalarType x = octave_value_extract<ScalarType> (argx);
      ArrayType y = octave_value_extract<ArrayType> (argy);

      if (ismin)
        retval = min (x, y, nanflag, realabs);
      else
        retval = max (x, y, nanflag, realabs);
    }
  else if (argy.is_scalar_type ())
    {
      ArrayType x = octave_value_extract<ArrayType> (argx);
      ScalarType y = octave_value_extract<ScalarType> (argy);

      if (ismin)
        retval = min (x, y, nanflag, realabs);
      else
        retval = max (x, y, nanflag, realabs);
    }
  else
    {
      ArrayType x = octave_value_extract<ArrayType> (argx);
      ArrayType y = octave_value_extract<ArrayType> (argy);

      if (ismin)
        retval = min (x, y, nanflag, realabs);
      else
        retval = max (x, y, nanflag, realabs);
    }

  return retval;
}

// Matlab returns double arrays for min/max operations on character
// arrays, so we specialize here to get that behavior.  Other possible
// solutions are to convert the arguments to double here and call the
// code for double, but that could waste a lot of memory, or to have the
// underlying charNDArray::min/max functions return NDArray instead of
// charNDArray, but that is inconsistent with the way other min/max
// functions work.

template <>
octave_value
do_minmax_bin_op<charNDArray> (const octave_value& argx,
                               const octave_value& argy, bool ismin)
{
  octave_value retval;

  charNDArray x = octave_value_extract<charNDArray> (argx);
  charNDArray y = octave_value_extract<charNDArray> (argy);

  if (ismin)
    {
      if (x.numel () == 1)
        retval = NDArray (min (x(0), y));
      else if (y.numel () == 1)
        retval = NDArray (min (x, y(0)));
      else
        retval = NDArray (min (x, y));
    }
  else
    {
      if (x.numel () == 1)
        retval = NDArray (max (x(0), y));
      else if (y.numel () == 1)
        retval = NDArray (max (x, y(0)));
      else
        retval = NDArray (max (x, y));
    }

  return retval;
}

static octave_value_list
do_minmax_body (const octave_value_list& args, int nargout, bool ismin)
{
  int nargin = args.length ();
  int orig_nargin = nargin;

  const char *fcn = (ismin ? "min" : "max");

  bool do_perm = false;
  bool allflag = false;
  bool nanflag = true; // NaNs are ignored by default
  bool cmethod = true; // resolves to "auto"
  bool realabs = true;
  bool red_op = false;
  bool linear = false;

  // Get "ComparisonMethod" optional argument first
  if (nargin > 2
      && args(nargin - 1).is_string () && args(nargin - 2).is_string ())
    {
      std::string name = args(nargin - 2).string_value ();
      std::string sval = args(nargin - 1).string_value ();
      if (name == "ComparisonMethod" || name == "comparisonmethod")
        {
          if (sval == "auto")
            cmethod = true;
          else if (sval == "real")
            cmethod = false;
          else if (sval == "abs")
            {
              cmethod = false;
              realabs = false;
            }
          else
            error ("%s: invalid comparison method '%s'", fcn, sval.c_str ());

          nargin -= 2;
        }
    }

  while (nargin > 2 && args(nargin - 1).is_string ())
    {
      std::string str = args(nargin - 1).string_value ();

      if (str == "all")
        {
          allflag = true;
          red_op = true;
        }
      else if (str == "omitnan" || str == "omitmissing")
        {
          if (args(0).is_double_type () || args(0).is_single_type ())
            nanflag = true;
        }
      else if (str == "includenan" || str == "includemissing")
        nanflag = false;
      else if (str == "linear")
        linear = true;
      else
        error ("%s: unrecognized optional argument '%s'", fcn, str.c_str ());

      nargin--;
    }

  if (nargin < 1 || nargin > 3)
    print_usage ();
  if (allflag && nargin > 2)
    error ("%s: cannot set DIM or VECDIM with 'all' flag", fcn);
  if (nargin > 2)
    red_op = true;
  if (nargin > 1)
    if (orig_nargin > 2 && args(1).rows () == 0 && args(1). columns () == 0)
      red_op = true;

  octave_value_list retval (nargout > 1 ? 2 : 1);
  Array<int> vecdim;

  if (nargin == 1 || red_op)
    {
      octave_value arg = args(0);
      // Handle DIM, VECDIM
      int dim = -1;
      Array<int> perm_vec;
      if (nargin == 3)
        {
          octave_value dimarg = args(2);
          get_dim_vecdim_all (dimarg, arg, dim, perm_vec, do_perm, allflag, fcn);
          vecdim = dimarg.int_vector_value ();

          if (! args(1).isempty ())
            warning ("%s: second argument is ignored", fcn);
        }
      else
        {
          dim_vector dims = arg.dims ();
          vecdim.resize (dim_vector (1, 1));
          vecdim(0) = dims.first_non_singleton () + 1;
        }
      // Handle allflag
      if (allflag)
        arg = arg.reshape (dim_vector (arg.numel (), 1));

      switch (arg.builtin_type ())
        {
        case btyp_double:
          {
            if (arg.is_range () && (dim == -1 || dim == 1))
              {
                range<double> range = arg.range_value ();
                if (range.numel () < 1)
                  {
                    retval(0) = arg;
                    if (nargout > 1)
                      retval(1) = arg;
                  }
                else if (ismin)
                  {
                    retval(0) = range.min ();
                    if (nargout > 1)
                      retval(1) = static_cast<double>
                                  (range.increment () < 0 ? range.numel () : 1);
                  }
                else
                  {
                    retval(0) = range.max ();
                    if (nargout > 1)
                      retval(1) = static_cast<double>
                                  (range.increment () >= 0 ? range.numel ()
                                   : 1);
                  }
              }
            else if (arg.issparse ())
              {
                if (cmethod)
                  retval = do_minmax_red_op<SparseMatrix>
                           (arg, nargout, dim, nanflag, ismin);
                else
                  retval = do_minmax_red_op<SparseMatrix>
                           (arg, nargout, dim, nanflag, realabs, ismin);
              }
            else
              {
                if (cmethod)
                  retval = do_minmax_red_op<NDArray>
                           (arg, nargout, dim, nanflag, ismin);
                else
                  retval = do_minmax_red_op<NDArray>
                           (arg, nargout, dim, nanflag, realabs, ismin);
              }
          }
          break;

        case btyp_complex:
          {
            if (arg.issparse ())
              {
                if (cmethod)
                  retval = do_minmax_red_op<SparseComplexMatrix>
                           (arg, nargout, dim, nanflag, ismin);
                else
                  retval = do_minmax_red_op<SparseComplexMatrix>
                           (arg, nargout, dim, nanflag, realabs, ismin);
              }
            else
              {
                if (cmethod)
                  retval = do_minmax_red_op<ComplexNDArray>
                           (arg, nargout, dim, nanflag, ismin);
                else
                  retval = do_minmax_red_op<ComplexNDArray>
                           (arg, nargout, dim, nanflag, realabs, ismin);
              }
          }
          break;

        case btyp_float:
          {
            if (cmethod)
              retval = do_minmax_red_op<FloatNDArray>
                       (arg, nargout, dim, nanflag, ismin);
            else
              retval = do_minmax_red_op<FloatNDArray>
                       (arg, nargout, dim, nanflag, realabs, ismin);
          }
          break;

        case btyp_float_complex:
          {
            if (cmethod)
              retval = do_minmax_red_op<FloatComplexNDArray>
                       (arg, nargout, dim, nanflag, ismin);
            else
              retval = do_minmax_red_op<FloatComplexNDArray>
                       (arg, nargout, dim, nanflag, realabs, ismin);
          }
          break;

        case btyp_char:
          retval = do_minmax_red_op<charNDArray> (arg, nargout, dim, ismin);
          break;

#define MAKE_INT_BRANCH(X)                                        \
        case btyp_ ## X:                                          \
          {                                                       \
            if (cmethod)                                          \
              retval = do_minmax_red_op<X ## NDArray> (arg,       \
                       nargout, dim, ismin);                      \
            else                                                  \
              retval = do_minmax_red_op<X ## NDArray> (arg,       \
                       nargout, dim, nanflag, realabs, ismin);    \
          }                                                       \
          break;

          MAKE_INT_BRANCH (int8);
          MAKE_INT_BRANCH (int16);
          MAKE_INT_BRANCH (int32);
          MAKE_INT_BRANCH (int64);
          MAKE_INT_BRANCH (uint8);
          MAKE_INT_BRANCH (uint16);
          MAKE_INT_BRANCH (uint32);
          MAKE_INT_BRANCH (uint64);

#undef MAKE_INT_BRANCH

        case btyp_bool:
          retval = do_minmax_red_op<boolNDArray> (arg, nargout, dim, ismin);
          break;

        default:
          err_wrong_type_arg (fcn, arg);
        }

      if (do_perm)
        {
          retval(0) = retval(0).permute (perm_vec, true);
          if (nargout > 1)
            retval(1) = retval(1).permute (perm_vec, true);
        }
    }
  else
    {
      if (nargout > 1)
        error ("%s: two output arguments are not supported for two input arrays", fcn);
      if (linear)
        error ("%s: 'linear' is not supported for two input arrays", fcn);
      octave_value argx = args(0);
      octave_value argy = args(1);
      builtin_type_t xtyp = argx.builtin_type ();
      builtin_type_t ytyp = argy.builtin_type ();
      builtin_type_t rtyp;
      if (xtyp == btyp_char && ytyp == btyp_char)
        rtyp = btyp_char;
      // FIXME: This is what should happen when boolNDArray has max()
      // else if (xtyp == btyp_bool && ytyp == btyp_bool)
      //   rtyp = btyp_bool;
      else
        rtyp = btyp_mixed_numeric (xtyp, ytyp);

      switch (rtyp)
        {
        case btyp_double:
          {
            if ((argx.issparse ()
                 && (argy.issparse () || argy.is_scalar_type ()))
                || (argy.issparse () && argx.is_scalar_type ()))
              {
                if (cmethod)
                  retval = do_minmax_bin_op<SparseMatrix>
                           (argx, argy, nanflag, ismin);
                else
                  retval = do_minmax_bin_op<SparseMatrix>
                           (argx, argy, nanflag, realabs, ismin);
              }
            else
              {
                if (cmethod)
                  retval = do_minmax_bin_op<NDArray>
                           (argx, argy, nanflag, ismin);
                else
                  retval = do_minmax_bin_op<NDArray>
                           (argx, argy, nanflag, realabs, ismin);
              }
          }
          break;

        case btyp_complex:
          {
            if ((argx.issparse ()
                 && (argy.issparse () || argy.is_scalar_type ()))
                || (argy.issparse () && argx.is_scalar_type ()))
              {
                if (cmethod)
                  retval = do_minmax_bin_op<SparseComplexMatrix>
                           (argx, argy, nanflag, ismin);
                else
                  retval = do_minmax_bin_op<SparseComplexMatrix>
                           (argx, argy, nanflag, realabs, ismin);
              }
            else
              {
                if (cmethod)
                  retval = do_minmax_bin_op<ComplexNDArray>
                           (argx, argy, nanflag, ismin);
                else
                  retval = do_minmax_bin_op<ComplexNDArray>
                           (argx, argy, nanflag, realabs, ismin);
              }
          }
          break;

        case btyp_float:
          {
            if (cmethod)
              retval = do_minmax_bin_op<FloatNDArray>
                       (argx, argy, nanflag, ismin);
            else
              retval = do_minmax_bin_op<FloatNDArray>
                       (argx, argy, nanflag, realabs, ismin);
          }
          break;

        case btyp_float_complex:
          {
            if (cmethod)
              retval = do_minmax_bin_op<FloatComplexNDArray>
                       (argx, argy, nanflag, ismin);
            else
              retval = do_minmax_bin_op<FloatComplexNDArray>
                       (argx, argy, nanflag, realabs, ismin);
          }
          break;

        case btyp_char:
          retval = do_minmax_bin_op<charNDArray> (argx, argy, ismin);
          break;

#define MAKE_INT_BRANCH(X)                                      \
        case btyp_ ## X:                                        \
          {                                                     \
            if (cmethod)                                        \
              retval = do_minmax_bin_op<X ## NDArray>           \
                       (argx, argy, ismin);                     \
            else                                                \
              retval = do_minmax_bin_op<X ## NDArray>           \
                       (argx, argy, nanflag, realabs, ismin);   \
          }                                                     \
          break;

          MAKE_INT_BRANCH (int8);
          MAKE_INT_BRANCH (int16);
          MAKE_INT_BRANCH (int32);
          MAKE_INT_BRANCH (int64);
          MAKE_INT_BRANCH (uint8);
          MAKE_INT_BRANCH (uint16);
          MAKE_INT_BRANCH (uint32);
          MAKE_INT_BRANCH (uint64);

#undef MAKE_INT_BRANCH

        // FIXME: This is what should happen when boolNDArray has max()
        // case btyp_bool:
        //   retval = do_minmax_bin_op<boolNDArray> (argx, argy, ismin);
        //   break;

        default:
          error ("%s: cannot compute %s (%s, %s)", fcn, fcn,
                 argx.type_name ().c_str (), argy.type_name ().c_str ());
        }

      // FIXME: Delete when boolNDArray has max()
      if (xtyp == btyp_bool && ytyp == btyp_bool)
        retval(0) = retval(0).bool_array_value ();

    }

  // Process "linear" option
  if (linear && nargout > 1 && ! allflag && ! args(0).isempty ())
    retval(1) = get_linear_index (retval(1), args(0), vecdim, false);

  return retval;
}

DEFUN (min, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{m} =} min (@var{x})
@deftypefnx {} {@var{m} =} min (@var{x}, [], @var{dim})
@deftypefnx {} {@var{m} =} min (@var{x}, [], @var{vecdim})
@deftypefnx {} {@var{m} =} min (@var{x}, [], "all")
@deftypefnx {} {@var{m} =} min (@var{x}, [], @var{nanflag})
@deftypefnx {} {[@var{m}, @var{im}] =} min (@dots{})
@deftypefnx {} {[@var{m}, @var{im}] =} min (@dots{}, "linear")
@deftypefnx {} {@var{m} =} min (@var{x}, @var{y})
@deftypefnx {} {@var{m} =} min (@var{x}, @var{y}, @var{nanflag})
@deftypefnx {} {@dots{} =} min (@dots{}, "ComparisonMethod", @var{method})
Find minimum values in the array @var{x}.

If @var{x} is a vector, then @code{min (@var{x})} returns the minimum value of
the elements in @var{x}.

If @var{x} is a matrix, then @code{min (@var{x})} returns a row vector with
each element containing the minimum value of the corresponding column in
@var{x}.

If @var{x} is an array, then @code{min (@var{x})} computes the minimum value
along the first non-singleton dimension of @var{x}.

The optional input @var{dim} specifies the dimension to operate on and must be
a positive integer.  Specifying any singleton dimension of @var{x}, including
any dimension exceeding @code{ndims (@var{x})}, will return @var{x}.

Specifying multiple dimensions with input @var{vecdim}, a vector of
non-repeating dimensions, will operate along the array slice defined by
@var{vecdim}.  If @var{vecdim} indexes all dimensions of @var{x}, then it is
equivalent to the option @qcode{"all"}.  Any dimension in @var{vecdim} greater
than @code{ndims (@var{x})} is ignored.

Specifying the dimension as @qcode{"all"} will cause @code{min} to operate on
on all elements of @var{x}, and is equivalent to @code{min (@var{x}(:))}.

If called with two output arguments, @code{min} also returns the first index of
the minimum value(s) in @var{x}.  The second output argument is only valid when
@code{min} operates on a single input array.  Setting the @qcode{"linear"} flag
returns the linear index to the corresponding minimum values in @var{x}.

If called with two input arrays (@var{x} and @var{y}), @code{min} return the
pairwise minimum according to the rules for @ref{Broadcasting}.

The optional variable @var{nanflag} specifies whether to include or exclude
@code{NaN} values from the calculation using any of the previously specified
input argument combinations.  The default value for @var{nanflag} is
@qcode{"omitnan"} which ignores @code{NaN} values in the calculation.  To
include @code{NaN} values, set the value of @var{nanflag} to
@qcode{"includenan"}, in which case @code{min} will return @code{NaN}, if any
element along the operating dimension is @code{NaN}.

The optional "ComparisonMethod" paired argument specifies the comparison method
for numeric input and it applies to both one input and two input arrays.
@var{method} can take any of the following values:

@table @asis
@item @qcode{'auto'} : This is the default method, which compares elements by
@code{real (@var{x})} when @var{x} is real, and by @code{abs (@var{x})} when
@var{x} is complex.

@item @qcode{'real'} : Compares elements by @code{real (@var{x})} when @var{x}
is real or complex.  For elements with equal real parts, a second comparison by
@code{imag (@var{x})} is performed.

@item @qcode{'abs'} : Compares elements by @code{abs (@var{x})} when @var{x}
is real or complex.  For elements with equal magnitude, a second comparison by
@code{angle (@var{x})} in the interval [-pi,pi] is performed.
@end table
@seealso{max, cummin, cummax}
@end deftypefn */)
{
  return do_minmax_body (args, nargout, true);
}

/*
## Test generic double class
%!assert (min ([1, 4, 2, 3]), 1)
%!assert (min ([1; -10; 5; -2]), -10)
%!assert (min ([4, 2i 4.999; -2, 2, 3+4i]), [-2, 2, 4.999])
## Special routines for char arrays
%!assert (min (["abc", "ABC"]), 65)
%!assert (min (["abc"; "CBA"]), [67 66 65])
## Special routines for logical arrays
%!assert (min (logical ([])), logical ([]))
%!assert (min (logical ([0 0 1 0])), false)
%!assert (min (logical ([0 0 1 0; 0 1 1 0])), logical ([0 0 1 0]))
## Single values
%!assert (min (single ([1, 4, 2, 3])), single (1))
%!assert (min (single ([1; -10; 5; -2])), single (-10))
%!assert (min (single ([4, 2i 4.999; -2, 2, 3+4i])), single ([-2, 2, 4.999]))
## Integer values
%!assert (min (uint8 ([1, 4, 2, 3])), uint8 (1))
%!assert (min (uint8 ([1; -10; 5; -2])), uint8 (-10))
%!assert (min (int8 ([1, 4, 2, 3])), int8 (1))
%!assert (min (int8 ([1; -10; 5; -2])), int8 (-10))
%!assert (min (uint16 ([1, 4, 2, 3])), uint16 (1))
%!assert (min (uint16 ([1; -10; 5; -2])), uint16 (-10))
%!assert (min (int16 ([1, 4, 2, 3])), int16 (1))
%!assert (min (int16 ([1; -10; 5; -2])), int16 (-10))
%!assert (min (uint32 ([1, 4, 2, 3])), uint32 (1))
%!assert (min (uint32 ([1; -10; 5; -2])), uint32 (-10))
%!assert (min (int32 ([1, 4, 2, 3])), int32 (1))
%!assert (min (int32 ([1; -10; 5; -2])), int32 (-10))
%!assert (min (uint64 ([1, 4, 2, 3])), uint64 (1))
%!assert (min (uint64 ([1; -10; 5; -2])), uint64 (-10))
%!assert (min (int64 ([1, 4, 2, 3])), int64 (1))
%!assert (min (int64 ([1; -10; 5; -2])), int64 (-10))
## Sparse double values
%!assert (min (sparse ([1, 4, 2, 3])), sparse (1))
%!assert (min (sparse ([1; -10; 5; -2])), sparse(-10))
## Order sparse complex values by phase angle
%!test <*51307>
%! assert (min (sparse ([4, 2i 4.999; -2, 2, 3+4i])), sparse ([-2, 2, 4.999]));
%! assert (min (sparse ([4, -2i 4.99; -2, 2, 3+4i])), sparse ([-2, -2i, 4.99]));
%!assert (min (sparse ([4, 2i 4.999]), sparse ([-2, 2, 3+4i])),
%!        sparse ([-2, 2, 4.999]))
%!assert (min (sparse ([4, -2i 4.999]), sparse ([-2, 2, 3+4i])),
%!        sparse ([-2, -2i, 4.999]))

## Test dimension argument
%!test
%! x = reshape (1:8, [2,2,2]);
%! assert (min (x, [], 1), reshape ([1, 3, 5, 7], [1,2,2]));
%! assert (min (x, [], 2), reshape ([1, 2, 5, 6], [2,1,2]));
%! [y, i] = min (x, [], 3);
%! assert (ndims (y), 2);
%! assert (y, [1, 3; 2, 4]);
%! assert (ndims (i), 2);
%! assert (i, [1, 1; 1, 1]);

## Test vecdim argument
%!test
%! x = reshape (1:8, [2,2,2]);
%! assert (min (x, [], [1, 3]), [1, 3]);
%! assert (min (x, [], [2, 3]), [1; 2]);
%! assert (min (x, [], [1, 2]), reshape ([1, 5], [1,1,2]));
%! assert (min (x, [], [1, 2, 3]), min (x, [], "all"));

## Test 2-output forms for various arg types
## Special routines for char arrays
%!test
%! [y, i] = min (["abc", "ABC"]);
%! assert (y, 65);
%! assert (i, 4);
## Special routines for logical arrays
%!test
%! x = logical ([0 0 1 0]);
%! [y, i] = min (x);
%! assert (y, false);
%! assert (i, 1);
## Special handling of ranges
%!test
%! rng = 1:2:10;
%! [y, i] = min (rng);
%! assert (y, 1);
%! assert (i, 1);
%! rng = 10:-2:1;
%! [y, i] = min (rng);
%! assert (y, 2);
%! assert (i, 5);

## Test 2-input calling form for various arg types
## Test generic double class
%!test
%! x = [1, 2, 3, 4];  y = fliplr (x);
%! assert (min (x, y), [1 2 2 1]);
%! assert (min (x, 3), [1 2 3 3]);
%! assert (min (2, x), [1 2 2 2]);
%! assert (min (x, 2.1i), [1 2 2.1i 2.1i]);
## Test ordering of complex results with equal magnitude
%!test <*51307>
%! x = [1, 2, 3, 4];
%! assert (min (x, 2i), [1 2 2i 2i]);
%! assert (min (x, -2i), [1 -2i -2i -2i]);
## Special routines for char arrays
%!assert (min ("abc", "b"), [97 98 98])
%!assert (min ("b", "cba"), [98 98 97])
## Special handling for logical arrays
%!assert (min ([true false], false), [false false])
%!assert (min (true, [true false]), [true false])
## Single values
%!test
%! x = single ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (min (x, y), single ([1 2 2 1]));
%! assert (min (x, 3), single ([1 2 3 3]));
%! assert (min (2, x), single ([1 2 2 2]));
%! assert (min (x, 2.1i), single ([1 2 2.1i 2.1i]));
## Integer values
%!test
%! x = uint8 ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (min (x, y), uint8 ([1 2 2 1]));
%! assert (min (x, 3), uint8 ([1 2 3 3]));
%! assert (min (2, x), uint8 ([1 2 2 2]));
%! x = int8 ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (min (x, y), int8 ([1 2 2 1]));
%! assert (min (x, 3), int8 ([1 2 3 3]));
%! assert (min (2, x), int8 ([1 2 2 2]));
%! x = uint16 ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (min (x, y), uint16 ([1 2 2 1]));
%! assert (min (x, 3), uint16 ([1 2 3 3]));
%! assert (min (2, x), uint16 ([1 2 2 2]));
%! x = int16 ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (min (x, y), int16 ([1 2 2 1]));
%! assert (min (x, 3), int16 ([1 2 3 3]));
%! assert (min (2, x), int16 ([1 2 2 2]));
%! x = uint32 ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (min (x, y), uint32 ([1 2 2 1]));
%! assert (min (x, 3), uint32 ([1 2 3 3]));
%! assert (min (2, x), uint32 ([1 2 2 2]));
%! x = int32 ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (min (x, y), int32 ([1 2 2 1]));
%! assert (min (x, 3), int32 ([1 2 3 3]));
%! assert (min (2, x), int32 ([1 2 2 2]));
%! x = uint64 ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (min (x, y), uint64 ([1 2 2 1]));
%! assert (min (x, 3), uint64 ([1 2 3 3]));
%! assert (min (2, x), uint64 ([1 2 2 2]));
%! x = int64 ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (min (x, y), int64 ([1 2 2 1]));
%! assert (min (x, 3), int64 ([1 2 3 3]));
%! assert (min (2, x), int64 ([1 2 2 2]));
## Sparse double values
%!test
%! x = sparse ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (min (x, y), sparse ([1 2 2 1]));
%! assert (min (x, 3), sparse ([1 2 3 3]));
%! assert (min (2, x), sparse ([1 2 2 2]));
%! assert (min (x, 2.1i), sparse ([1 2 2.1i 2.1i]));
%!assert (min (sparse ([4, 2i, 4.999; -2, 2, 3+4i])), sparse ([-2, 2, 4.999]))
%!assert (min (sparse ([4, 2+i, 4.999; -2, 2, 3+4i])), sparse ([-2, 2, 4.999]))
%!assert (min (sparse ([4, 2i, 4.999; -2, 2-i, 3+4i])),
%!        sparse ([-2, 2i, 4.999]))

## Sparse with NaNs
%!assert (min (sparse ([NaN, 1, 2, 1])), sparse (1))
%!assert (min (sparse ([NaN; 1; 2; 1])), sparse (1))
%!assert (min (sparse ([0, NaN, 1, 2, 1])), sparse (0))
%!assert (min (sparse ([0; NaN; 1; 2; 1])), sparse (0))
%!assert (min (sparse ([0; NaN; 1; 2; 1]), [], "includenan"), sparse (NaN))
%!assert (min (sparse ([0; NaN; 1; 2; 1]), [], 1, "includenan"), sparse (NaN))
%!assert (min (sparse ([0; NaN; 1; 2; 1]), [], 2, "includenan"),
%!        sparse ([0; NaN; 1; 2; 1]))
%!assert (min (sparse ([NaN, 1i, 2, 1])), sparse (1))
%!assert (min (sparse ([NaN, 1i, 2, 1]), "ComparisonMethod", "real"),
%!        sparse (1i))
%!assert (min (sparse (single ([NaN, 1, 2, 1]))), sparse (1))
%!assert (min (sparse (single ([NaN; 1; 2; 1]))), sparse (1))
%!assert (min (sparse (single ([0, NaN, 1, 2, 1]))), sparse (0))
%!assert (min (sparse (single ([0; NaN; 1; 2; 1]))), sparse (0))
%!assert (min (sparse (single ([0; NaN; 1; 2; 1])), [], "includenan"),
%!        sparse (NaN))
%!assert (min (sparse (single ([0; NaN; 1; 2; 1])), [], 1, "includenan"),
%!        sparse (NaN))
%!assert (min (sparse (single ([NaN, 1i, 2, 1]))), sparse (1))
%!assert (min (sparse (single ([NaN, 1i, 2, 1])), "ComparisonMethod", "real"),
%!        sparse (1i))
%!assert (min (sparse (single ([NaN, 1i, 2, 1]))), sparse (1))

## Test broadcasting of empty matrices with min/max functions
%!assert (min (sparse (zeros (0,1)), sparse ([1, 2, 3, 4])),
%!        sparse (zeros (0,4)))
%!error min (sparse (zeros (0,2)), sparse ([1, 2, 3, 4]))
%!assert (min (sparse (zeros (1,0)), sparse ([1; 2; 3; 4])),
%!        sparse (zeros (4,0)))
%!error min (sparse (zeros (2,0)), sparse ([1; 2; 3; 4]))
%!assert (min (sparse (zeros (0,1)), sparse ([1, 2, 3, 4i])),
%!        sparse (zeros (0,4)))
%!error min (sparse (zeros (0,2)), sparse ([1, 2, 3, 4i]))
%!assert (min (sparse (zeros (1,0)), sparse ([1; 2; 3; 4i])),
%!        sparse (zeros (4,0)))
%!error min (sparse (zeros (2,0)), sparse ([1; 2; 3; 4i]))

## NaNs and complex numbers
%!assert (min (NaN, 0), 0)
%!assert (min (NaN+1i, 0), 0)
%!assert (min (2+2i, 2-2i), 2-2i)
%!assert (min (2-2i, 2+2i), 2-2i)
%!test
%! [M1, I1] = min ([2+2i, 2-2i]);
%! [M2, I2] = min ([2-2i, 2+2i]);
%! assert (M1, M2);
%! assert (I1, 2);
%! assert (I2, 1);
%!assert (min (NaN, 0, "ComparisonMethod", "real"),
%!        min (NaN+1i, 0, "ComparisonMethod", "real"))
%!assert (min (2+i, 1+10i), 2+1i)
%!assert (min (2+i, 1+10i, "ComparisonMethod", "real"), 1+10i)
%!assert (min (2+i, 1+10i, "ComparisonMethod", "abs"), 2+1i)
%!assert (min (2+i, -1+10i, "ComparisonMethod", "abs"), 2+1i)
%!assert (min (2+i, -2+i, "ComparisonMethod", "abs"), 2+1i)
%!assert (min (2+i, 2-i, "ComparisonMethod", "abs"), 2-1i)
%!assert (min (2+i, 2-i, "ComparisonMethod", "real"), 2-1i)
%!assert (min ([1i 2 -3 4]), 1i)
%!assert (min ([-2+i, 2-i]), 2-1i)

## Test nanflag with dense arrays
%!test
%! [m,i] = min ([1,2,3;4,3,NaN;4,5,6], [], 2, "includenan");
%! assert (m, [1; NaN; 4]);
%! assert (i, [1; 3; 1]);
%! [m,i] = min ([1,2,3;4,NaN,NaN;4,5,6], [], 2, "includenan");
%! assert (m, [1; NaN; 4]);
%! assert (i, [1; 2; 1]);
%!test
%! x = magic (3);
%! x(2, 3) = NaN;
%! assert (min (x, [], 2, "includenan"), [1; NaN; 2]);

## Test empty matrices and those with 0 dimensions (including catching errors)
%!assert (size (min (0, zeros (0, 1))), [0, 1])
%!assert (size (min ([], zeros (0, 1))), [0, 0])
%!assert (size (min (zeros (0, 1), [])), [0, 0])
%!assert (size (min ([], zeros (1, 0))), [0, 0])
%!assert (size (min (zeros (1, 0), [])), [0, 0])
%!error min ([1, 2, 3, 4], zeros (1, 0))
%!assert (size (min ([1, 2, 3, 4], zeros (0, 1))), [0, 4])
%!assert (min (0, sparse (zeros (1,0))), sparse (zeros (1, 0)))
%!assert (min (sparse (zeros (1,0)), 0), sparse (zeros (1, 0)))
%!error min (sparse (zeros (1,0)), sparse ([1, 2, 3, 4]))
%!error min ([1, 2, NaN, 4], zeros (1, 0), 'includenan')
%!assert (min ([1, 2, NaN, 4], zeros (0, 1), 'includenan'), zeros (0, 4))
%!error min (sparse (zeros (1,0)), sparse ([1, 2, NaN 4]), 'includenan')
%!assert (min (sparse (zeros (1,0)), sparse ([1; 2; 3; 4])),
%!        sparse (zeros (4, 0)))
%!assert (min (zeros(0,3), zeros (0, 1)), zeros (0, 3))
%!error min (zeros(3, 0), zeros (0, 1))
%!assert (min (zeros(3, 0), zeros (1, 0)), zeros (3, 0))
%!error min (zeros(3, 0), zeros (0, 0))
%!error min (zeros(3, 0), [])
%!error min ([], zeros(3, 0))
%!error min (zeros(0,1), zeros(3, 0))
%!assert (min (zeros(1,0), zeros(3, 0)), zeros (3, 0))

## Test "linear" option
%!shared x
%! x = repmat ([4,3,2,4,1,5,3,2], 3, 2, 5, 2);
%!test
%! [m, i] = min (x, [], [2, 3], 'linear');
%! assert (m, ones (3, 1, 1, 2));
%! assert (i(:,:,1,1), [13; 14; 15]);
%! assert (i(:,:,1,2), [253; 254; 255]);
%!test
%! [m, i] = min (x, [], [1, 3], 'linear');
%! assert (m, x(1,:,1,:));
%! assert (i(:,:,1,1), [1:3:46]);
%! assert (i(:,:,1,2), [241:3:286]);
%!test
%! [m, i] = min (x, [], [2, 4], 'linear');
%! assert (m, ones (3, 1, 5));
%! assert (i(:,:,1), [13; 14; 15]);
%! assert (i(:,:,2), [61; 62; 63]);
%! assert (i(:,:,3), [109; 110; 111]);
%! assert (i(:,:,4), [157; 158; 159]);
%! assert (i(:,:,5), [205; 206; 207]);
%!test
%! [m, i] = min (x, [], [1, 4], 'linear');
%! assert (m, x(1, :,:,1));
%! assert (i(:,:,1), [1:3:46]);
%! assert (i(:,:,2), [49:3:94]);
%! assert (i(:,:,3), [97:3:142]);
%! assert (i(:,:,4), [145:3:190]);
%! assert (i(:,:,5), [193:3:238]);
%!test
%! [m, i] = min (x, [], [1, 2, 3], 'linear');
%! assert (m, ones (1, 1, 1, 2));
%! assert (i(:,:,1,1), 13);
%! assert (i(:,:,1,2), 253);
%!test
%! [m, i] = min (x, [], [2, 3, 4], 'linear');
%! assert (m, [1; 1; 1]);
%! assert (i, [13; 14; 15]);
%!test
%! [m, i] = min ([1, 2, 3; 4, 5, 6], [], 1, 'linear');
%! assert (m, [1, 2, 3]);
%! assert (i, [1, 3, 5]);
%!test
%! [m, i] = min ([1, 2, 3; 4, 5, 6], [], 2, 'linear');
%! assert (m, [1; 4]);
%! assert (i, [1; 2]);

## Test input validation
%!error <Invalid call> min ()
%!error <Invalid call> min (1, 2, 3, 4)
%!error <unrecognized optional argument 'foobar'> min (1, [], "foobar")
%!error <cannot set DIM or VECDIM with 'all' flag>
%! min (ones (3,3), [], 1, "all")
%!error <cannot set DIM or VECDIM with 'all' flag>
%! min (ones (3,3), [], [1, 2], "all")
%!error <invalid dimension DIM = 0> min (ones (3,3), [], 0)
%!error <invalid dimension DIM = -1> min (ones (3,3), [], -1)
%!error <invalid dimension in VECDIM = -2> min (ones (3,3), [], [1 -2])
%!error <duplicate dimension in VECDIM = 2> min (ones (3,3), [], [1 2 2])
%!error <duplicate dimension in VECDIM = 1> min (ones (3,3), [], [1 1 2])
%!warning <second argument is ignored> min ([1 2 3 4], 2, 2);
%!error <wrong type argument 'cell'> min ({1 2 3 4})
%!error <cannot compute min \(cell, scalar\)> min ({1, 2, 3}, 2)
%!error <two output arguments are not supported for two input arrays>
%! [m, i] = min ([], [])
%!error <'linear' is not supported for two input arrays>
%! min ([1 2 3 4], 1,  "linear")
%! [m, i] = min ([1 2 3 4], [], "linear");
*/

DEFUN (max, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{m} =} max (@var{x})
@deftypefnx {} {@var{m} =} max (@var{x}, [], @var{dim})
@deftypefnx {} {@var{m} =} max (@var{x}, [], @var{vecdim})
@deftypefnx {} {@var{m} =} max (@var{x}, [], "all")
@deftypefnx {} {@var{m} =} max (@var{x}, [], @var{nanflag})
@deftypefnx {} {[@var{m}, @var{im}] =} max (@dots{})
@deftypefnx {} {[@var{m}, @var{im}] =} max (@dots{}, "linear")
@deftypefnx {} {@var{m} =} max (@var{x}, @var{y})
@deftypefnx {} {@var{m} =} max (@var{x}, @var{y}, @var{nanflag})
@deftypefnx {} {@dots{} =} max (@dots{}, "ComparisonMethod", @var{method})
Find maximum values in the array @var{x}.

If @var{x} is a vector, then @code{max (@var{x})} returns the maximum value of
the elements in @var{x}.

If @var{x} is a matrix, then @code{max (@var{x})} returns a row vector with
each element containing the maximum value of the corresponding column in
@var{x}.

If @var{x} is an array, then @code{max (@var{x})} computes the maximum value
along the first non-singleton dimension of @var{x}.

The optional input @var{dim} specifies the dimension to operate on and must be
a positive integer.  Specifying any singleton dimension of @var{x}, including
any dimension exceeding @code{ndims (@var{x})}, will return @var{x}.

Specifying multiple dimensions with input @var{vecdim}, a vector of
non-repeating dimensions, will operate along the array slice defined by
@var{vecdim}.  If @var{vecdim} indexes all dimensions of @var{x}, then it is
equivalent to the option @qcode{"all"}.  Any dimension in @var{vecdim} greater
than @code{ndims (@var{x})} is ignored.

Specifying the dimension as @qcode{"all"} will cause @code{max} to operate on
on all elements of @var{x}, and is equivalent to @code{max (@var{x}(:))}.

If called with two output arguments, @code{max} also returns the first index of
the maximum value(s) in @var{x}.  The second output argument is only valid when
@code{max} operates on a single input array.  Setting the @qcode{"linear"} flag
returns the linear index to the corresponding minimum values in @var{x}.

If called with two input arrays (@var{x} and @var{y}), @code{max} return the
pairwise maximum according to the rules for @ref{Broadcasting}.

The optional variable @var{nanflag} specifies whether to include or exclude
@code{NaN} values from the calculation using any of the previously specified
input argument combinations.  The default value for @var{nanflag} is
@qcode{"omitnan"} which ignores @code{NaN} values in the calculation.  To
include @code{NaN} values, set the value of @var{nanflag} to
@qcode{"includenan"}, in which case @code{max} will return @code{NaN}, if any
element along the operating dimension is @code{NaN}.

The optional "ComparisonMethod" paired argument specifies the comparison method
for numeric input and it applies to both one input and two input arrays.
@var{method} can take any of the following values:

@table @asis
@item @qcode{'auto'} : This is the default method, which compares elements by
@code{real (@var{x})} when @var{x} is real, and by @code{abs (@var{x})} when
@var{x} is complex.

@item @qcode{'real'} : Compares elements by @code{real (@var{x})} when @var{x}
is real or complex.  For elements with equal real parts, a second comparison by
@code{imag (@var{x})} is performed.

@item @qcode{'abs'} : Compares elements by @code{abs (@var{x})} when @var{x}
is real or complex.  For elements with equal magnitude, a second comparison by
@code{angle (@var{x})} in the interval [-pi,pi] is performed.
@end table
@seealso{min, cummax, cummin}
@end deftypefn */)
{
  return do_minmax_body (args, nargout, false);
}

/*
## Test generic double class
%!assert (max ([1, 4, 2, 3]), 4)
%!assert (max ([1; -10; 5; -2]), 5)
%!assert (max ([4, 2i 4.999; -2, 2, 3+4i]), [4, 2i, 3+4i])
## Special routines for char arrays
%!assert (max (["abc", "ABC"]), 99)
%!assert (max (["abc"; "CBA"]), [97 98 99])
## Special routines for logical arrays
%!assert (max (logical ([])), logical ([]))
%!assert (max (logical ([0 0 1 0])), true)
%!assert (max (logical ([0 0 1 0; 0 1 0 0])), logical ([0 1 1 0]))
## Single values
%!assert (max (single ([1, 4, 2, 3])), single (4))
%!assert (max (single ([1; -10; 5; -2])), single (5))
%!assert (max (single ([4, 2i 4.999; -2, 2, 3+4i])), single ([4, 2i, 3+4i]))
## Integer values
%!assert (max (uint8 ([1, 4, 2, 3])), uint8 (4))
%!assert (max (uint8 ([1; -10; 5; -2])), uint8 (5))
%!assert (max (int8 ([1, 4, 2, 3])), int8 (4))
%!assert (max (int8 ([1; -10; 5; -2])), int8 (5))
%!assert (max (uint16 ([1, 4, 2, 3])), uint16 (4))
%!assert (max (uint16 ([1; -10; 5; -2])), uint16 (5))
%!assert (max (int16 ([1, 4, 2, 3])), int16 (4))
%!assert (max (int16 ([1; -10; 5; -2])), int16 (5))
%!assert (max (uint32 ([1, 4, 2, 3])), uint32 (4))
%!assert (max (uint32 ([1; -10; 5; -2])), uint32 (5))
%!assert (max (int32 ([1, 4, 2, 3])), int32 (4))
%!assert (max (int32 ([1; -10; 5; -2])), int32 (5))
%!assert (max (uint64 ([1, 4, 2, 3])), uint64 (4))
%!assert (max (uint64 ([1; -10; 5; -2])), uint64 (5))
%!assert (max (int64 ([1, 4, 2, 3])), int64 (4))
%!assert (max (int64 ([1; -10; 5; -2])), int64 (5))
## Sparse double values
%!assert (max (sparse ([1, 4, 2, 3])), sparse (4))
%!assert (max (sparse ([1; -10; 5; -2])), sparse(5))
## Order sparse complex values by phase angle
%!test <*51307>
%! assert (max (sparse ([4, 2i 4.999; -2, 2, 3+4i])), sparse ([4, 2i, 3+4i]));
%! assert (max (sparse ([4, -2i 4.999; -2, 2, 3+4i])), sparse ([4, 2, 3+4i]));
%!assert (max (sparse ([4, 2i 4.999]), sparse ([-2, 2, 3+4i])),
%!        sparse ([4, 2i, 3+4i]))
%!assert (max (sparse ([4, -2i 4.999]), sparse ([-2, 2, 3+4i])),
%!        sparse ([4, 2, 3+4i]))

## Test dimension argument
%!test
%! x = reshape (1:8, [2,2,2]);
%! assert (max (x, [], 1), reshape ([2, 4, 6, 8], [1,2,2]));
%! assert (max (x, [], 2), reshape ([3, 4, 7, 8], [2,1,2]));
%! [y, i] = max (x, [], 3);
%! assert (ndims (y), 2);
%! assert (y, [5, 7; 6, 8]);
%! assert (ndims (i), 2);
%! assert (i, [2, 2; 2, 2]);

## Test vecdim argument
%!test
%! x = reshape (1:8, [2,2,2]);
%! assert (max (x, [], [1, 3]), [6, 8]);
%! assert (max (x, [], [2, 3]), [7; 8]);
%! assert (max (x, [], [1, 2]), reshape ([4, 8], [1,1,2]));
%! assert (max (x, [], [1, 2, 3]), max (x, [], "all"));

## Test 2-output forms for various arg types
## Special routines for char arrays
%!test
%! [y, i] = max (["abc", "ABC"]);
%! assert (y, 99);
%! assert (i, 3);
## Special routines for logical arrays
%!test
%! x = logical ([0 0 1 0]);
%! [y, i] = max (x);
%! assert (y, true);
%! assert (i, 3);
## Special handling of ranges
%!test
%! rng = 1:2:10;
%! [y, i] = max (rng);
%! assert (y, 9);
%! assert (i, 5);
%! rng = 10:-2:1;
%! [y, i] = max (rng);
%! assert (y, 10);
%! assert (i, 1);

## Test 2-input calling form for various arg types
## Test generic double class
%!test
%! x = [1, 2, 3, 4];  y = fliplr (x);
%! assert (max (x, y), [4 3 3 4]);
%! assert (max (x, 3), [3 3 3 4]);
%! assert (max (2, x), [2 2 3 4]);
%! assert (max (x, 2.1i), [2.1i 2.1i 3 4]);
## Test ordering of complex results with equal magnitude
%!test <*51307>
%! x = [1, 2, 3, 4];
%! assert (max (x, 2i), [2i 2i 3 4]);
%! assert (max (x, -2i), [-2i 2 3 4]);
## Special routines for char arrays
%!assert (max ("abc", "b"), [98 98 99])
%!assert (max ("b", "cba"), [99 98 98])
## Special handling for logical arrays
%!assert (max ([true false], false), [true false])
%!assert (max (true, [false false]), [true true])
## Single values
%!test
%! x = single ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (max (x, y), single ([4 3 3 4]));
%! assert (max (x, 3), single ([3 3 3 4]));
%! assert (max (2, x), single ([2 2 3 4]));
%! assert (max (x, 2.1i), single ([2.1i 2.1i 3 4]));
## Integer values
%!test
%! x = uint8 ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (max (x, y), uint8 ([4 3 3 4]));
%! assert (max (x, 3), uint8 ([3 3 3 4]));
%! assert (max (2, x), uint8 ([2 2 3 4]));
%! x = int8 ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (max (x, y), int8 ([4 3 3 4]));
%! assert (max (x, 3), int8 ([3 3 3 4]));
%! assert (max (2, x), int8 ([2 2 3 4]));
%! x = uint16 ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (max (x, y), uint16 ([4 3 3 4]));
%! assert (max (x, 3), uint16 ([3 3 3 4]));
%! assert (max (2, x), uint16 ([2 2 3 4]));
%! x = int16 ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (max (x, y), int16 ([4 3 3 4]));
%! assert (max (x, 3), int16 ([3 3 3 4]));
%! assert (max (2, x), int16 ([2 2 3 4]));
%! x = uint32 ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (max (x, y), uint32 ([4 3 3 4]));
%! assert (max (x, 3), uint32 ([3 3 3 4]));
%! assert (max (2, x), uint32 ([2 2 3 4]));
%! x = int32 ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (max (x, y), int32 ([4 3 3 4]));
%! assert (max (x, 3), int32 ([3 3 3 4]));
%! assert (max (2, x), int32 ([2 2 3 4]));
%! x = uint64 ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (max (x, y), uint64 ([4 3 3 4]));
%! assert (max (x, 3), uint64 ([3 3 3 4]));
%! assert (max (2, x), uint64 ([2 2 3 4]));
%! x = int64 ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (max (x, y), int64 ([4 3 3 4]));
%! assert (max (x, 3), int64 ([3 3 3 4]));
%! assert (max (2, x), int64 ([2 2 3 4]));
## Sparse double values
%!test
%! x = sparse ([1, 2, 3, 4]);  y = fliplr (x);
%! assert (max (x, y), sparse ([4 3 3 4]));
%! assert (max (x, 3), sparse ([3 3 3 4]));
%! assert (max (2, x), sparse ([2 2 3 4]));
%! assert (max (x, 2.1i), sparse ([2.1i 2.1i 3 4]));
%!assert (max (sparse ([4, 2i, 4.999; -2, 2, 3+4i])), sparse ([4, 2i, 3+4i]))
%!assert (max (sparse ([4, 2+i, 4.999; -2, 2, 3+4i])), sparse ([4, 2+i, 3+4i]))
%!assert (max (sparse ([4, 2i, 4.999; -2, 2-i, 3+4i])), sparse ([4, 2-i, 3+4i]))

## Test for bug #40743
%!assert <*40743> (max (zeros (1,0), ones (1,1)), zeros (1,0))
%!assert <*40743> (max (sparse (zeros (1,0)), sparse (ones (1,1))),
%!                sparse (zeros (1,0)))

## Sparse with NaNs
%!assert (max (sparse ([NaN, 1, 2, 1])), sparse (2))
%!assert (max (sparse ([NaN; 1; 2; 1])), sparse (2))
%!assert (max (sparse ([0, NaN, 1, 2, 1])), sparse (2))
%!assert (max (sparse ([0; NaN; 1; 2; 1])), sparse (2))
%!assert (max (sparse ([0; NaN; 1; 2; 1]), [], "includenan"), sparse (NaN))
%!assert (max (sparse ([0; NaN; 1; 2; 1]), [], 1, "includenan"), sparse (NaN))
%!assert (max (sparse ([0; NaN; 1; 2; 1]), [], 2, "includenan"),
%!        sparse ([0; NaN; 1; 2; 1]))
%!assert (max (sparse ([NaN, 1i, 2, 1])), sparse (2))
%!assert (max (sparse ([NaN, 1i, 2, 1]), "ComparisonMethod", "real"),
%!        sparse (2))
%!assert (max (sparse (single ([NaN, 1, 2, 1]))), sparse (2))
%!assert (max (sparse (single ([NaN; 1; 2; 1]))), sparse (2))
%!assert (max (sparse (single ([0, NaN, 1, 2, 1]))), sparse (2))
%!assert (max (sparse (single ([0; NaN; 1; 2; 1]))), sparse (2))
%!assert (max (sparse (single ([0; NaN; 1; 2; 1])), [], "includenan"),
%!        sparse (NaN))
%!assert (max (sparse (single ([0; NaN; 1; 2; 1])), [], 1, "includenan"),
%!        sparse (NaN))
%!assert (max (sparse (single ([NaN, 1i, 2, 1]))), sparse (2))
%!assert (max (sparse (single ([NaN, 1i, 2, 1])), "ComparisonMethod", "real"),
%!        sparse (2))
%!assert (max (sparse (single ([NaN, 1i, 1]))), sparse (1i))

## Test broadcasting of empty matrices with min/max functions
%!assert (max (sparse (zeros (0,1)), sparse ([1, 2, 3, 4])),
%!        sparse (zeros (0,4)))
%!error max (sparse (zeros (0,2)), sparse ([1, 2, 3, 4]))
%!assert (max (sparse (zeros (1,0)), sparse ([1; 2; 3; 4])),
%!        sparse (zeros (4,0)))
%!error max (sparse (zeros (2,0)), sparse ([1; 2; 3; 4]))
%!assert (max (sparse (zeros (0,1)), sparse ([1, 2, 3, 4i])),
%!        sparse (zeros (0,4)))
%!error max (sparse (zeros (0,2)), sparse ([1, 2, 3, 4i]))
%!assert (max (sparse (zeros (1,0)), sparse ([1; 2; 3; 4i])),
%!        sparse (zeros (4,0)))
%!error max (sparse (zeros (2,0)), sparse ([1; 2; 3; 4i]))

## NaNs and complex numbers
%!assert (max (NaN, 0), 0)
%!assert (max (NaN+1i, 0), 0)
%!assert (max (2+2i, 2-2i), 2+2i)
%!assert (max (2-2i, 2+2i), 2+2i)
%!test
%! [M1, I1] = max ([2+2i, 2-2i]);
%! [M2, I2] = max ([2-2i, 2+2i]);
%! assert (M1, M2);
%! assert (I1, 1);
%! assert (I2, 2);
%!assert (max (NaN, 0, "ComparisonMethod", "real"),
%!        max (NaN+1i, 0, "ComparisonMethod", "real"))
%!assert (max (2+i, 1+10i), 1+10i)
%!assert (max (2+i, 1+10i, "ComparisonMethod", "real"), 2+i)
%!assert (max (2+i, 1+10i, "ComparisonMethod", "abs"), 1+10i)
%!assert (max (2+i, -1+10i, "ComparisonMethod", "abs"), -1+10i)
%!assert (max (2+i, -2+i, "ComparisonMethod", "abs"), -2+1i)
%!assert (max (2+i, 2-i, "ComparisonMethod", "abs"), 2+1i)
%!assert (max (2+i, 2-i, "ComparisonMethod", "real"), 2+1i)
%!assert (max ([1i 2 -3 4]), 4)
%!assert (max ([-2+i, 2-i]), -2+1i)

## Test nanflag with dense arrays
%!test
%! [m,i] = max ([1,2,3;4,3,NaN;4,5,6], [], 2, "includenan");
%! assert (m, [3; NaN; 6]);
%! assert (i, [3; 3; 3]);
%! [m,i] = max ([1,2,3;4,NaN,NaN;4,5,6], [], 2, "includenan");
%! assert (m, [3; NaN; 6]);
%! assert (i, [3; 2; 3]);
%!test
%! x = magic (3);
%! x(2, 3) = NaN;
%! assert (max (x, [], 2, "includenan"), [8; NaN; 9]);

## Test empty matrices and those with 0 dimensions (including catching errors)
%!assert (size (max (0, zeros (0, 1))), [0, 1])
%!assert (size (max ([], zeros (0, 1))), [0, 0])
%!assert (size (max (zeros (0, 1), [])), [0, 0])
%!assert (size (max ([], zeros (1, 0))), [0, 0])
%!assert (size (max (zeros (1, 0), [])), [0, 0])
%!error max ([1, 2, 3, 4], zeros (1, 0))
%!assert (size (max ([1, 2, 3, 4], zeros (0, 1))), [0, 4])
%!assert (max (0, sparse (zeros (1,0))), sparse (zeros (1, 0)))
%!assert (max (sparse (zeros (1,0)), 0), sparse (zeros (1, 0)))
%!error max (sparse (zeros (1,0)), sparse ([1, 2, 3, 4]))
%!error max ([1, 2, NaN, 4], zeros (1, 0), 'includenan')
%!assert (max ([1, 2, NaN, 4], zeros (0, 1), 'includenan'), zeros (0, 4))
%!error max (sparse (zeros (1,0)), sparse ([1, 2, NaN 4]), 'includenan')
%!assert (max (sparse (zeros (1,0)), sparse ([1; 2; 3; 4])),
%!        sparse (zeros (4, 0)))
%!assert (max (zeros(0,3), zeros (0, 1)), zeros (0, 3))
%!error max (zeros(3, 0), zeros (0, 1))
%!assert (max (zeros(3, 0), zeros (1, 0)), zeros (3, 0))
%!error max (zeros(3, 0), zeros (0, 0))
%!error max (zeros(3, 0), [])
%!error max ([], zeros(3, 0))
%!error max (zeros(0,1), zeros(3, 0))
%!assert (max (zeros(1,0), zeros(3, 0)), zeros (3, 0))

## Test "linear" option
%!shared x
%! x = repmat ([4,3,2,4,1,5,3,2], 3, 2, 5, 2);
%!test
%! [m, i] = max (x, [], [2, 3], 'linear');
%! assert (m, 5 * ones (3, 1, 1, 2));
%! assert (i(:,:,1,1), [16; 17; 18]);
%! assert (i(:,:,1,2), [256; 257; 258]);
%!test
%! [m, i] = max (x, [], [1, 3], 'linear');
%! assert (m, x(1,:,1,:));
%! assert (i(:,:,1,1), [1:3:46]);
%! assert (i(:,:,1,2), [241:3:286]);
%!test
%! [m, i] = max (x, [], [2, 4], 'linear');
%! assert (m, 5 * ones (3, 1, 5));
%! assert (i(:,:,1), [16; 17; 18]);
%! assert (i(:,:,2), [64; 65; 66]);
%! assert (i(:,:,3), [112; 113; 114]);
%! assert (i(:,:,4), [160; 161; 162]);
%! assert (i(:,:,5), [208; 209; 210]);
%!test
%! [m, i] = max (x, [], [1, 4], 'linear');
%! assert (m, x(1, :,:,1));
%! assert (i(:,:,1), [1:3:46]);
%! assert (i(:,:,2), [49:3:94]);
%! assert (i(:,:,3), [97:3:142]);
%! assert (i(:,:,4), [145:3:190]);
%! assert (i(:,:,5), [193:3:238]);
%!test
%! [m, i] = max (x, [], [1, 2, 3], 'linear');
%! assert (m, 5 * ones (1, 1, 1, 2));
%! assert (i(:,:,1,1), 16);
%! assert (i(:,:,1,2), 256);
%!test
%! [m, i] = max (x, [], [2, 3, 4], 'linear');
%! assert (m, [5; 5; 5]);
%! assert (i, [16; 17; 18]);
%!test
%! [m, i] = max ([1, 2, 3; 4, 5, 6], [], 1, 'linear');
%! assert (m, [4, 5, 6]);
%! assert (i, [2, 4, 6]);
%!test
%! [m, i] = max ([1, 2, 3; 4, 5, 6], [], 2, 'linear');
%! assert (m, [3; 6]);
%! assert (i, [5; 6]);

## Test input validation
%!error <Invalid call> max ()
%!error <Invalid call> max (1, 2, 3, 4)
%!error <unrecognized optional argument 'foobar'> max (1, [], "foobar")
%!error <cannot set DIM or VECDIM with 'all' flag>
%! max (ones (3,3), [], 1, "all")
%!error <cannot set DIM or VECDIM with 'all' flag>
%! max (ones (3,3), [], [1, 2], "all")
%!error <invalid dimension DIM = 0> max (ones (3,3), [], 0)
%!error <invalid dimension DIM = -1> max (ones (3,3), [], -1)
%!error <invalid dimension in VECDIM = -2> max (ones (3,3), [], [1 -2])
%!error <duplicate dimension in VECDIM = 2> max (ones (3,3), [], [1 2 2])
%!error <duplicate dimension in VECDIM = 1> max (ones (3,3), [], [1 1 2])
%!warning <second argument is ignored> max ([1 2 3 4], 2, 2);
%!error <wrong type argument 'cell'> max ({1 2 3 4})
%!error <cannot compute max \(cell, scalar\)> max ({1, 2, 3}, 2)
%!error <two output arguments are not supported for two input arrays>
%! [m, i] = max ([], [])
%!error <'linear' is not supported for two input arrays>
%! max ([1 2 3 4], 1,  "linear")
%! [m ,i] = max ([1 2 3 4], [], "linear");
*/

static Array<octave_idx_type>
reverse_index (Array<octave_idx_type> idx, const dim_vector array_size, int dim)
{
  octave_idx_type n = idx.numel ();
  if (dim == -1)
    dim = array_size.first_non_singleton ();
  octave_idx_type dim_size = array_size(dim);
  for (octave_idx_type i = 0; i < n; i++)
    idx(i) = dim_size - idx(i) - 1;
  return idx;
}

template <typename ArrayType>
static octave_value_list
do_cumminmax_red_op (const octave_value& arg, int nargout, int dim,
                     bool nanflag, bool ismin, bool direction)
{
  octave_value_list retval (nargout > 1 ? 2 : 1);
  ArrayType array = octave_value_extract<ArrayType> (arg);

  if (nargout <= 1)
    {
      if (ismin)
        {
          if (direction)
            retval(0) = array.flip (dim).cummin (dim, nanflag).flip (dim);
          else
            retval(0) = array.cummin (dim, nanflag);
        }
      else
        {
          if (direction)
            retval(0) = array.flip (dim).cummax (dim, nanflag).flip (dim);
          else
            retval(0) = array.cummax (dim, nanflag);
        }
    }
  else
    {
      retval.resize (2);
      Array<octave_idx_type> idx;
      if (ismin)
        {
          if (direction)
            {
              retval(0) = array.flip (dim).cummin (idx, dim, nanflag).flip (dim);
              idx = reverse_index (idx, array.dims (), dim);
            }
          else
            retval(0) = array.cummin (idx, dim, nanflag);
        }
      else
        {
          if (direction)
            {
              retval(0) = array.flip (dim).cummax (idx, dim, nanflag).flip (dim);
              idx = reverse_index (idx, array.dims (), dim);
            }
          else
            retval(0) = array.cummax (idx, dim, nanflag);
        }

      retval(1) = octave_value (idx, true, true);
      if (direction)
        retval(1) = retval(1).array_value ().flip (dim);
    }

  return retval;
}

template <typename ArrayType>
static octave_value_list
do_cumminmax_red_op (const octave_value& arg, int nargout, int dim,
                     bool nanflag, bool realabs, bool ismin, bool direction)
{
  octave_value_list retval (nargout > 1 ? 2 : 1);
  ArrayType array = octave_value_extract<ArrayType> (arg);

  if (nargout <= 1)
    {
      if (ismin)
        {
          if (direction)
            retval(0) = array.flip (dim).cummin (dim, nanflag, realabs).flip (dim);
          else
            retval(0) = array.cummin (dim, nanflag, realabs);
        }
      else
        {
          if (direction)
            retval(0) = array.flip (dim).cummax (dim, nanflag, realabs).flip (dim);
          else
            retval(0) = array.cummax (dim, nanflag, realabs);
        }
    }
  else
    {
      retval.resize (2);
      Array<octave_idx_type> idx;
      if (ismin)
        {
          if (direction)
            {
              retval(0) = array.flip (dim).cummin (idx, dim, nanflag, realabs).flip (dim);
              idx = reverse_index (idx, array.dims (), dim);
            }
          else
            retval(0) = array.cummin (idx, dim, nanflag, realabs);
        }
      else
        {
          if (direction)
            {
              retval(0) = array.flip (dim).cummax (idx, dim, nanflag, realabs).flip (dim);
              idx = reverse_index (idx, array.dims (), dim);
            }
          else
            retval(0) = array.cummax (idx, dim, nanflag, realabs);
        }

      retval(1) = octave_value (idx, true, true);
      if (direction)
        retval(1) = retval(1).array_value ().flip (dim);
    }

  return retval;
}

static octave_value_list
do_cumminmax_body (const octave_value_list& args,
                   int nargout, bool ismin)
{
  int nargin = args.length ();

  const char *fcn = (ismin ? "cummin" : "cummax");

  bool direction = false;
  bool do_perm = false;
  bool allflag = false;
  bool nanflag = true; // NaNs are ignored by default
  bool cmethod = true; // resolves to "auto"
  bool realabs = true;
  bool linear = false;

  // Get "ComparisonMethod" optional argument first
  if (nargin > 2
      && args(nargin - 1).is_string () && args(nargin - 2).is_string ())
    {
      std::string name = args(nargin - 2).string_value ();
      std::string sval = args(nargin - 1).string_value ();
      if (name == "ComparisonMethod" || name == "comparisonmethod")
        {
          if (sval == "auto")
            cmethod = true;
          else if (sval == "real")
            cmethod = false;
          else if (sval == "abs")
            {
              cmethod = false;
              realabs = false;
            }
          else
            error ("%s: invalid comparison method '%s'", fcn, sval.c_str ());

          nargin -= 2;
        }
    }

  while (nargin > 1 && args(nargin - 1).is_string ())
    {
      std::string str = args(nargin - 1).string_value ();

      if (str == "forward")
        direction = false;
      else if (str == "reverse")
        direction = true;
      else if (str == "all")
        allflag = true;
      else if (str == "omitnan" || str == "omitmissing")
        {
          if (args(0).is_double_type () || args(0).is_single_type ())
            nanflag = true;
        }
      else if (str == "includenan" || str == "includemissing")
        nanflag = false;
      else if (str == "linear")
        linear = true;
      else
        error ("%s: unrecognized optional argument '%s'", fcn, str.c_str ());

      nargin--;
    }

  if (nargin < 1 || nargin > 2)
    print_usage ();
  if (allflag && nargin > 1)
    error ("%s: cannot set DIM or VECDIM with 'all' flag", fcn);

  octave_value arg = args(0);
  Array<int> vecdim;

  // Handle DIM, VECDIM
  int dim = -1;
  Array<int> perm_vec;
  if (nargin == 2)
    {
      octave_value dimarg = args(1);
      get_dim_vecdim_all (dimarg, arg, dim, perm_vec, do_perm, allflag, fcn);
      vecdim = dimarg.int_vector_value ();
    }
  else
    {
      dim_vector dims = arg.dims ();
      vecdim.resize (dim_vector (1, 1));
      vecdim(0) = dims.first_non_singleton () + 1;
    }

  // Handle allflag
  if (allflag)
    arg = arg.reshape (dim_vector (arg.numel (), 1));

  octave_value_list retval;

  switch (arg.builtin_type ())
    {
    case btyp_double:
      {
        if (cmethod)
          retval = do_cumminmax_red_op<NDArray>
                   (arg, nargout, dim, nanflag, ismin, direction);
        else
          retval = do_cumminmax_red_op<NDArray>
                   (arg, nargout, dim, nanflag, realabs, ismin, direction);
      }
      break;

    case btyp_complex:
      {
        if (cmethod)
          retval = do_cumminmax_red_op<ComplexNDArray>
                   (arg, nargout, dim, nanflag, ismin, direction);
        else
          retval = do_cumminmax_red_op<ComplexNDArray>
                   (arg, nargout, dim, nanflag, realabs, ismin, direction);
      }
      break;

    case btyp_float:
      {
        if (cmethod)
          retval = do_cumminmax_red_op<FloatNDArray>
                   (arg, nargout, dim, nanflag, ismin, direction);
        else
          retval = do_cumminmax_red_op<FloatNDArray>
                   (arg, nargout, dim, nanflag, realabs, ismin, direction);
      }
      break;

    case btyp_float_complex:
      {
        if (cmethod)
          retval = do_cumminmax_red_op<FloatComplexNDArray>
                   (arg, nargout, dim, nanflag, ismin, direction);
        else
          retval = do_cumminmax_red_op<FloatComplexNDArray>
                   (arg, nargout, dim, nanflag, realabs, ismin, direction);
      }
      break;

#define MAKE_INT_BRANCH(X)                                            \
    case btyp_ ## X:                                                  \
      {                                                               \
        if (cmethod)                                                  \
          retval = do_cumminmax_red_op<X ## NDArray> (arg, nargout,   \
                   dim, nanflag, ismin, direction);                   \
        else                                                          \
          retval = do_cumminmax_red_op<X ## NDArray> (arg, nargout,   \
                   dim, nanflag, realabs, ismin, direction);          \
      }                                                               \
      break;

      MAKE_INT_BRANCH (int8);
      MAKE_INT_BRANCH (int16);
      MAKE_INT_BRANCH (int32);
      MAKE_INT_BRANCH (int64);
      MAKE_INT_BRANCH (uint8);
      MAKE_INT_BRANCH (uint16);
      MAKE_INT_BRANCH (uint32);
      MAKE_INT_BRANCH (uint64);

#undef MAKE_INT_BRANCH

    case btyp_bool:
      {
        if (cmethod)
          retval = do_cumminmax_red_op<int8NDArray>
                   (arg, nargout, dim, nanflag, ismin, direction);
        else
          retval = do_cumminmax_red_op<int8NDArray>
                   (arg, nargout, dim, nanflag, realabs, ismin, direction);
        if (retval.length () > 0)
          retval(0) = retval(0).bool_array_value ();
      }
      break;

    default:
      err_wrong_type_arg (fcn, arg);
    }

  if (do_perm)
    {
      retval(0) = retval(0).permute (perm_vec, true);
      if (nargout > 1)
        retval(1) = retval(1).permute (perm_vec, true);
    }

  // Process "linear" option
  if (linear && nargout > 1 && ! allflag && ! args(0).isempty ())
    retval(1) = get_linear_index (retval(1), args(0), vecdim, true);

  return retval;
}

DEFUN (cummin, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{M} =} cummin (@var{x})
@deftypefnx {} {@var{m} =} cummin (@var{x}, @var{dim})
@deftypefnx {} {@var{m} =} cummin (@var{x}, @var{vecdim})
@deftypefnx {} {@var{m} =} cummin (@var{x}, "all")
@deftypefnx {} {@var{m} =} cummin (@dots{}, @var{direction})
@deftypefnx {} {@var{m} =} cummin (@var{x}, @var{nanflag})
@deftypefnx {} {[@var{m}, @var{im}] =} cummin (@dots{})
@deftypefnx {} {[@var{m}, @var{im}] =} cummin (@dots{}, "linear")
@deftypefnx {} {@dots{} =} cummin (@dots{}, "ComparisonMethod", @var{method})
Return the cumulative minimum values from the array @var{x}.

If @var{x} is a vector, then @code{cummin (@var{x})} returns a vector of the
same size with the cumulative minimum values of @var{x}.

If @var{x} is a matrix, then @code{cummin (@var{x})} returns a matrix of the
same size with the cumulative minimum values along each column of @var{x}.

If @var{x} is an array, then @code{cummin(@var{x})} returns an array of the
same size with the cumulative product along the first non-singleton dimension
of @var{x}.

The class of output @var{y} is the same as the class of input @var{x}, unless
@var{x} is logical, in which case @var{y} is double.

The optional input @var{dim} specifies the dimension to operate on and must be
a positive integer.  Specifying any singleton dimension in @var{x}, including
any dimension exceeding @code{ndims (@var{x})}, will return @var{x}.

Specifying multiple dimensions with input @var{vecdim}, a vector of
non-repeating dimensions, will operate along the array slice defined by
@var{vecdim}.  If @var{vecdim} indexes all dimensions of @var{x}, then it is
equivalent to the option @qcode{"all"}.  Any dimension in @var{vecdim} greater
than @code{ndims (@var{x})} is ignored.

Specifying the dimension as @qcode{"all"} will cause @code{cummin} to operate
on all elements of @var{x}, and is equivalent to @code{cummin (@var{x}(:))}.

The optional input @var{direction} specifies how the operating dimension is
traversed and can take the following values:

@table @asis
@item @qcode{"forward"} (default)

The cumulative minimum values are computed from beginning (index 1) to end
along the operating dimension.

@item @qcode{"reverse"}

The cumulative minimum values are computed from end to beginning along the
operating dimension.
@end table

The optional variable @var{nanflag} specifies whether to include or exclude
@code{NaN} values from the calculation using any of the previously specified
input argument combinations.  The default value for @var{nanflag} is
@qcode{"omitnan"} which ignores @code{NaN} values in the calculation.  To
include @code{NaN} values, set the value of @var{nanflag} to
@qcode{"includenan"}, in which case @code{max} will return @code{NaN}, if any
element along the operating dimension is @code{NaN}.

If called with two output arguments, @code{cummin} also returns the first index
of the minimum value(s) in @var{x}.  Setting the @qcode{"linear"} flag returns
the linear index to the corresponding minimum values in @var{x}.

The optional "ComparisonMethod" paired argument specifies the comparison method
for numeric input and it applies to both one input and two input arrays.
@var{method} can take any of the following values:

@table @asis
@item @qcode{'auto'} : This is the default method, which compares elements by
@code{real (@var{x})} when @var{x} is real, and by @code{abs (@var{x})} when
@var{x} is complex.

@item @qcode{'real'} : Compares elements by @code{real (@var{x})} when @var{x}
is real or complex.  For elements with equal real parts, a second comparison by
@code{imag (@var{x})} is performed.

@item @qcode{'abs'} : Compares elements by @code{abs (@var{x})} when @var{x}
is real or complex.  For elements with equal magnitude, a second comparison by
@code{angle (@var{x})} in the interval [-pi,pi] is performed.
@end table
@seealso{cummax, min, max}
@end deftypefn */)
{
  return do_cumminmax_body (args, nargout, true);
}

/*
%!assert (cummin ([1, 4, 2, 3]), [1 1 1 1])
%!assert (cummin ([1; -10; 5; -2]), [1; -10; -10; -10])
%!assert (cummin ([4, i; -2, 2]), [4, i; -2, i])
%!assert (cummin ([1, 2; NaN, 1], 2), [1, 1; NaN, 1])

%!test
%! x = reshape (1:8, [2,2,2]);
%! assert (cummin (x, 1), reshape ([1 1 3 3 5 5 7 7], [2,2,2]));
%! assert (cummin (x, 2), reshape ([1 2 1 2 5 6 5 6], [2,2,2]));
%! [w, iw] = cummin (x, 3);
%! assert (ndims (w), 3);
%! assert (w, repmat ([1 3; 2 4], [1 1 2]));
%! assert (ndims (iw), 3);
%! assert (iw, ones (2,2,2));

## Test "includenan" and "reverse" options
%!assert (cummin ([1; -10; NaN; -2], 'includenan'), [1; -10; NaN; NaN])
%!assert (cummin ([1; -10; NaN; -2], 'reverse', 'includenan'), [NaN(3,1); -2])
%!assert (cummin ([1, -10, NaN, -2], 'includenan'), [1, -10, NaN, NaN])
%!assert (cummin ([1, -10, NaN, -2], 'reverse', 'includenan'), [NaN(1,3), -2])
%!shared A, C
%! A = [3, 5, NaN, 4, 2; 2, 6, 2, 9, 4; 1, 3, 0, NaN, 1; 5, 3, 4, 2, 0];
%! C = [3+2i, 5i, NaN, 4+i, 2i; 2, 6i, 2, 9+2i, 4i; ...
%!      1, 2+3i, 0, NaN, 1i; 5, 3i, 4, 2i, 0+i];
%!test
%! [m, i] = cummin (A, 'includenan');
%! m_exp = [3, 5, NaN, 4, 2; 2, 5, NaN, 4, 2; ...
%!          1, 3, NaN, NaN, 1; 1, 3, NaN, NaN, 0];
%! i_exp = [1, 1, 1, 1, 1; 2, 1, 1, 1, 1; 3, 3, 1, 3, 3; 3, 3, 1, 3, 4];
%! assert (m, m_exp);
%! assert (i, i_exp);
%!test
%! [m, i] = cummin (A, 'includenan', 'reverse');
%! m_exp = [1, 3, NaN, NaN, 0; 1, 3, 0, NaN, 0; ...
%!          1, 3, 0, NaN, 0; 5, 3, 4, 2, 0];
%! i_exp = [3, 4, 1, 3, 4; 3, 4, 3, 3, 4; 3, 4, 3, 3, 4; 4, 4, 4, 4, 4];
%! assert (m, m_exp);
%! assert (i, i_exp);
%!test
%! [m, i] = cummin (A, 'reverse');
%! m_exp = [1, 3, 0, 2, 0; 1, 3, 0, 2, 0; 1, 3, 0, 2, 0; 5, 3, 4, 2, 0];
%! i_exp = [3, 4, 3, 4, 4; 3, 4, 3, 4, 4; 3, 4, 3, 4, 4; 4, 4, 4, 4, 4];
%! assert (m, m_exp);
%! assert (i, i_exp);
%!test
%! [m, i] = cummin (A);
%! m_exp = [3, 5, NaN, 4, 2; 2, 5, 2, 4, 2; 1, 3, 0, 4, 1; 1, 3, 0, 2, 0];
%! i_exp = [1, 1, 1, 1, 1; 2, 1, 2, 1, 1; 3, 3, 3, 1, 3; 3, 3, 3, 4, 4];
%! assert (m, m_exp);
%! assert (i, i_exp);
%!test
%! [m, i] = cummin (A, 2, 'includenan');
%! m_exp = [3, 3, NaN, NaN, NaN; 2, 2, 2, 2, 2; 1, 1, 0, NaN, NaN; 5, 3, 3, 2, 0];
%! i_exp = [1, 1, 3, 3, 3; 1, 1, 1, 1, 1; 1, 1, 3, 4, 4; 1, 2, 2, 4, 5];
%! assert (m, m_exp);
%! assert (i, i_exp);
%!test
%! [m, i] = cummin (A, 2, 'includenan', 'reverse');
%! m_exp = [NaN, NaN, NaN, 2, 2; 2, 2, 2, 4, 4; ...
%!          NaN, NaN, NaN, NaN, 1; 0, 0, 0, 0, 0];
%! i_exp = [3, 3, 3, 5, 5; 3, 3, 3, 5, 5; 4, 4, 4, 4, 5; 5, 5, 5, 5, 5];
%! assert (m, m_exp);
%! assert (i, i_exp);
%!test
%! [m, i] = cummin (A, 2, 'reverse');
%! m_exp = [2, 2, 2, 2, 2; 2, 2, 2, 4, 4; 0, 0, 0, 1, 1; 0, 0, 0, 0, 0];
%! i_exp = [5, 5, 5, 5, 5; 3, 3, 3, 5, 5; 3, 3, 3, 5, 5; 5, 5, 5, 5, 5];
%! assert (m, m_exp);
%! assert (i, i_exp);
%!test
%! [m, i] = cummin (A, 2);
%! m_exp = [3, 3, 3, 3, 2; 2, 2, 2, 2, 2; 1, 1, 0, 0, 0; 5, 3, 3, 2, 0];
%! i_exp = [1, 1, 1, 1, 5; 1, 1, 1, 1, 1; 1, 1, 3, 3, 3; 1, 2, 2, 4, 5];
%! assert (m, m_exp);
%! assert (i, i_exp);

## Test single output against linear option with previous examples
%!test
%! [~, i_lin] = cummin (A, 'includenan', 'linear');
%! assert (cummin (A, 'includenan'), A(i_lin));
%!test
%! [~, i_lin] = cummin (A, 'includenan', 'reverse', 'linear');
%! assert (cummin (A, 'includenan', 'reverse'), A(i_lin));
%!test
%! [~, i_lin] = cummin (A, 'reverse', 'linear');
%! assert (cummin (A, 'reverse'), A(i_lin));
%!test
%! [~, i_lin] = cummin (A, 'linear');
%! assert (cummin (A), A(i_lin));
%!test
%! [~, i_lin] = cummin (A, 2, 'includenan', 'linear');
%! assert (cummin (A, 2, 'includenan'), A(i_lin));
%!test
%! [~, i_lin] = cummin (A, 2, 'includenan', 'reverse', 'linear');
%! assert (cummin (A, 2, 'includenan', 'reverse'), A(i_lin));
%!test
%! [~, i_lin] = cummin (A, 2, 'reverse', 'linear');
%! assert (cummin (A, 2, 'reverse'), A(i_lin));
%!test
%! [~, i_lin] = cummin (A, 2, 'linear');
%! assert (cummin (A, 2), A(i_lin));

## Test "includenan" and "reverse" options with complex input
%!test
%! [m, idx] = cummin (C, 'includenan');
%! m_exp = [3+2i, 5i, NaN, 4+i, 2i; 2, 5i, NaN, 4+i, 2i; ...
%!          1, 2+3i, NaN, NaN, 1i; 1, 3i, NaN, NaN, 1i];
%! i_exp = [1, 1, 1, 1, 1; 2, 1, 1, 1, 1; 3, 3, 1, 3, 3; 3, 4, 1, 3, 3];
%! assert (m, m_exp);
%! assert (idx, i_exp);
%!test
%! [m, idx] = cummin (C, 'includenan', 'reverse');
%! m_exp = [1, 3i, NaN, NaN, 1i; 1, 3i, 0, NaN, 1i; ...
%!          1, 3i, 0, NaN, 1i; 5, 3i, 4, 2i, 1i];
%! i_exp = [3, 4, 1, 3, 4; 3, 4, 3, 3, 4; 3, 4, 3, 3, 4; 4, 4, 4, 4, 4];
%! assert (m, m_exp);
%! assert (idx, i_exp);
%!test
%! [m, idx] = cummin (C, 'reverse');
%! m_exp = [1, 3i, 0, 2i, 1i; 1, 3i, 0, 2i, 1i; ...
%!          1, 3i, 0, 2i, 1i; 5, 3i, 4, 2i, 1i];
%! i_exp = [3, 4, 3, 4, 4; 3, 4, 3, 4, 4; 3, 4, 3, 4, 4; 4, 4, 4, 4, 4];
%! assert (m, m_exp);
%! assert (idx, i_exp);
%!test
%! [m, idx] = cummin (C);
%! m_exp = [3+2i, 5i, NaN, 4+i, 2i; 2, 5i, 2, 4+i, 2i; ...
%!          1, 2+3i, 0, 4+i, 1i; 1, 3i, 0, 2i, 1i];
%! i_exp = [1, 1, 1, 1, 1; 2, 1, 2, 1, 1; 3, 3, 3, 1, 3; 3, 4, 3, 4, 3];
%! assert (m, m_exp);
%! assert (idx, i_exp);
%!test
%! [m, idx] = cummin (C, 2, 'includenan');
%! m_exp = [3+2i, 3+2i, NaN, NaN, NaN; 2, 2, 2, 2, 2; ...
%!          1, 1, 0, NaN, NaN; 5, 3i, 3i, 2i, 1i];
%! i_exp = [1, 1, 3, 3, 3; 1, 1, 1, 1, 1; 1, 1, 3, 4, 4; 1, 2, 2, 4, 5];
%! assert (m, m_exp);
%! assert (idx, i_exp);
%!test
%! [m, idx] = cummin (C, 2, 'includenan', 'reverse');
%! m_exp = [NaN, NaN, NaN, 2i, 2i; 2, 2, 2, 4i, 4i; ...
%!          NaN, NaN, NaN, NaN, 1i; 1i, 1i, 1i, 1i, 1i];
%! i_exp = [3, 3, 3, 5, 5; 3, 3, 3, 5, 5; 4, 4, 4, 4, 5; 5, 5, 5, 5, 5];
%! assert (m, m_exp);
%! assert (idx, i_exp);
%!test
%! [m, idx] = cummin (C, 2, 'reverse');
%! m_exp = [2i, 2i, 2i, 2i, 2i; 2, 2, 2, 4i, 4i; ...
%!          0, 0, 0, 1i, 1i; 1i, 1i, 1i, 1i, 1i];
%! i_exp = [5, 5, 5, 5, 5; 3, 3, 3, 5, 5; 3, 3, 3, 5, 5; 5, 5, 5, 5, 5];
%! assert (m, m_exp);
%! assert (idx, i_exp);
%!test
%! [m, idx] = cummin (C, 2);
%! m_exp = [3+2i, 3+2i, 3+2i, 3+2i, 2i; 2, 2, 2, 2, 2; ...
%!          1, 1, 0, 0, 0; 5, 3i, 3i, 2i, 1i];
%! i_exp = [1, 1, 1, 1, 5; 1, 1, 1, 1, 1; 1, 1, 3, 3, 3; 1, 2, 2, 4, 5];
%! assert (m, m_exp);
%! assert (idx, i_exp);

## Test 'ComparisonMethod' with complex input
%!test
%! [m, idx] = cummin (C, 2, 'ComparisonMethod', 'real');
%! m_exp = [3+2i, 5i, 5i, 5i, 2i; 2, 6i, 6i, 6i, 4i; ...
%!          1, 1, 0, 0, 0; 5, 3i, 3i, 2i, 1i];
%! i_exp = [1, 2, 2, 2, 5; 1, 2, 2, 2, 5; 1, 1, 3, 3, 3; 1, 2, 2, 4, 5];
%! assert (m, m_exp);
%! assert (idx, i_exp);
%!test
%! [m, idx] = cummin (C, 'includenan', 'ComparisonMethod', 'real');
%! m_exp = [3+2i, 5i, NaN, 4+i, 2i; 2, 5i, NaN, 4+i, 2i; ...
%!          1, 5i, NaN, NaN, 1i; 1, 3i, NaN, NaN, 1i];
%! i_exp = [1, 1, 1, 1, 1; 2, 1, 1, 1, 1; 3, 1, 1, 3, 3; 3, 4, 1, 3, 3];
%! assert (m, m_exp);
%! assert (idx, i_exp);

## Test "linear" option with ND array
%!shared x
%! x = randi ([-10, 10], 3, 4, 5, 2);
%!test
%! [m, i] = cummin (x, [2, 3], 'linear');
%! assert (m, x(i));
%!test
%! [m, i] = cummin (x, [1, 3], 'linear');
%! assert (m, x(i));
%!test
%! [m, i] = cummin (x, [2, 4], 'linear');
%! assert (m, x(i));
%!test
%! [m, i] = cummin (x, [1, 4], 'linear');
%! assert (m, x(i));
%!test
%! [m, i] = cummin (x, [1, 2, 3], 'linear');
%! assert (m, x(i));
%!test
%! [m, i] = cummin (x, [2, 3, 4], 'linear');
%! assert (m, x(i));
%!test
%! [m, i] = cummin ([1, 2, 3; 4, 5, 6], 1, 'linear');
%! assert (m, [1, 2, 3; 1, 2, 3]);
%! assert (i, [1, 3, 5; 1, 3, 5]);
%!test
%! [m, i] = cummin ([1, 2, 3; 4, 5, 6], 2, 'linear');
%! assert (m, [1, 1, 1; 4, 4, 4]);
%! assert (i, [1, 1, 1; 2, 2, 2]);
%!test
%! [m, i] = cummin ([1, 2, 3; 4, 5, 6], 1, 'linear', 'reverse');
%! assert (m, [1, 2, 3; 4, 5, 6]);
%! assert (i, [1, 3, 5; 2, 4, 6]);
%!test
%! [m, i] = cummin ([1, 2, 3; 4, 5, 6], 2, 'linear', 'reverse');
%! assert (m, [1, 2, 3; 4, 5, 6]);
%! assert (i, [1, 3, 5; 2, 4, 6]);

%!error <Invalid call> cummin ()
%!error <Invalid call> cummin (1, 2, 3)
%!error <unrecognized optional argument 'foobar'> cummin (1, "foobar")
%!error <cannot set DIM or VECDIM with 'all' flag>
%! cummin (ones (3,3), 1, "all")
%!error <cannot set DIM or VECDIM with 'all' flag>
%! cummin (ones (3,3), [1, 2], "all")
%!error <invalid dimension DIM = 0> cummin (ones (3,3), 0)
%!error <invalid dimension DIM = -1> cummin (ones (3,3), -1)
%!error <invalid dimension in VECDIM = -2> cummin (ones (3,3), [1 -2])
%!error <duplicate dimension in VECDIM = 2> cummin (ones (3,3), [1 2 2])
%!error <duplicate dimension in VECDIM = 1> cummin (ones (3,3), [1 1 2])
%!error <wrong type argument 'cell'> cummin ({1 2 3 4})
*/

DEFUN (cummax, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{M} =} cummax (@var{x})
@deftypefnx {} {@var{m} =} cummax (@var{x}, @var{dim})
@deftypefnx {} {@var{m} =} cummax (@var{x}, @var{vecdim})
@deftypefnx {} {@var{m} =} cummax (@var{x}, "all")
@deftypefnx {} {@var{m} =} cummax (@dots{}, @var{direction})
@deftypefnx {} {@var{m} =} cummax (@var{x}, @var{nanflag})
@deftypefnx {} {[@var{m}, @var{im}] =} cummax (@dots{})
@deftypefnx {} {[@var{m}, @var{im}] =} cummax (@dots{}, "linear")
@deftypefnx {} {@dots{} =} cummax (@dots{}, "ComparisonMethod", @var{method})
Return the cumulative maximum values from the array @var{x}.

If @var{x} is a vector, then @code{cummax (@var{x})} returns a vector of the
same size with the cumulative maximum values of @var{x}.

If @var{x} is a matrix, then @code{cummax (@var{x})} returns a matrix of the
same size with the cumulative maximum values along each column of @var{x}.

If @var{x} is an array, then @code{cummax(@var{x})} returns an array of the
same size with the cumulative product along the first non-singleton dimension
of @var{x}.

The class of output @var{y} is the same as the class of input @var{x}, unless
@var{x} is logical, in which case @var{y} is double.

The optional input @var{dim} specifies the dimension to operate on and must be
a positive integer.  Specifying any singleton dimension in @var{x}, including
any dimension exceeding @code{ndims (@var{x})}, will return @var{x}.

Specifying multiple dimensions with input @var{vecdim}, a vector of
non-repeating dimensions, will operate along the array slice defined by
@var{vecdim}.  If @var{vecdim} indexes all dimensions of @var{x}, then it is
equivalent to the option @qcode{"all"}.  Any dimension in @var{vecdim} greater
than @code{ndims (@var{x})} is ignored.

Specifying the dimension as @qcode{"all"} will cause @code{cummax} to operate
on all elements of @var{x}, and is equivalent to @code{cummax (@var{x}(:))}.

The optional input @var{direction} specifies how the operating dimension is
traversed and can take the following values:

@table @asis
@item @qcode{"forward"} (default)

The cumulative maximum values are computed from beginning (index 1) to end
along the operating dimension.

@item @qcode{"reverse"}

The cumulative maximum values are computed from end to beginning along the
operating dimension.
@end table

The optional variable @var{nanflag} specifies whether to include or exclude
@code{NaN} values from the calculation using any of the previously specified
input argument combinations.  The default value for @var{nanflag} is
@qcode{"omitnan"} which ignores @code{NaN} values in the calculation.  To
include @code{NaN} values, set the value of @var{nanflag} to
@qcode{"includenan"}, in which case @code{max} will return @code{NaN}, if any
element along the operating dimension is @code{NaN}.

If called with two output arguments, @code{cummax} also returns the first index
of the maximum value(s) in @var{x}.  Setting the @qcode{"linear"} flag returns
the linear index to the corresponding minimum values in @var{x}.

The optional "ComparisonMethod" paired argument specifies the comparison method
for numeric input and it applies to both one input and two input arrays.
@var{method} can take any of the following values:

@table @asis
@item @qcode{'auto'} : This is the default method, which compares elements by
@code{real (@var{x})} when @var{x} is real, and by @code{abs (@var{x})} when
@var{x} is complex.

@item @qcode{'real'} : Compares elements by @code{real (@var{x})} when @var{x}
is real or complex.  For elements with equal real parts, a second comparison by
@code{imag (@var{x})} is performed.

@item @qcode{'abs'} : Compares elements by @code{abs (@var{x})} when @var{x}
is real or complex.  For elements with equal magnitude, a second comparison by
@code{angle (@var{x})} in the interval [-pi,pi] is performed.
@end table
@seealso{cummin, max, min}
@end deftypefn */)
{
  return do_cumminmax_body (args, nargout, false);
}

/*
%!assert (cummax ([1, 4, 2, 3]), [1, 4, 4, 4])
%!assert (cummax ([1; -10; 5; -2]), [1; 1; 5; 5])
%!assert (cummax ([4, i, 4.9, -2, 2, 3+4i]), [4, 4, 4.9, 4.9, 4.9, 3+4i])
%!assert (cummax ([1, NaN, 0; NaN, NaN, 1], 2), [1, 1, 1; NaN, NaN, 1])

%!test
%! x = reshape (8:-1:1, [2,2,2]);
%! assert (cummax (x, 1), reshape ([8 8 6 6 4 4 2 2], [2,2,2]));
%! assert (cummax (x, 2), reshape ([8 7 8 7 4 3 4 3], [2,2,2]));
%! [w, iw] = cummax (x, 3);
%! assert (ndims (w), 3);
%! assert (w, repmat ([8 6; 7 5], [1 1 2]));
%! assert (ndims (iw), 3);
%! assert (iw, ones (2,2,2));

## Test "includenan" and "reverse" options
%!assert (cummax ([1; -10; NaN; -2], 'includenan'), [1; 1; NaN; NaN])
%!assert (cummax ([1; -10; NaN; -2], 'reverse', 'includenan'), [NaN(3,1); -2])
%!assert (cummax ([1, -10, NaN, -2], 'includenan'), [1, 1, NaN, NaN])
%!assert (cummax ([1, -10, NaN, -2], 'reverse', 'includenan'), [NaN(1,3), -2])
%!shared A, C
%! A = [3, 5, NaN, 4, 2; 2, 6, 2, 9, 4; 1, 3, 0, NaN, 1; 5, 3, 4, 2, 0];
%! C = [3+2i, 5i, NaN, 4+i, 2i; 2, 6i, 2, 9+2i, 4i; ...
%!      1, 2+3i, 0, NaN, 1i; 5, 3i, 4, 2i, 0+i];
%!test
%! [m, i] = cummax (A, 'includenan');
%! m_exp = [3, 5, NaN, 4, 2; 3, 6, NaN, 9, 4; ...
%!          3, 6, NaN, NaN, 4; 5, 6, NaN, NaN, 4];
%! i_exp = [1, 1, 1, 1, 1; 1, 2, 1, 2, 2; 1, 2, 1, 3, 2; 4, 2, 1, 3, 2];
%! assert (m, m_exp);
%! assert (i, i_exp);
%!test
%! [m, i] = cummax (A, 'includenan', 'reverse');
%! m_exp = [5, 6, NaN, NaN, 4; 5, 6, 4, NaN, 4; ...
%!          5, 3, 4, NaN, 1; 5, 3, 4, 2, 0];
%! i_exp = [4, 2, 1, 3, 2; 4, 2, 4, 3, 2; 4, 4, 4, 3, 3; 4, 4, 4, 4, 4];
%! assert (m, m_exp);
%! assert (i, i_exp);
%!test
%! [m, i] = cummax (A, 'reverse');
%! m_exp = [5, 6, 4, 9, 4; 5, 6, 4, 9, 4; 5, 3, 4, 2, 1; 5, 3, 4, 2, 0];
%! i_exp = [4, 2, 4, 2, 2; 4, 2, 4, 2, 2; 4, 4, 4, 4, 3; 4, 4, 4, 4, 4];
%! assert (m, m_exp);
%! assert (i, i_exp);
%!test
%! [m, i] = cummax (A);
%! m_exp = [3, 5, NaN, 4, 2; 3, 6, 2, 9, 4; 3, 6, 2, 9, 4; 5, 6, 4, 9, 4];
%! i_exp = [1, 1, 1, 1, 1; 1, 2, 2, 2, 2; 1, 2, 2, 2, 2; 4, 2, 4, 2, 2];
%! assert (m, m_exp);
%! assert (i, i_exp);
%!test
%! [m, i] = cummax (A, 2, 'includenan');
%! m_exp = [3, 5, NaN, NaN, NaN; 2, 6, 6, 9, 9; 1, 3, 3, NaN, NaN; 5, 5, 5, 5, 5];
%! i_exp = [1, 2, 3, 3, 3; 1, 2, 2, 4, 4; 1, 2, 2, 4, 4; 1, 1, 1, 1, 1];
%! assert (m, m_exp);
%! assert (i, i_exp);
%!test
%! [m, i] = cummax (A, 2, 'includenan', 'reverse');
%! m_exp = [NaN, NaN, NaN, 4, 2; 9, 9, 9, 9, 4; ...
%!          NaN, NaN, NaN, NaN, 1; 5, 4, 4, 2, 0];
%! i_exp = [3, 3, 3, 4, 5; 4, 4, 4, 4, 5; 4, 4, 4, 4, 5; 1, 3, 3, 4, 5];
%! assert (m, m_exp);
%! assert (i, i_exp);
%!test
%! [m, i] = cummax (A, 2, 'reverse');
%! m_exp = [5, 5, 4, 4, 2; 9, 9, 9, 9, 4; 3, 3, 1, 1, 1; 5, 4, 4, 2, 0];
%! i_exp = [2, 2, 4, 4, 5; 4, 4, 4, 4, 5; 2, 2, 5, 5, 5; 1, 3, 3, 4, 5];
%! assert (m, m_exp);
%! assert (i, i_exp);
%!test
%! [m, i] = cummax (A, 2);
%! m_exp = [3, 5, 5, 5, 5; 2, 6, 6, 9, 9; 1, 3, 3, 3, 3; 5, 5, 5, 5, 5];
%! i_exp = [1, 2, 2, 2, 2; 1, 2, 2, 4, 4; 1, 2, 2, 2, 2; 1, 1, 1, 1, 1];
%! assert (m, m_exp);
%! assert (i, i_exp);

## Test single output against linear option with previous examples
%!test
%! [~, i_lin] = cummax (A, 'includenan', 'linear');
%! assert (cummax (A, 'includenan'), A(i_lin));
%!test
%! [~, i_lin] = cummax (A, 'includenan', 'reverse', 'linear');
%! assert (cummax (A, 'includenan', 'reverse'), A(i_lin));
%!test
%! [~, i_lin] = cummax (A, 'reverse', 'linear');
%! assert (cummax (A, 'reverse'), A(i_lin));
%!test
%! [~, i_lin] = cummax (A, 'linear');
%! assert (cummax (A), A(i_lin));
%!test
%! [~, i_lin] = cummax (A, 2, 'includenan', 'linear');
%! assert (cummax (A, 2, 'includenan'), A(i_lin));
%!test
%! [~, i_lin] = cummax (A, 2, 'includenan', 'reverse', 'linear');
%! assert (cummax (A, 2, 'includenan', 'reverse'), A(i_lin));
%!test
%! [~, i_lin] = cummax (A, 2, 'reverse', 'linear');
%! assert (cummax (A, 2, 'reverse'), A(i_lin));
%!test
%! [~, i_lin] = cummax (A, 2, 'linear');
%! assert (cummax (A, 2), A(i_lin));

## Test "includenan" and "reverse" options with complex input
%!test
%! [m, idx] = cummax (C, 'includenan');
%! m_exp = [3+2i, 5i, NaN, 4+i, 2i; 3+2i, 6i, NaN, 9+2i, 4i; ...
%!          3+2i, 6i, NaN, NaN, 4i; 5, 6i, NaN, NaN, 4i];
%! i_exp = [1, 1, 1, 1, 1; 1, 2, 1, 2, 2; 1, 2, 1, 3, 2; 4, 2, 1, 3, 2];
%! assert (m, m_exp);
%! assert (idx, i_exp);
%!test
%! [m, idx] = cummax (C, 'includenan', 'reverse');
%! m_exp = [5, 6i, NaN, NaN, 4i; 5, 6i, 4, NaN, 4i; ...
%!          5, 2+3i, 4, NaN, 1i; 5, 3i, 4, 2i, 1i];
%! i_exp = [4, 2, 1, 3, 2; 4, 2, 4, 3, 2; 4, 3, 4, 3, 4; 4, 4, 4, 4, 4];
%! assert (m, m_exp);
%! assert (idx, i_exp);
%!test
%! [m, idx] = cummax (C, 'reverse');
%! m_exp = [5, 6i, 4, 9+2i, 4i; 5, 6i, 4, 9+2i, 4i; ...
%!          5, 2+3i, 4, 2i, 1i; 5, 3i, 4, 2i, 1i];
%! i_exp = [4, 2, 4, 2, 2; 4, 2, 4, 2, 2; 4, 3, 4, 4, 4; 4, 4, 4, 4, 4];
%! assert (m, m_exp);
%! assert (idx, i_exp);
%!test
%! [m, idx] = cummax (C);
%! m_exp = [3+2i, 5i, NaN, 4+i, 2i; 3+2i, 6i, 2, 9+2i, 4i; ...
%!          3+2i, 6i, 2, 9+2i, 4i; 5, 6i, 4, 9+2i, 4i];
%! i_exp = [1, 1, 1, 1, 1; 1, 2, 2, 2, 2; 1, 2, 2, 2, 2; 4, 2, 4, 2, 2];
%! assert (m, m_exp);
%! assert (idx, i_exp);
%!test
%! [m, idx] = cummax (C, 2, 'includenan');
%! m_exp = [3+2i, 5i, NaN, NaN, NaN; 2, 6i, 6i, 9+2i, 9+2i; ...
%!          1, 2+3i, 2+3i, NaN, NaN; 5, 5, 5, 5, 5];
%! i_exp = [1, 2, 3, 3, 3; 1, 2, 2, 4, 4; 1, 2, 2, 4, 4; 1, 1, 1, 1, 1];
%! assert (m, m_exp);
%! assert (idx, i_exp);
%!test
%! [m, idx] = cummax (C, 2, 'includenan', 'reverse');
%! m_exp = [NaN, NaN, NaN, 4+i, 2i; 9+2i, 9+2i, 9+2i, 9+2i, 4i; ...
%!          NaN, NaN, NaN, NaN, 1i; 5, 4, 4, 2i, 1i];
%! i_exp = [3, 3, 3, 4, 5; 4, 4, 4, 4, 5; 4, 4, 4, 4, 5; 1, 3, 3, 4, 5];
%! assert (m, m_exp);
%! assert (idx, i_exp);
%!test
%! [m, idx] = cummax (C, 2, 'reverse');
%! m_exp = [5i, 5i, 4+i, 4+i, 2i; 9+2i, 9+2i, 9+2i, 9+2i, 4i; ...
%!          2+3i, 2+3i, 1i, 1i, 1i; 5, 4, 4, 2i, 1i];
%! i_exp = [2, 2, 4, 4, 5; 4, 4, 4, 4, 5; 2, 2, 5, 5, 5; 1, 3, 3, 4, 5];
%! assert (m, m_exp);
%! assert (idx, i_exp);
%!test
%! [m, idx] = cummax (C, 2);
%! m_exp = [3+2i, 5i, 5i, 5i, 5i; 2, 6i, 6i, 9+2i, 9+2i; ...
%!          1, 2+3i, 2+3i, 2+3i, 2+3i; 5, 5, 5, 5, 5];
%! i_exp = [1, 2, 2, 2, 2; 1, 2, 2, 4, 4; 1, 2, 2, 2, 2; 1, 1, 1, 1, 1];
%! assert (m, m_exp);
%! assert (idx, i_exp);

## Test 'ComparisonMethod' with complex input
%!test
%! [m, idx] = cummax (C, 2, 'ComparisonMethod', 'real');
%! m_exp = [3+2i, 3+2i, 3+2i, 4+i, 4+i; 2, 2, 2, 9+2i, 9+2i; ...
%!          1, 2+3i, 2+3i, 2+3i, 2+3i; 5, 5, 5, 5, 5];
%! i_exp = [1, 1, 1, 4, 4; 1, 1, 1, 4, 4; 1, 2, 2, 2, 2; 1, 1, 1, 1, 1];
%! assert (m, m_exp);
%! assert (idx, i_exp);
%!test
%! [m, idx] = cummax (C, 'includenan', 'reverse', 'ComparisonMethod', 'real');
%! m_exp = [5, 2+3i, NaN, NaN, 4i; 5, 2+3i, 4, NaN, 4i; ...
%!          5, 2+3i, 4, NaN, 1i; 5, 3i, 4, 2i, 1i];
%! i_exp = [4, 3, 1, 3, 2; 4, 3, 4, 3, 2; 4, 3, 4, 3, 4; 4, 4, 4, 4, 4];
%! assert (m, m_exp);
%! assert (idx, i_exp);

## Test "linear" option with ND array
%!shared x
%! x = randi ([-10, 10], 3, 4, 5, 2);
%!test
%! [m, i] = cummax (x, [2, 3], 'linear');
%! assert (m, x(i));
%!test
%! [m, i] = cummax (x, [1, 3], 'linear');
%! assert (m, x(i));
%!test
%! [m, i] = cummax (x, [2, 4], 'linear');
%! assert (m, x(i));
%!test
%! [m, i] = cummax (x, [1, 4], 'linear');
%! assert (m, x(i));
%!test
%! [m, i] = cummax (x, [1, 2, 3], 'linear');
%! assert (m, x(i));
%!test
%! [m, i] = cummax (x, [2, 3, 4], 'linear');
%! assert (m, x(i));
%!test
%! [m, i] = cummax ([1, 2, 3; 4, 5, 6], 1, 'linear');
%! assert (m, [1, 2, 3; 4, 5, 6]);
%! assert (i, [1, 3, 5; 2, 4, 6]);
%!test
%! [m, i] = cummax ([1, 2, 3; 4, 5, 6], 2, 'linear');
%! assert (m, [1, 2, 3; 4, 5, 6]);
%! assert (i, [1, 3, 5; 2, 4, 6]);
%!test
%! [m, i] = cummax ([1, 2, 3; 4, 5, 6], 1, 'linear', 'reverse');
%! assert (m, [4, 5, 6; 4, 5, 6]);
%! assert (i, [2, 4, 6; 2, 4, 6]);
%!test
%! [m, i] = cummax ([1, 2, 3; 4, 5, 6], 2, 'linear', 'reverse');
%! assert (m, [3, 3, 3; 6, 6, 6]);
%! assert (i, [5, 5, 5; 6, 6, 6]);

%!error <Invalid call> cummax ()
%!error <Invalid call> cummax (1, 2, 3)
%!error <unrecognized optional argument 'foobar'> cummax (1, "foobar")
%!error <cannot set DIM or VECDIM with 'all' flag>
%! cummax (ones (3,3), 1, "all")
%!error <cannot set DIM or VECDIM with 'all' flag>
%! cummax (ones (3,3), [1, 2], "all")
%!error <invalid dimension DIM = 0> cummax (ones (3,3), 0)
%!error <invalid dimension DIM = -1> cummax (ones (3,3), -1)
%!error <invalid dimension in VECDIM = -2> cummax (ones (3,3), [1 -2])
%!error <duplicate dimension in VECDIM = 2> cummax (ones (3,3), [1 2 2])
%!error <duplicate dimension in VECDIM = 1> cummax (ones (3,3), [1 1 2])
%!error <wrong type argument 'cell'> cummax ({1 2 3 4})
*/

OCTAVE_END_NAMESPACE(octave)
