/*

Copyright (C) 2007 David Bateman

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

#include <string>
#include <vector>
#include <list>

#include "lo-mappers.h"

#include "oct-map.h"
#include "defun-dld.h"
#include "parse.h"
#include "variables.h"
#include "ov-colon.h"
#include "unwind-prot.h"

static bool
maybe_update_column (octave_value& Ac, const octave_value& A, 
		     const dim_vector& dva, const dim_vector& dvc,
		     octave_idx_type i, octave_value_list &idx)
{
  octave_idx_type nd = dva.length ();

  if (i == 0)
    {
      idx(0) = octave_value (':');
      for (octave_idx_type j = 1; j < nd; j++)
	{
	  if (dva (j) == 1)
	    idx (j) = octave_value (1);
	  else
	    idx (j) = octave_value ((i % dvc(j)) + 1);

	  i = i / dvc (j);
	}

      Ac = A;
      Ac = Ac.single_subsref ("(", idx);
      return true;
    }
  else
    {
      bool is_changed = false;
      octave_idx_type k = i;
      octave_idx_type k1 = i - 1;
      for (octave_idx_type j = 1; j < nd; j++)
	{
	  if (dva(j) != 1 && k % dvc (j) != k1 % dvc (j))
	    {
	      idx (j) = octave_value ((k % dvc(j)) + 1);
	      is_changed = true;
	    }

	  k = k / dvc (j);
	  k1 = k1 / dvc (j);
	}

      if (is_changed)
	{
	  Ac = A;
	  Ac = Ac.single_subsref ("(", idx);
	  return true;
	}
      else
	return false;
    }
}

#if 0
// FIXME -- this function is not used; is it OK to delete it?
static void
update_index (octave_value_list& idx, const dim_vector& dv, octave_idx_type i)
{
  octave_idx_type nd = dv.length ();

  if (i == 0)
    {
      for (octave_idx_type j = nd - 1; j > 0; j--)
	idx(j) = octave_value (static_cast<double>(1));
      idx(0) = octave_value (':');
    }
  else
    {
      for (octave_idx_type j = 1; j < nd; j++)
	{
	  idx (j) = octave_value (i % dv (j) + 1);
	  i = i / dv (j);
	}
    }
}
#endif

static void
update_index (Array<int>& idx, const dim_vector& dv, octave_idx_type i)
{
  octave_idx_type nd = dv.length ();

  idx(0) = 0;
  for (octave_idx_type j = 1; j < nd; j++)
    {
      idx (j) = i % dv (j);
      i = i / dv (j);
    }
}

DEFUN_DLD (bsxfun, args, ,
  " -*- texinfo -*-\n\
@deftypefn {Loadable Function} {} bsxfun (@var{f}, @var{a}, @var{b})\n\
Applies a binary function @var{f} element-wise to two matrix arguments\n\
@var{a} and @var{b}. The function @var{f} must be capable of accepting\n\
two column vector arguments of equal length, or one column vector\n\
argument and a scalar.\n\
\n\
The dimensions of @var{a} and @var{b} must be equal or singleton. The\n\
singleton dimensions of the matrices will be expanded to the same\n\
dimensionality as the other matrix.\n\
\n\
@seealso{arrayfun, cellfun}\n\
@end deftypefn")
{
  int nargin = args.length ();
  octave_value_list retval;

  if (nargin != 3)
    print_usage ();
  else
    {
      octave_function *func = 0;
      std::string name;
      std::string fcn_name;

      if (args(0).is_function_handle () || args(0).is_inline_function ())
	func = args(0).function_value ();
      else if (args(0).is_string ())
	{
	  name = args(0).string_value ();
	  fcn_name = unique_symbol_name ("__bsxfun_fcn_");
	  std::string fname = "function y = ";
	  fname.append (fcn_name);
	  fname.append ("(x) y = ");
	  func = extract_function (args(0), "bsxfun", fcn_name, fname,
				   "; endfunction");
	}
      else
	  error ("bsxfun: first argument must be a string or function handle");

      if (! error_state)
	{
	  const octave_value A = args (1);
	  dim_vector dva = A.dims ();
	  octave_idx_type nda = dva.length ();
	  const octave_value B = args (2);
	  dim_vector dvb = B.dims ();
	  octave_idx_type ndb = dvb.length ();
	  octave_idx_type nd = nda;
      
	  if (nda > ndb)
	      dvb.resize (nda, 1);
	  else if (nda < ndb)
	    {
	      dva.resize (ndb, 1);
	      nd = ndb;
	    }

	  for (octave_idx_type i = 0; i < nd; i++)
	    if (dva (i) != dvb (i) && dva (i) != 1 && dvb (i) != 1)
	      {
		error ("bsxfun: dimensions don't match");
		break;
	      }

	  if (!error_state)
	    {
	      // Find the size of the output
	      dim_vector dvc;
	      dvc.resize (nd);
	  
	      for (octave_idx_type i = 0; i < nd; i++)
		dvc (i) = (dva (i) < 1  ? dva (i) : (dvb (i) < 1 ? dvb (i) :
		      (dva (i) > dvb (i) ? dva (i) : dvb (i))));

	      if (dva == dvb || dva.numel () == 1 || dvb.numel () == 1)
		{
		  octave_value_list inputs;
		  inputs (0) = A;
		  inputs (1) = B;
		  retval = feval (func, inputs, 1);
		}
	      else if (dvc.numel () < 1)
		{
		  octave_value_list inputs;
		  inputs (0) = A.resize (dvc);
		  inputs (1) = B.resize (dvc);
		  retval = feval (func, inputs, 1);	      
		}
	      else
		{
		  octave_idx_type ncount = 1;
		  for (octave_idx_type i = 1; i < nd; i++)
		    ncount *= dvc (i);

#define BSXDEF(T) \
		  T result_ ## T; \
		  bool have_ ## T = false;

		  BSXDEF(NDArray);
		  BSXDEF(ComplexNDArray);
		  BSXDEF(boolNDArray);
		  BSXDEF(int8NDArray);
		  BSXDEF(int16NDArray);
		  BSXDEF(int32NDArray);
		  BSXDEF(int64NDArray);
		  BSXDEF(uint8NDArray);
		  BSXDEF(uint16NDArray);
		  BSXDEF(uint32NDArray);
		  BSXDEF(uint64NDArray);

		  octave_value Ac ;
		  octave_value_list idxA;
		  octave_value Bc;
		  octave_value_list idxB;
		  octave_value C;
		  octave_value_list inputs;
		  Array<int> ra_idx (dvc.length(), 0);


		  for (octave_idx_type i = 0; i < ncount; i++)
		    {
		      if (maybe_update_column (Ac, A, dva, dvc, i, idxA))
			inputs (0) = Ac;

		      if (maybe_update_column (Bc, B, dvb, dvc, i, idxB))
			inputs (1) = Bc;
			
		      octave_value_list tmp = feval (func, inputs, 1);

		      if (error_state)
			break;

#define BSXINIT(T, CLS, EXTRACTOR) \
 		      (result_type == CLS) \
			{ \
			    have_ ## T = true; \
			    result_ ## T = \
				tmp (0). EXTRACTOR ## _array_value (); \
			    result_ ## T .resize (dvc); \
                        }

		      if (i == 0)
			{
			  if (! tmp(0).is_sparse_type ())
			    {
			      std::string result_type = tmp(0).class_name ();
			      if (result_type == "double")
				{
				  if (tmp(0).is_real_type ())
				    {
				      have_NDArray = true;
				      result_NDArray = tmp(0).array_value ();
				      result_NDArray.resize (dvc);
				    }
				  else
				    {
				      have_ComplexNDArray = true;
				      result_ComplexNDArray = 
					tmp(0).complex_array_value ();
				      result_ComplexNDArray.resize (dvc);
				    }
				}
			      else if BSXINIT(boolNDArray, "logical", bool)
			      else if BSXINIT(int8NDArray, "int8", int8)
			      else if BSXINIT(int16NDArray, "int16", int16)
			      else if BSXINIT(int32NDArray, "int32", int32)
			      else if BSXINIT(int64NDArray, "int64", int64)
			      else if BSXINIT(uint8NDArray, "uint8", uint8)
			      else if BSXINIT(uint16NDArray, "uint16", uint16)
			      else if BSXINIT(uint32NDArray, "uint32", uint32)
			      else if BSXINIT(uint64NDArray, "uint64", uint64)
			      else
				{
				  C = tmp (0);
				  C = C.resize (dvc);
				}
			    }
			}
		      else
			{
			  update_index (ra_idx, dvc, i);
			  
			  if (have_NDArray)
			    {
			      if (tmp(0).class_name () != "double")
				{
				  have_NDArray = false;
				  C = result_NDArray;
				  C = do_cat_op (C, tmp(0), ra_idx);
				}
			      else if (tmp(0).is_real_type ())
				result_NDArray.insert (tmp(0).array_value(), 
						       ra_idx);
			      else
				{
				  result_ComplexNDArray = 
				    ComplexNDArray (result_NDArray);
				  result_ComplexNDArray.insert 
				    (tmp(0).complex_array_value(), ra_idx);
				  have_NDArray = false;
				  have_ComplexNDArray = true;
				}
			    }

#define BSXLOOP(T, CLS, EXTRACTOR) \
			(have_ ## T) \
			  { \
			    if (tmp (0).class_name () != CLS) \
			      { \
				have_ ## T = false; \
				C = result_ ## T; \
				C = do_cat_op (C, tmp (0), ra_idx); \
			      } \
			    else \
			      result_ ## T .insert \
				(tmp(0). EXTRACTOR ## _array_value (), \
				ra_idx); \
			  }

			  else if BSXLOOP(ComplexNDArray, "double", complex)
			  else if BSXLOOP(boolNDArray, "logical", bool)
			  else if BSXLOOP(int8NDArray, "int8", int8)
			  else if BSXLOOP(int16NDArray, "int16", int16)
			  else if BSXLOOP(int32NDArray, "int32", int32)
			  else if BSXLOOP(int64NDArray, "int64", int64)
			  else if BSXLOOP(uint8NDArray, "uint8", uint8)
			  else if BSXLOOP(uint16NDArray, "uint16", uint16)
			  else if BSXLOOP(uint32NDArray, "uint32", uint32)
			  else if BSXLOOP(uint64NDArray, "uint64", uint64)
			  else
			    C = do_cat_op (C, tmp(0), ra_idx);
			}
		    }

#define BSXEND(T) \
		  (have_ ## T) \
		    retval (0) = result_ ## T;

		  if BSXEND(NDArray)
		  else if BSXEND(ComplexNDArray)
		  else if BSXEND(boolNDArray)
		  else if BSXEND(int8NDArray)
		  else if BSXEND(int16NDArray)
		  else if BSXEND(int32NDArray)
		  else if BSXEND(int64NDArray)
		  else if BSXEND(uint8NDArray)
		  else if BSXEND(uint16NDArray)
		  else if BSXEND(uint32NDArray)
		  else if BSXEND(uint64NDArray)
		  else
		    retval(0) = C;
		}
	    }
	}

      if (! fcn_name.empty ())
	clear_function (fcn_name);
    }	

  return retval;
}

/*

%!shared a, b, c, f
%! a = randn (4, 4);
%! b = mean (a, 1);
%! c = mean (a, 2);
%! f = @minus;
%!error(bsxfun (f));
%!error(bsxfun (f, a));
%!error(bsxfun (a, b));
%!error(bsxfun (a, b, c));
%!error(bsxfun (f, a, b, c));
%!error(bsxfun (f, ones(4, 0), ones(4, 4)))
%!assert(bsxfun (f, ones(4, 0), ones(4, 1)), zeros(4, 0));
%!assert(bsxfun (f, ones(1, 4), ones(4, 1)), zeros(4, 4));
%!assert(bsxfun (f, a, b), a - repmat(b, 4, 1));
%!assert(bsxfun (f, a, c), a - repmat(c, 1, 4));
%!assert(bsxfun ("minus", ones(1, 4), ones(4, 1)), zeros(4, 4));

%!shared a, b, c, f
%! a = randn (4, 4);
%! a(1) *= 1i;
%! b = mean (a, 1);
%! c = mean (a, 2);
%! f = @minus;
%!error(bsxfun (f));
%!error(bsxfun (f, a));
%!error(bsxfun (a, b));
%!error(bsxfun (a, b, c));
%!error(bsxfun (f, a, b, c));
%!error(bsxfun (f, ones(4, 0), ones(4, 4)))
%!assert(bsxfun (f, ones(4, 0), ones(4, 1)), zeros(4, 0));
%!assert(bsxfun (f, ones(1, 4), ones(4, 1)), zeros(4, 4));
%!assert(bsxfun (f, a, b), a - repmat(b, 4, 1));
%!assert(bsxfun (f, a, c), a - repmat(c, 1, 4));
%!assert(bsxfun ("minus", ones(1, 4), ones(4, 1)), zeros(4, 4));

%!shared a, b, c, f
%! a = randn (4, 4);
%! a(end) *= 1i;
%! b = mean (a, 1);
%! c = mean (a, 2);
%! f = @minus;
%!error(bsxfun (f));
%!error(bsxfun (f, a));
%!error(bsxfun (a, b));
%!error(bsxfun (a, b, c));
%!error(bsxfun (f, a, b, c));
%!error(bsxfun (f, ones(4, 0), ones(4, 4)))
%!assert(bsxfun (f, ones(4, 0), ones(4, 1)), zeros(4, 0));
%!assert(bsxfun (f, ones(1, 4), ones(4, 1)), zeros(4, 4));
%!assert(bsxfun (f, a, b), a - repmat(b, 4, 1));
%!assert(bsxfun (f, a, c), a - repmat(c, 1, 4));
%!assert(bsxfun ("minus", ones(1, 4), ones(4, 1)), zeros(4, 4));

%!shared a, b, c, f
%! a = randn (4, 4);
%! b = a (1, :);
%! c = a (:, 1);
%! f = @(x, y) x == y;
%!error(bsxfun (f));
%!error(bsxfun (f, a));
%!error(bsxfun (a, b));
%!error(bsxfun (a, b, c));
%!error(bsxfun (f, a, b, c));
%!error(bsxfun (f, ones(4, 0), ones(4, 4)))
%!assert(bsxfun (f, ones(4, 0), ones(4, 1)), zeros(4, 0, "logical"));
%!assert(bsxfun (f, ones(1, 4), ones(4, 1)), ones(4, 4, "logical"));
%!assert(bsxfun (f, a, b), a == repmat(b, 4, 1));
%!assert(bsxfun (f, a, c), a == repmat(c, 1, 4));

%!shared a, b, c, d, f
%! a = randn (4, 4, 4);
%! b = mean (a, 1);
%! c = mean (a, 2);
%! d = mean (a, 3);
%! f = @minus;
%!error(bsxfun (f, ones([4, 0, 4]), ones([4, 4, 4])));
%!assert(bsxfun (f, ones([4, 0, 4]), ones([4, 1, 4])), zeros([4, 0, 4]));
%!assert(bsxfun (f, ones([4, 4, 0]), ones([4, 1, 1])), zeros([4, 4, 0]));
%!assert(bsxfun (f, ones([1, 4, 4]), ones([4, 1, 4])), zeros([4, 4, 4]));
%!assert(bsxfun (f, ones([4, 4, 1]), ones([4, 1, 4])), zeros([4, 4, 4]));
%!assert(bsxfun (f, ones([4, 1, 4]), ones([1, 4, 4])), zeros([4, 4, 4]));
%!assert(bsxfun (f, ones([4, 1, 4]), ones([1, 4, 1])), zeros([4, 4, 4]));
%!assert(bsxfun (f, a, b), a - repmat(b, [4, 1, 1]));
%!assert(bsxfun (f, a, c), a - repmat(c, [1, 4, 1]));
%!assert(bsxfun (f, a, d), a - repmat(d, [1, 1, 4]));
%!assert(bsxfun ("minus", ones([4, 0, 4]), ones([4, 1, 4])), zeros([4, 0, 4]));

%% The below is a very hard case to treat
%!assert(bsxfun (f, ones([4, 1, 4, 1]), ones([1, 4, 1, 4])), zeros([4, 4, 4, 4]));

*/
