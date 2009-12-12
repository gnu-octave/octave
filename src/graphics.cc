/*

Copyright (C) 2007, 2008, 2009 John W. Eaton

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

#include <cctype>
#include <cfloat>
#include <cstdlib>

#include <algorithm>
#include <list>
#include <map>
#include <set>
#include <string>
#include <sstream>

#include "file-ops.h"
#include "file-stat.h"

#include "cmd-edit.h"
#include "defun.h"
#include "display.h"
#include "error.h"
#include "graphics.h"
#include "input.h"
#include "ov.h"
#include "oct-obj.h"
#include "oct-map.h"
#include "ov-fcn-handle.h"
#include "parse.h"
#include "toplev.h"
#include "unwind-prot.h"

// forward declaration
static octave_value xget (const graphics_handle& h, const caseless_str& name);

static void
gripe_set_invalid (const std::string& pname)
{
  error ("set: invalid value for %s property", pname.c_str ());
}

// Check to see that PNAME matches just one of PNAMES uniquely.
// Return the full name of the match, or an empty caseless_str object
// if there is no match, or the match is ambiguous.

static caseless_str
validate_property_name (const std::string& who, const std::string& what,
			const std::set<std::string>& pnames,
			const caseless_str& pname)
{
  size_t len = pname.length ();
  std::set<std::string> matches;

  for (std::set<std::string>::const_iterator p = pnames.begin ();
       p != pnames.end (); p++)
    {
      if (pname.compare (*p, len))
	{
	  if (len == p->length ())
	    {
	      // Exact match.
	      return pname;
	    }

	  matches.insert (*p);
	}
    }

  size_t num_matches = matches.size ();

  if (num_matches == 0)
    {
      error ("%s: unknown %s property %s",
	     who.c_str (), what.c_str (), pname.c_str ());
    }
  else if (num_matches > 1)
    {
      string_vector sv (matches);

      std::ostringstream os;

      sv.list_in_columns (os);

      std::string match_list = os.str ();

      error ("%s: ambiguous %s property name %s; possible matches:\n\n%s",
	     who.c_str (), what.c_str (), pname.c_str (), match_list.c_str ());
    }
  else if (num_matches == 1)
    {
      // Exact match was handled above.

      std::string possible_match = *(matches.begin ());

      warning_with_id ("Octave:abbreviated-property-match",
		       "%s: allowing %s to match %s property %s",
		       who.c_str (), pname.c_str (), what.c_str (),
		       possible_match.c_str ());

      return possible_match;
    }

  return caseless_str ();
}

static Matrix
jet_colormap (void)
{
  Matrix cmap (64, 3, 0.0);

  for (octave_idx_type i = 0; i < 64; i++)
    {
      // This is the jet colormap.  It would be nice to be able
      // to feval the jet function but since there is a static
      // property object that includes a colormap_property
      // object, we need to initialize this before main is even
      // called, so calling an interpreted function is not
      // possible.

      double x = i / 63.0;

      if (x >= 3.0/8.0 && x < 5.0/8.0)
        cmap(i,0) = 4.0 * x - 3.0/2.0;
      else if (x >= 5.0/8.0 && x < 7.0/8.0)
        cmap(i,0) = 1.0;
      else if (x >= 7.0/8.0)
        cmap(i,0) = -4.0 * x + 9.0/2.0;

      if (x >= 1.0/8.0 && x < 3.0/8.0)
        cmap(i,1) = 4.0 * x - 1.0/2.0;
      else if (x >= 3.0/8.0 && x < 5.0/8.0)
        cmap(i,1) = 1.0;
      else if (x >= 5.0/8.0 && x < 7.0/8.0)
        cmap(i,1) = -4.0 * x + 7.0/2.0;

      if (x < 1.0/8.0)
        cmap(i,2) = 4.0 * x + 1.0/2.0;
      else if (x >= 1.0/8.0 && x < 3.0/8.0)
        cmap(i,2) = 1.0;
      else if (x >= 3.0/8.0 && x < 5.0/8.0)
        cmap(i,2) = -4.0 * x + 5.0/2.0;
    }

  return cmap;
}

static double
default_screendepth (void)
{
  return display_info::depth ();
}

static Matrix
default_screensize (void)
{
  Matrix retval (1, 4, 1.0);

  retval(2) = display_info::width ();
  retval(3) = display_info::height ();

  return retval;
}

static double
default_screenpixelsperinch (void)
{
  return (display_info::x_dpi () + display_info::y_dpi ()) / 2;
}

static Matrix
default_colororder (void)
{
  Matrix retval (7, 3, 0.0);

  retval(0,2) = 1.0;

  retval(1,1) = 0.5;

  retval(2,0) = 1.0;

  retval(3,1) = 0.75;
  retval(3,2) = 0.75;

  retval(4,0) = 0.75;
  retval(4,2) = 0.75;

  retval(5,0) = 0.75;
  retval(5,1) = 0.75;

  retval(6,0) = 0.25;
  retval(6,1) = 0.25;
  retval(6,2) = 0.25;

  return retval;
}

static Matrix
default_lim (void)
{
  Matrix m (1, 2, 0);
  m(1) = 1;
  return m;
}

static Matrix
default_data (void)
{
  Matrix retval (1, 2);

  retval(0) = 0;
  retval(1) = 1;

  return retval;
}

static Matrix
default_axes_position (void)
{
  Matrix m (1, 4, 0.0);
  m(0) = 0.13;
  m(1) = 0.11;
  m(2) = 0.775;
  m(3) = 0.815;
  return m;
}

static Matrix
default_axes_outerposition (void)
{
  Matrix m (1, 4, 0.0);
  m(2) = m(3) = 1.0;
  return m;
}

static Matrix
default_axes_tick (void)
{
  Matrix m (1, 6, 0.0);
  m(0) = 0.0;
  m(1) = 0.2;
  m(2) = 0.4;
  m(3) = 0.6;
  m(4) = 0.8;
  m(5) = 1.0;
  return m;
}

static Matrix
default_axes_ticklength (void)
{
  Matrix m (1, 2, 0.01);
  m(1) = 0.025;
  return m;
}

static Matrix
default_figure_position (void)
{
  Matrix m (1, 4, 0.0);
  m(0) = 300;
  m(1) = 200;
  m(2) = 560;
  m(3) = 420;
  return m;
}

static Matrix
default_figure_papersize (void)
{
  Matrix m (1, 2, 0.0);
  m(0) = 8.5;
  m(1) = 11.0;
  return m;
}

static Matrix
default_figure_paperposition (void)
{
  Matrix m (1, 4, 0.0);
  m(0) = 0.25;
  m(1) = 2.50;
  m(2) = 8.00;
  m(3) = 6.00;
  return m;
}

static Matrix
convert_position (const Matrix& pos, const caseless_str& from_units,
		  const caseless_str& to_units,
		  const Matrix& parent_dim = Matrix (1, 2, 0.0))
{
  Matrix retval (1, 4);
  double res = 0;

  if (from_units.compare ("pixels"))
    retval = pos;
  else if (from_units.compare ("normalized"))
    {
      retval(0) = pos(0) * parent_dim(0) + 1;
      retval(1) = pos(1) * parent_dim(1) + 1;
      retval(2) = pos(2) * parent_dim(0);
      retval(3) = pos(3) * parent_dim(1);
    }
  else if (from_units.compare ("characters"))
    {
      if (res <= 0)
	res = xget (0, "screenpixelsperinch").double_value ();

      double f = 0.0;

      // FIXME -- this assumes the system font is Helvetica 10pt 
      //          (for which "x" requires 6x12 pixels at 74.951 pixels/inch)
      f = 12.0 * res / 74.951;

      if (f > 0)
	{
	  retval(0) = 0.5 * pos(0) * f;
	  retval(1) = pos(1) * f;
	  retval(2) = 0.5 * pos(2) * f;
	  retval(3) = pos(3) * f;
	}
    }
  else
    {
      if (res <= 0)
	res = xget (0, "screenpixelsperinch").double_value ();

      double f = 0.0;

      if (from_units.compare ("points"))
	f = res / 72.0;
      else if (from_units.compare ("inches"))
	f = res;
      else if (from_units.compare ("centimeters"))
	f = res / 2.54;

      if (f > 0)
	{
	  retval(0) = pos(0) * f + 1;
	  retval(1) = pos(1) * f + 1;
	  retval(2) = pos(2) * f;
	  retval(3) = pos(3) * f;
	}
    }

  if (! to_units.compare ("pixels"))
    {
      if (to_units.compare ("normalized"))
	{
	  retval(0) = (retval(0) - 1) / parent_dim(0);
	  retval(1) = (retval(1) - 1) / parent_dim(1);
	  retval(2) /= parent_dim(0);
	  retval(3) /= parent_dim(1);
	}
      else if (to_units.compare ("characters"))
	{
	  if (res <= 0)
	    res = xget (0, "screenpixelsperinch").double_value ();

	  double f = 0.0;

	  f = 12.0 * res / 74.951;

	  if (f > 0)
	    {
	      retval(0) = 2 * retval(0) / f;
	      retval(1) = retval(1) / f;
	      retval(2) = 2 * retval(2) / f;
	      retval(3) = retval(3) / f;
	    }
	}
      else
	{
	  if (res <= 0)
	    res = xget (0, "screenpixelsperinch").double_value ();

	  double f = 0.0;

	  if (to_units.compare ("points"))
	    f = res / 72.0;
	  else if (to_units.compare ("inches"))
	    f = res;
	  else if (to_units.compare ("centimeters"))
	    f = res / 2.54;

	  if (f > 0)
	    {
	      retval(0) = (retval(0) - 1) / f;
	      retval(1) = (retval(1) - 1) / f;
	      retval(2) /= f;
	      retval(3) /= f;
	    }
	}
    }

  return retval;
}

static graphics_object
xget_ancestor (const graphics_object& go_arg, const std::string& type)
{
  graphics_object go = go_arg;

  do
    {
      if (go.valid_object ())
	{
	  if (go.isa (type))
	    return go;
	  else
	    go = gh_manager::get_object (go.get_parent ());
	}
      else
	return graphics_object ();
    }
 while (true);
}

static octave_value
convert_cdata (const base_properties& props, const octave_value& cdata,
	       bool is_scaled, int cdim)
{
  dim_vector dv (cdata.dims ());

  if (dv.length () == cdim && dv(cdim-1) == 3)
    return cdata;

  Matrix cmap (1, 3, 0.0);
  Matrix clim (1, 2, 0.0);

  graphics_object go = gh_manager::get_object (props.get___myhandle__ ());
  graphics_object fig = xget_ancestor (go, "figure");

  if (fig.valid_object ())
    {
      Matrix _cmap = fig.get (caseless_str ("colormap")).matrix_value ();

      if (! error_state)
	cmap = _cmap;
    }

  if (is_scaled)
    {
      graphics_object ax = xget_ancestor (go, "axes");

      if (ax.valid_object ())
	{
	  Matrix _clim = ax.get (caseless_str ("clim")).matrix_value ();

	  if (! error_state)
	    clim = _clim;
	}
    }

  dv.resize (cdim);
  dv(cdim-1) = 3;

  NDArray a (dv);

  octave_idx_type lda = a.numel () / static_cast<octave_idx_type> (3);
  octave_idx_type nc = cmap.rows ();

  double *av = a.fortran_vec ();
  const double *cmapv = cmap.data ();
  const double *cv = 0;
  const octave_uint8 *icv = 0;

  if (cdata.is_integer_type ())
    icv = cdata.uint8_array_value ().data ();
  else
    cv = cdata.array_value ().data ();

  for (octave_idx_type i = 0; i < lda; i++)
    {
      double x = (cv ? cv[i] : double (icv[i]));

      if (is_scaled)
	x = xround ((nc - 1) * (x - clim(0)) / (clim(1) - clim(0)));
      else
	x = xround (x - 1);

      if (xisnan (x))
	{
	  av[i]       = x;
	  av[i+lda]   = x;
	  av[i+2*lda] = x;
	}
      else
	{
	  if (x < 0)
	    x = 0;
	  else if (x >= nc)
	    x = (nc - 1);

	  octave_idx_type idx = static_cast<octave_idx_type> (x);

	  av[i]       = cmapv[idx];
	  av[i+lda]   = cmapv[idx+nc];
	  av[i+2*lda] = cmapv[idx+2*nc];
	}
    }

  return octave_value (a);
}

template<class T>
static void
get_array_limits (const Array<T>& m, double& emin, double& emax,
		  double& eminp)
{
  const T *data = m.data ();
  octave_idx_type n = m.numel ();

  for (octave_idx_type i = 0; i < n; i++)
    {
      double e = double (data[i]);

      if (! (xisinf (e) || xisnan (e)))
	{
	  if (e < emin)
	    emin = e;

	  if (e > emax)
	    emax = e;

	  if (e > 0 && e < eminp)
	    eminp = e;
	}
    }
}

static bool
lookup_object_name (const caseless_str& name, caseless_str& go_name,
		    caseless_str& rest)
{
  int len = name.length ();
  int offset = 0;
  bool result = false;

  if (len >= 4)
    {
      caseless_str pfx = name.substr (0, 4);

      if (pfx.compare ("axes") || pfx.compare ("line")
	  || pfx.compare ("text"))
	offset = 4;
      else if (len >= 5)
	{
	  pfx = name.substr (0, 5);

	  if (pfx.compare ("image") || pfx.compare ("patch"))
	    offset = 5;
	  else if (len >= 6)
	    {
	      pfx = name.substr (0, 6);

	      if (pfx.compare ("figure"))
		offset = 6;
	      else if (len >= 7)
		{
		  pfx = name.substr (0, 7);

		  if (pfx.compare ("surface") || pfx.compare ("hggroup"))
		    offset = 7;
		}
	    }
	}

      if (offset > 0)
	{
	  go_name = pfx;
	  rest = name.substr (offset);
	  result = true;
	}
    }

  return result;
}

static base_graphics_object*
make_graphics_object_from_type (const caseless_str& type,
				const graphics_handle& h = graphics_handle (),
				const graphics_handle& p = graphics_handle ())
{
  base_graphics_object *go = 0;

  if (type.compare ("figure"))
    go = new figure (h, p);
  else if (type.compare ("axes"))
    go = new axes (h, p);
  else if (type.compare ("line"))
    go = new line (h, p);
  else if (type.compare ("text"))
    go = new text (h, p);
  else if (type.compare ("image"))
    go = new image (h, p);
  else if (type.compare ("patch"))
    go = new patch (h, p);
  else if (type.compare ("surface"))
    go = new surface (h, p);
  else if (type.compare ("hggroup"))
    go = new hggroup (h, p);

  return go;
}

// ---------------------------------------------------------------------

bool
base_property::set (const octave_value& v, bool do_run )
{
  if (do_set (v))
    {

      // notify backend
      if (id >= 0)
	{
	  graphics_object go = gh_manager::get_object (parent);
	  if (go)
	    {
	      graphics_backend backend = go.get_backend();
	      if (backend)
		backend.property_changed (go, id);
	    }
	}

      // run listeners
      if (do_run && ! error_state)
	run_listeners (POSTSET);

      return true;
    }

  return false;
}


void
base_property::run_listeners (listener_mode mode)
{
  const octave_value_list& l = listeners[mode];

  for (int i = 0; i < l.length (); i++)
    {
      gh_manager::execute_callback (parent, l(i), octave_value ());

      if (error_state)
	break;
    }
}

radio_values::radio_values (const std::string& opt_string)
{
  size_t beg = 0;
  size_t len = opt_string.length ();
  bool done = len == 0;

  while (! done)
    {
      size_t end = opt_string.find ('|', beg);

      if (end == std::string::npos)
	{
	  end = len;
	  done = true;
	}

      std::string t = opt_string.substr (beg, end-beg);

      // Might want more error checking here...
      if (t[0] == '{')
	{
	  t = t.substr (1, t.length () - 2);
	  default_val = t;
	}
      else if (beg == 0) // ensure default value
	default_val = t;

      possible_vals.insert (t);

      beg = end + 1;
    }
}

bool
color_values::str2rgb (std::string str)
{
  double tmp_rgb[3] = {0, 0, 0};
  bool retval = true;
  unsigned int len = str.length();

  std::transform (str.begin (), str.end (), str.begin (), tolower);

  if (str.compare(0, len, "blue", 0, len) == 0)
    tmp_rgb[2] = 1;
  else if (str.compare(0, len, "black", 0, len) == 0
	   || str.compare(0, len, "k", 0, len) == 0)
    tmp_rgb[0] = tmp_rgb[1] = tmp_rgb[2] = 0;
  else if (str.compare(0, len, "red", 0, len) == 0)
    tmp_rgb[0] = 1;
  else if (str.compare(0, len, "green", 0, len) == 0)
    tmp_rgb[1] = 1;
  else if (str.compare(0, len, "yellow", 0, len) == 0)
    tmp_rgb[0] = tmp_rgb[1] = 1;
  else if (str.compare(0, len, "magenta", 0, len) == 0)
    tmp_rgb[0] = tmp_rgb[2] = 1;
  else if (str.compare(0, len, "cyan", 0, len) == 0)
    tmp_rgb[1] = tmp_rgb[2] = 1;
  else if (str.compare(0, len, "white", 0, len) == 0
	   || str.compare(0, len, "w", 0, len) == 0)
    tmp_rgb[0] = tmp_rgb[1] = tmp_rgb[2] = 1;
  else	
    retval = false;

  if (retval)
    {
      for (int i = 0; i < 3; i++)
	xrgb(i) = tmp_rgb[i];
    }

  return retval;
}

bool
color_property::do_set (const octave_value& val)
{
  if (val.is_string ())
    {
      std::string s = val.string_value ();

      if (! s.empty ())
	{
	  if (radio_val.contains (s))
	    {
	      if (current_type != radio_t || current_val != s)
		{
		  current_val = s;
		  current_type = radio_t;
		  return true;
		}
	    }
          else
	    {
	      color_values col (s);
	      if (! error_state)
		{
		  if (current_type != color_t || col != color_val)
		    {
		      color_val = col;
		      current_type = color_t;
		      return true;
		    }
		}
	      else
		error ("invalid value for color property \"%s\" (value = %s)",
		       get_name ().c_str (), s.c_str ());
	    }	
	}
      else
	error ("invalid value for color property \"%s\"",
           get_name ().c_str ());
    }
  else if (val.is_numeric_type ())
    {
      Matrix m = val.matrix_value ();

      if (m.numel () == 3)
	{
	  color_values col (m (0), m (1), m(2));
	  if (! error_state)
	    {
	      if (current_type != color_t || col != color_val)
		{
		  color_val = col;
		  current_type = color_t;
		  return true;
		}
	    }
	}
      else
	error ("invalid value for color property \"%s\"",
           get_name ().c_str ());
    }
  else 
    error ("invalid value for color property \"%s\"",
           get_name ().c_str ());

  return false;
}

bool
double_radio_property::do_set (const octave_value& val)
{
  if (val.is_string ())
    {
      std::string s = val.string_value ();

      if (! s.empty () && radio_val.contains (s))
	{
	  if (current_type != radio_t || s != current_val)
	    {
	      current_val = s;
	      current_type = radio_t;
	      return true;
	    }
	}
      else
	error ("invalid value for double_radio property \"%s\"",
	       get_name ().c_str ());
    }
  else if (val.is_scalar_type () && val.is_real_type ())
    {
      double new_dval = val.double_value ();

      if (current_type != double_t || new_dval != dval)
	{
	  dval = new_dval;
	  current_type = double_t;
	  return true;
	}
    }
  else 
    error ("invalid value for double_radio property \"%s\"",
	   get_name ().c_str ());

  return false;
}

bool
array_property::validate (const octave_value& v)
{
  bool xok = false;

  // FIXME -- should we always support []?
  if (v.is_empty () && v.is_numeric_type ())
    return true;

  // check value type
  if (type_constraints.size () > 0)
    {
      for (std::list<std::string>::const_iterator it = type_constraints.begin ();
           ! xok && it != type_constraints.end (); ++it)
        if ((*it) == v.class_name ())
          xok = true;
    }
  else
    xok = v.is_numeric_type ();

  if (xok)
    {
      dim_vector vdims = v.dims ();
      int vlen = vdims.length ();

      xok = false;

      // check value size
      if (size_constraints.size () > 0)
        for (std::list<dim_vector>::const_iterator it = size_constraints.begin ();
             ! xok && it != size_constraints.end (); ++it)
          {
            dim_vector itdims = (*it);

            if (itdims.length () == vlen)
              {
                xok = true;

                for (int i = 0; xok && i < vlen; i++)
                  if (itdims(i) >= 0 && itdims(i) != vdims(i))
                    xok = false;
              }
          }
      else
        return true;
    }

  return xok;
}

bool
array_property::is_equal (const octave_value& v) const
{
  if (data.type_name () == v.type_name ())
    {
      if (data.dims () == v.dims ())
	{

#define CHECK_ARRAY_EQUAL(T,F,A) \
	    { \
	      if (data.numel () == 1) \
		return data.F ## scalar_value () == \
		  v.F ## scalar_value (); \
	      else  \
		{ \
                  /* Keep copy of array_value to allow sparse/bool arrays */ \
		  /* that are converted, to not be deallocated early */ \
		  const A m1 = data.F ## array_value (); \
		  const T* d1 = m1.data (); \
		  const A m2 = v.F ## array_value (); \
		  const T* d2 = m2.data ();\
		  \
		  bool flag = true; \
		  \
		  for (int i = 0; flag && i < data.numel (); i++) \
		    if (d1[i] != d2[i]) \
		      flag = false; \
		  \
		  return flag; \
		} \
	    }

	  if (data.is_double_type() || data.is_bool_type ())
	    CHECK_ARRAY_EQUAL (double, , NDArray)
	  else if (data.is_single_type ())
	    CHECK_ARRAY_EQUAL (float, float_, FloatNDArray)
	  else if (data.is_int8_type ())
	    CHECK_ARRAY_EQUAL (octave_int8, int8_, int8NDArray)
	  else if (data.is_int16_type ())
	    CHECK_ARRAY_EQUAL (octave_int16, int16_, int16NDArray)
	  else if (data.is_int32_type ())
	    CHECK_ARRAY_EQUAL (octave_int32, int32_, int32NDArray)
	  else if (data.is_int64_type ())
	    CHECK_ARRAY_EQUAL (octave_int64, int64_, int64NDArray)
	  else if (data.is_uint8_type ())
	    CHECK_ARRAY_EQUAL (octave_uint8, uint8_, uint8NDArray)
	  else if (data.is_uint16_type ())
	    CHECK_ARRAY_EQUAL (octave_uint16, uint16_, uint16NDArray)
	  else if (data.is_uint32_type ())
	    CHECK_ARRAY_EQUAL (octave_uint32, uint32_, uint32NDArray)
	  else if (data.is_uint64_type ())
	    CHECK_ARRAY_EQUAL (octave_uint64, uint64_, uint64NDArray)
	}
    }

  return false;
}

void
array_property::get_data_limits (void)
{
  xmin = xminp = octave_Inf;
  xmax = -octave_Inf;

  if (! data.is_empty ())
    {
      if (data.is_integer_type ())
	{
	  if (data.is_int8_type ())
	    get_array_limits (data.int8_array_value (), xmin, xmax, xminp);
	  else if (data.is_uint8_type ())
	    get_array_limits (data.uint8_array_value (), xmin, xmax, xminp);
	  else if (data.is_int16_type ())
	    get_array_limits (data.int16_array_value (), xmin, xmax, xminp);
	  else if (data.is_uint16_type ())
	    get_array_limits (data.uint16_array_value (), xmin, xmax, xminp);
	  else if (data.is_int32_type ())
	    get_array_limits (data.int32_array_value (), xmin, xmax, xminp);
	  else if (data.is_uint32_type ())
	    get_array_limits (data.uint32_array_value (), xmin, xmax, xminp);
	  else if (data.is_int64_type ())
	    get_array_limits (data.int64_array_value (), xmin, xmax, xminp);
	  else if (data.is_uint64_type ())
	    get_array_limits (data.uint64_array_value (), xmin, xmax, xminp);
	}
      else
	get_array_limits (data.array_value (), xmin, xmax, xminp);
    }
}

bool
handle_property::do_set (const octave_value& v)
{
  double dv = v.double_value ();

  if (! error_state)
    {
      graphics_handle gh = gh_manager::lookup (dv);

      if (xisnan (gh.value ()) || gh.ok ())
	{
	  if (current_val != gh)
	    {
	      current_val = gh;
	      return true;
	    }
	}
      else
        error ("set: invalid graphics handle (= %g) for property \"%s\"",
	       dv, get_name ().c_str ());
    }
  else
    error ("set: invalid graphics handle for property \"%s\"",
	   get_name ().c_str ());

  return false;
}

bool
callback_property::validate (const octave_value& v) const
{
  // case 1: function handle
  // case 2: cell array with first element being a function handle
  // case 3: string corresponding to known function name
  // case 4: evaluatable string
  // case 5: empty matrix

  if (v.is_function_handle ())
    return true;
  else if (v.is_string ())
    // complete validation will be done at execution-time
    return true;
  else if (v.is_cell () && v.length () > 0
           && (v.rows() == 1 || v.columns () == 1)
           && v.cell_value ()(0).is_function_handle ())
    return true;
  else if (v.is_empty ())
    return true;

  return false;
}

void
callback_property::execute (const octave_value& data) const
{
  if (callback.is_defined () && ! callback.is_empty ())
    gh_manager::execute_callback (get_parent (), callback, data);
}

// Used to cache dummy graphics objects from which dynamic
// properties can be cloned.
static std::map<caseless_str, graphics_object> dprop_obj_map;

property
property::create (const std::string& name, const graphics_handle& h,
		  const caseless_str& type, const octave_value_list& args)
{
  property retval;

  if (type.compare ("string"))
    {
      std::string val = (args.length () > 0 ? args(0).string_value () : "");

      if (! error_state)
	retval = property (new string_property (name, h, val));
    }
  else if (type.compare ("any"))
    {
      octave_value val =
	  (args.length () > 0 ? args(0) : octave_value (Matrix ()));

      retval = property (new any_property (name, h, val));
    }
  else if (type.compare ("radio"))
    {
      if (args.length () > 0)
	{
	  std::string vals = args(0).string_value ();

	  if (! error_state)
	    {
	      retval = property (new radio_property (name, h, vals));

	      if (args.length () > 1)
		retval.set (args(1));
	    }
	  else
	    error ("addproperty: invalid argument for radio property, expected a string value");
	}
      else
	error ("addproperty: missing possible values for radio property");
    }
  else if (type.compare ("double"))
    {
      double d = (args.length () > 0 ? args(0).double_value () : 0);

      if (! error_state)
	retval = property (new double_property (name, h, d));
    }
  else if (type.compare ("handle"))
    {
      double hh = (args.length () > 0 ? args(0).double_value () : octave_NaN);

      if (! error_state)
	{
	  graphics_handle gh (hh);

	  retval = property (new handle_property (name, h, gh));
	}
    }
  else if (type.compare ("boolean"))
    {
      retval = property (new bool_property (name, h, false));

      if (args.length () > 0)
	retval.set (args(0));
    }
  else if (type.compare ("data"))
    {
      retval = property (new array_property (name, h, Matrix ()));

      if (args.length () > 0)
	{
	  retval.set (args(0));

	  // FIXME -- additional argument could define constraints,
	  // but is this really useful?
	}
    }
  else if (type.compare ("color"))
    {
      color_values cv (0, 0, 0);
      radio_values rv;

      if (args.length () > 1)
	rv = radio_values (args(1).string_value ());

      if (! error_state)
	{
	  retval = property (new color_property (name, h, cv, rv));

	  if (! error_state)
	    {
	      if (args.length () > 0 && ! args(0).is_empty ())
		retval.set (args(0));
	      else
		retval.set (rv.default_value ());
	    }
	}
    }
  else
    {
      caseless_str go_name, go_rest;

      if (lookup_object_name (type, go_name, go_rest))
	{
	  graphics_object go;

	  std::map<caseless_str, graphics_object>::const_iterator it =
	      dprop_obj_map.find (go_name);

	  if (it == dprop_obj_map.end ())
	    {
	      base_graphics_object *bgo =
		  make_graphics_object_from_type (go_name);

	      if (bgo)
		{
		  go = graphics_object (bgo);

		  dprop_obj_map[go_name] = go;
		}
	    }
	  else
	    go = it->second;

	  if (go.valid_object ())
	    {
	      property prop = go.get_properties ().get_property (go_rest);

	      if (! error_state)
		{
		  retval = prop.clone ();

		  retval.set_parent (h);
		  retval.set_name (name);

		  if (args.length () > 0)
		    retval.set (args(0));
		}
	    }
	  else
	    error ("addproperty: invalid object type (= %s)",
		   go_name.c_str ());
	}
      else
	error ("addproperty: unsupported type for dynamic property (= %s)",
	       type.c_str ());
    }
  
  return retval;
}

// ---------------------------------------------------------------------

void
property_list::set (const caseless_str& name, const octave_value& val)
{
  size_t offset = 0;

  size_t len = name.length ();

  if (len > 4)
    {
      caseless_str pfx = name.substr (0, 4);

      if (pfx.compare ("axes") || pfx.compare ("line")
	  || pfx.compare ("text"))
	offset = 4;
      else if (len > 5)
	{
	  pfx = name.substr (0, 5);

	  if (pfx.compare ("image") || pfx.compare ("patch"))
	    offset = 5;
	  else if (len > 6)
	    {
	      pfx = name.substr (0, 6);

	      if (pfx.compare ("figure"))
		offset = 6;
	      else if (len > 7)
		{
		  pfx = name.substr (0, 7);

		  if (pfx.compare ("surface") || pfx.compare ("hggroup"))
		    offset = 7;
		}
	    }
	}

      if (offset > 0)
	{
	  // FIXME -- should we validate property names and values here?

	  std::string pname = name.substr (offset);

	  std::transform (pfx.begin (), pfx.end (), pfx.begin (), tolower);
	  std::transform (pname.begin (), pname.end (), pname.begin (), tolower);

	  bool has_property = false;
	  if (pfx == "axes")
	    has_property = axes::properties::has_core_property (pname);
	  else if (pfx == "line")
	    has_property = line::properties::has_core_property (pname);
	  else if (pfx == "text")
	    has_property = text::properties::has_core_property (pname);
	  else if (pfx == "image")
	    has_property = image::properties::has_core_property (pname);
	  else if (pfx == "patch")
	    has_property = patch::properties::has_core_property (pname);
	  else if (pfx == "figure")
	    has_property = figure::properties::has_core_property (pname);
	  else if (pfx == "surface")
	    has_property = surface::properties::has_core_property (pname);
	  else if (pfx == "hggroup")
	    has_property = hggroup::properties::has_core_property (pname);

	  if (has_property)
	    {
	      bool remove = false;
	      if (val.is_string ())
		{
		  caseless_str tval = val.string_value ();

		  remove = tval.compare ("remove");
		}

	      pval_map_type& pval_map = plist_map[pfx];

	      if (remove)
		{
		  pval_map_iterator p = pval_map.find (pname);

		  if (p != pval_map.end ())
		    pval_map.erase (p);
		}
	      else
		pval_map[pname] = val;
	    }
	  else
	    error ("invalid %s property `%s'", pfx.c_str (), pname.c_str ());
	}
    }

  if (! error_state && offset == 0)
    error ("invalid default property specification");
}

octave_value
property_list::lookup (const caseless_str& name) const
{
  octave_value retval;

  size_t offset = 0;

  size_t len = name.length ();

  if (len > 4)
    {
      caseless_str pfx = name.substr (0, 4);

      if (pfx.compare ("axes") || pfx.compare ("line")
	  || pfx.compare ("text"))
	offset = 4;
      else if (len > 5)
	{
	  pfx = name.substr (0, 5);

	  if (pfx.compare ("image") || pfx.compare ("patch"))
	    offset = 5;
	  else if (len > 6)
	    {
	      pfx = name.substr (0, 6);

	      if (pfx.compare ("figure"))
		offset = 6;
	      else if (len > 7)
		{
		  pfx = name.substr (0, 7);

		  if (pfx.compare ("surface") || pfx.compare ("hggroup"))
		    offset = 7;
		}
	    }
	}

      if (offset > 0)
	{
	  std::string pname = name.substr (offset);

	  std::transform (pfx.begin (), pfx.end (), pfx.begin (), tolower);
	  std::transform (pname.begin (), pname.end (), pname.begin (), tolower);

	  plist_map_const_iterator p = find (pfx);

	  if (p != end ())
	    {
	      const pval_map_type& pval_map = p->second;

	      pval_map_const_iterator q = pval_map.find (pname);

	      if (q != pval_map.end ())
		retval = q->second;
	    }
	}
    }

  return retval;
}

Octave_map
property_list::as_struct (const std::string& prefix_arg) const
{
  Octave_map m;

  for (plist_map_const_iterator p = begin (); p != end (); p++)
    {
      std::string prefix = prefix_arg + p->first;

      const pval_map_type pval_map = p->second;

      for (pval_map_const_iterator q = pval_map.begin ();
	   q != pval_map.end ();
	   q++)
	m.assign (prefix + q->first, q->second);
    }

  return m;    
}

graphics_handle::graphics_handle (const octave_value& a)
  : val (octave_NaN)
{
  if (a.is_empty ())
    /* do nothing */;
  else
    {
      double tval = a.double_value ();

      if (! error_state)
	val = tval;
      else
	error ("invalid graphics handle");
    }
}

void
graphics_object::set (const octave_value_list& args)
{
  int nargin = args.length ();

  if (nargin == 0)
    rep->defaults ();
  else if (nargin % 2 == 0)
    {
      for (int i = 0; i < nargin; i += 2)
	{
	  caseless_str name = args(i).string_value ();

	  if (! error_state)
	    {
	      octave_value val = args(i+1);

	      if (val.is_string ())
		{
		  caseless_str tval = val.string_value ();

		  if (tval.compare ("default"))
		    val = get_default (name);
		  else if (tval.compare ("factory"))
		    val = get_factory_default (name);
		}

	      if (error_state)
		break;

	      rep->set (name, val);
	    }
	  else
	    error ("set: expecting argument %d to be a property name", i);
	}
    }
  else
    error ("set: invalid number of arguments");
}

static double
make_handle_fraction (void)
{
  static double maxrand = RAND_MAX + 2.0;

  return (rand () + 1.0) / maxrand;
}

graphics_handle
gh_manager::get_handle (const std::string& go_name)
{
  graphics_handle retval;

  if (go_name == "figure")
    {
      // Figure handles are positive integers corresponding to the
      // figure number.

      // We always want the lowest unused figure number.

      retval = 1;

      while (handle_map.find (retval) != handle_map.end ())
	retval++;
    }
  else
    {
      // Other graphics handles are negative integers plus some random
      // fractional part.  To avoid running out of integers, we
      // recycle the integer part but tack on a new random part each
      // time.

      free_list_iterator p = handle_free_list.begin ();

      if (p != handle_free_list.end ())
	{
	  retval = *p;
	  handle_free_list.erase (p);
	}
      else
	{
	  retval = graphics_handle (next_handle);

	  next_handle = ceil (next_handle) - 1.0 - make_handle_fraction ();
	}
    }

  return retval;
}

void
gh_manager::do_free (const graphics_handle& h)
{
  if (h.ok ())
    {
      if (h.value () != 0)
	{
	  iterator p = handle_map.find (h);

	  if (p != handle_map.end ())
	    {
	      base_properties& bp = p->second.get_properties ();
	      
	      bp.set_beingdeleted (true);

	      bp.delete_children ();

	      octave_value val = bp.get_deletefcn ();

	      bp.execute_deletefcn ();

	      // notify backend
	      graphics_backend backend = p->second.get_backend ();
	      if (backend)
                backend.object_destroyed (p->second);

	      // Note: this will be valid only for first explicitly 
	      // deleted object.  All its children will then have an
	      // unknown backend.

	      // Graphics handles for non-figure objects are negative
	      // integers plus some random fractional part.  To avoid
	      // running out of integers, we recycle the integer part
	      // but tack on a new random part each time.

	      handle_map.erase (p);

	      if (h.value () < 0)
		handle_free_list.insert (ceil (h.value ()) - make_handle_fraction ());
	    }
	  else
	    error ("graphics_handle::free: invalid object %g", h.value ());
	}
      else
	error ("graphics_handle::free: can't delete root figure");
    }
}

gh_manager *gh_manager::instance = 0;

static void
xset (const graphics_handle& h, const caseless_str& name,
      const octave_value& val)
{
  graphics_object obj = gh_manager::get_object (h);
  obj.set (name, val);
}

static void
xset (const graphics_handle& h, const octave_value_list& args)
{
  if (args.length () > 0)
    {
      graphics_object obj = gh_manager::get_object (h);
      obj.set (args);
    }
}


static octave_value
xget (const graphics_handle& h, const caseless_str& name)
{
  graphics_object obj = gh_manager::get_object (h);
  return obj.get (name);
}

static graphics_handle
reparent (const octave_value& ov, const std::string& who,
	  const std::string& property, const graphics_handle& new_parent,
	  bool adopt = true)
{
  graphics_handle h = octave_NaN;

  double val = ov.double_value ();

  if (! error_state)
    {
      h = gh_manager::lookup (val);

      if (h.ok ())
	{
	  graphics_object obj = gh_manager::get_object (h);
	  
	  graphics_handle parent_h = obj.get_parent ();

	  graphics_object parent_obj = gh_manager::get_object (parent_h);

	  parent_obj.remove_child (h);

	  if (adopt)
	    obj.set ("parent", new_parent.value ());
	  else
	    obj.reparent (new_parent);
	}
      else
	error ("%s: invalid graphics handle (= %g) for %s",
	       who.c_str (), val, property.c_str ());
    }
  else
    error ("%s: expecting %s to be a graphics handle",
	   who.c_str (), property.c_str ());

  return h;
}

// This function is NOT equivalent to the scripting language function gcf.
graphics_handle
gcf (void)
{
  octave_value val = xget (0, "currentfigure");

  return val.is_empty () ? octave_NaN : val.double_value ();
}

// This function is NOT equivalent to the scripting language function gca.
graphics_handle
gca (void)
{
  octave_value val = xget (gcf (), "currentaxes");

  return val.is_empty () ? octave_NaN : val.double_value ();
}

static void
adopt (const graphics_handle& p, const graphics_handle& h)
{
  graphics_object parent_obj = gh_manager::get_object (p);

  parent_obj.adopt (h);
}

static bool
is_handle (const graphics_handle& h)
{
  return h.ok ();
}

static bool
is_handle (double val)
{
  graphics_handle h = gh_manager::lookup (val);

  return h.ok ();
}

static octave_value
is_handle (const octave_value& val)
{
  octave_value retval = false;

  if (val.is_real_scalar () && is_handle (val.double_value ()))
    retval = true;
  else if (val.is_real_matrix ())
    {
      if (val.is_string ())
	retval = boolNDArray (val.dims (), false);
      else
	{
	  const NDArray handles = val.array_value ();

	  if (! error_state)
	    {
	      boolNDArray result (handles.dims ());

	      for (octave_idx_type i = 0; i < handles.numel (); i++)
		result.xelem (i) = is_handle (handles (i));

	      retval = result;
	    }
	}
    }

  return retval;
}

static bool
is_figure (double val)
{
  graphics_object obj = gh_manager::get_object (val);

  return obj && obj.isa ("figure");
}

static void
xcreatefcn (const graphics_handle& h)
{
  graphics_object obj = gh_manager::get_object (h);
  obj.get_properties ().execute_createfcn  ();
}

// ---------------------------------------------------------------------

void
base_graphics_backend::property_changed (const graphics_handle& h, int id)
{
  graphics_object go = gh_manager::get_object (h);

  property_changed (go, id);
}

void
base_graphics_backend::object_created (const graphics_handle& h)
{
  graphics_object go = gh_manager::get_object (h);

  object_created (go);
}

void
base_graphics_backend::object_destroyed (const graphics_handle& h)
{
  graphics_object go = gh_manager::get_object (h);

  object_destroyed (go);
}
// ---------------------------------------------------------------------

static Matrix
maybe_set_children (const Matrix& kids, const octave_value& val)
{
  const Matrix new_kids = val.matrix_value ();

  bool ok = true;

  if (! error_state)
    {
      if (kids.numel () == new_kids.numel ())
	{
	  Matrix t1 = kids;
	  Matrix t2 = new_kids;

	  t1 = t1.sort ();
	  t2 = t2.sort ();

	  if (t1 != t2)
	    ok = false;
	}
      else
	ok = false;

      if (! ok)
	error ("set: new children must be a permutation of existing children");
    }
  else
    {
      ok = false;
      error ("set: expecting children to be array of graphics handles");
    }

  return ok ? new_kids : kids;
}

void
base_properties::set_from_list (base_graphics_object& obj,
				property_list& defaults)
{
  std::string go_name = graphics_object_name ();

  property_list::plist_map_const_iterator p = defaults.find (go_name);

  if (p != defaults.end ())
    {
      const property_list::pval_map_type pval_map = p->second;

      for (property_list::pval_map_const_iterator q = pval_map.begin ();
	   q != pval_map.end ();
	   q++)
	{
	  std::string pname = q->first;

	  obj.set (pname, q->second);

	  if (error_state)
	    {
	      error ("error setting default property %s", pname.c_str ());
	      break;
	    }
	}
    }
}

octave_value
base_properties::get_dynamic (const caseless_str& name) const
{
  octave_value retval;

  std::map<caseless_str, property, cmp_caseless_str>::const_iterator it = all_props.find (name);

  if (it != all_props.end ())
    retval = it->second.get ();
  else
    error ("get: unknown property \"%s\"", name.c_str ());

  return retval;
}

octave_value
base_properties::get_dynamic (bool all) const
{
  Octave_map m;

  for (std::map<caseless_str, property, cmp_caseless_str>::const_iterator it = all_props.begin ();
       it != all_props.end (); ++it)
    if (all || ! it->second.is_hidden ())
      m.assign (it->second.get_name (), it->second.get ());

  return m;
}

std::set<std::string>
base_properties::dynamic_property_names (void) const
{
  return dynamic_properties;
}

bool
base_properties::has_dynamic_property (const std::string& pname)
{
  const std::set<std::string>& dynprops = dynamic_property_names ();

  return dynprops.find (pname) != dynprops.end ();
}

void
base_properties::set_dynamic (const caseless_str& pname,
			      const octave_value& val)
{
  std::map<caseless_str, property, cmp_caseless_str>::iterator it = all_props.find (pname);

  if (it != all_props.end ())
    it->second.set (val);
  else
    error ("set: unknown property \"%s\"", pname.c_str ());

  if (! error_state)
    {
      dynamic_properties.insert (pname);

      mark_modified ();
    }
}

property
base_properties::get_property_dynamic (const caseless_str& name)
{
  std::map<caseless_str, property, cmp_caseless_str>::const_iterator it = all_props.find (name);

  if (it == all_props.end ())
    {
      error ("get_property: unknown property \"%s\"", name.c_str ());
      return property ();
    }
  else
    return it->second;
}

void
base_properties::remove_child (const graphics_handle& h)
{
  octave_idx_type k = -1;
  octave_idx_type n = children.numel ();
  for (octave_idx_type i = 0; i < n; i++)
    {
      if (h.value () == children(i))
	{
	  k = i;
	  break;
	}
    }

  if (k >= 0)
    {
      Matrix new_kids (n-1, 1);
      octave_idx_type j = 0;
      for (octave_idx_type i = 0; i < n; i++)
	{
	  if (i != k)
	    new_kids(j++) = children(i);
	}
      children = new_kids;
      mark_modified ();
    }
}

void
base_properties::set_parent (const octave_value& val)
{
  double tmp = val.double_value ();

  graphics_handle new_parent = octave_NaN;

  if (! error_state)
    {
      new_parent = gh_manager::lookup (tmp);

      if (new_parent.ok ())
	{
	  graphics_object parent_obj = gh_manager::get_object (get_parent ());

	  parent_obj.remove_child (__myhandle__);

	  parent = new_parent.as_octave_value ();

	  ::adopt (parent.handle_value (), __myhandle__);
	}
      else
	error ("set: invalid graphics handle (= %g) for parent", tmp);
    }
  else
    error ("set: expecting parent to be a graphics handle");
}

void
base_properties::set_children (const octave_value& val)
{
  children = maybe_set_children (children, val);
}

void
base_properties::mark_modified (void)
{
  __modified__ = "on";
  graphics_object parent_obj = gh_manager::get_object (get_parent ());
  if (parent_obj)
    parent_obj.mark_modified ();
}

void
base_properties::override_defaults (base_graphics_object& obj)
{
  graphics_object parent_obj = gh_manager::get_object (get_parent ());

  if (parent_obj)
    parent_obj.override_defaults (obj);
}

void
base_properties::update_axis_limits (const std::string& axis_type) const
{
  graphics_object obj = gh_manager::get_object (__myhandle__);

  if (obj)
    obj.update_axis_limits (axis_type);
}

void
base_properties::delete_children (void)
{
  octave_idx_type n = children.numel ();

  // A callback function might have already deleted the child,
  // so check before deleting
  for (octave_idx_type i = 0; i < n; i++)
    {
      graphics_object go = gh_manager::get_object (children(i));

      if (go.valid_object ())
	gh_manager::free (children(i));
    }
}

graphics_backend
base_properties::get_backend (void) const
{
  graphics_object go = gh_manager::get_object (get_parent ());

  if (go)
    return go.get_backend ();
  else
    return graphics_backend ();
}

void
base_properties::update_boundingbox (void)
{
  Matrix kids = get_children ();

  for (int i = 0; i < kids.numel (); i++)
    {
      graphics_object go = gh_manager::get_object (kids(i));

      if (go.valid_object ())
	go.get_properties ().update_boundingbox ();
    }
}

void
base_properties::add_listener (const caseless_str& nm, const octave_value& v,
			       listener_mode mode)
{
  property p = get_property (nm);

  if (! error_state && p.ok ())
    p.add_listener (v, mode);
}

void
base_properties::delete_listener (const caseless_str& nm, 
				  const octave_value& v, listener_mode mode)
{
  property p = get_property (nm);

  if (! error_state && p.ok ())
    p.delete_listener (v, mode);
}

// ---------------------------------------------------------------------

class gnuplot_backend : public base_graphics_backend
{
public:
  gnuplot_backend (void)
      : base_graphics_backend ("gnuplot") { }

  ~gnuplot_backend (void) { }

  bool is_valid (void) const { return true; }

  void object_destroyed (const graphics_object& go)
    {
      if (go.isa ("figure"))
	{
	  const figure::properties& props =
	      dynamic_cast<const figure::properties&> (go.get_properties ());

	  send_quit (props.get___plot_stream__ ());
	}
    }

  void property_changed (const graphics_object& go, int id)
    {
      if (go.isa ("figure"))
	{
	  graphics_object obj (go);

	  figure::properties& props =
	      dynamic_cast<figure::properties&> (obj.get_properties ());

	  switch (id)
	    {
	    case base_properties::VISIBLE:
	      if (! props.is_visible ())
		{
		  send_quit (props.get___plot_stream__ ());
		  props.set___plot_stream__ (Matrix ());
		  props.set___enhanced__ (false);
		}
	      break;
	    }
	}
    }

  void redraw_figure (const graphics_object& go) const
    {
      octave_value_list args;
      args(0) = go.get_handle ().as_octave_value ();
      feval ("gnuplot_drawnow", args);
    }

  void print_figure (const graphics_object& go, const std::string& term,
		     const std::string& file, bool mono,
		     const std::string& debug_file) const
    {
      octave_value_list args;
      if (! debug_file.empty ())
	args(4) = debug_file;
      args(3) = mono;
      args(2) = file;
      args(1) = term;
      args(0) = go.get_handle ().as_octave_value ();
      feval ("gnuplot_drawnow", args);
    }

  Matrix get_canvas_size (const graphics_handle&) const
    {
      Matrix sz (1, 2, 0.0);
      return sz;
    }

  double get_screen_resolution (void) const
    { return 72.0; }

  Matrix get_screen_size (void) const
    { return Matrix (1, 2, 0.0); }

private:
  void send_quit (const octave_value& pstream) const
    {
      if (! pstream.is_empty ())
	{
	  octave_value_list args;
	  Matrix fids = pstream.matrix_value ();

	  if (! error_state)
	    {
	      args(1) = "\nquit;\n";
	      args(0) = fids(0);
	      feval ("fputs", args);

	      args.resize (1);
	      feval ("fflush", args);
	      feval ("pclose", args);

	      if (fids.numel () > 1)
		{
		  args(0) = fids(1);
		  feval ("pclose", args);

		  if (fids.numel () > 2)
		    {
		      args(0) = fids(2);
		      feval ("waitpid", args);
		    }
		}
	    }
	}
    }
};

graphics_backend
graphics_backend::default_backend (void)
{
  if (available_backends.size () == 0)
    register_backend (new gnuplot_backend ());

  return available_backends["gnuplot"];
}

std::map<std::string, graphics_backend> graphics_backend::available_backends;

// ---------------------------------------------------------------------

void
base_graphics_object::update_axis_limits (const std::string& axis_type)
{
  if (valid_object ())
    {
      graphics_object parent_obj = gh_manager::get_object (get_parent ());

      if (parent_obj)
	parent_obj.update_axis_limits (axis_type);
    }
  else
    error ("base_graphics_object::update_axis_limits: invalid graphics object");
}

void
base_graphics_object::remove_all_listeners (void)
{
  Octave_map m = get (true).map_value ();

  for (Octave_map::const_iterator pa = m.begin (); pa != m.end (); pa++)
    {
      // FIXME -- there has to be a better way.  I think we want to
      // ask whether it is OK to delete the listener for the given
      // property.  How can we know in advance that it will be OK?

      unwind_protect::frame_id_t uwp_frame = unwind_protect::begin_frame ();

      unwind_protect::protect_var (discard_error_messages);
      unwind_protect::protect_var (error_state);

      discard_error_messages = true;

      property p = get_properties ().get_property (pa->first);

      if (! error_state && p.ok ())
	p.delete_listener ();

      unwind_protect::run_frame (uwp_frame);
    }
}

// ---------------------------------------------------------------------

#include "graphics-props.cc"

// ---------------------------------------------------------------------

void
root_figure::properties::set_currentfigure (const octave_value& v)
{
  graphics_handle val (v);

  if (error_state)
    return;

  if (xisnan (val.value ()) || is_handle (val))
    {
      currentfigure = val;

      gh_manager::push_figure (val);
    }
  else
    gripe_set_invalid ("currentfigure");
}

void
root_figure::properties::set_callbackobject (const octave_value& v)
{
  graphics_handle val (v);

  if (error_state)
    return;

  if (xisnan (val.value ()))
    {
      if (! cbo_stack.empty ())
	{
	  val = cbo_stack.front ();

	  cbo_stack.pop_front ();
	}

      callbackobject = val;
    }
  else if (is_handle (val))
    {
      if (get_callbackobject ().ok ())
	cbo_stack.push_front (get_callbackobject ());

      callbackobject = val;
    }
  else
    gripe_set_invalid ("callbackobject");
}

void
root_figure::properties::update_units (void)
{
  caseless_str xunits = get_units ();

  Matrix ss = default_screensize ();

  double dpi = get_screenpixelsperinch ();

  if (xunits.compare ("inches"))
    {
      ss(0) = 0;
      ss(1) = 0;
      ss(2) /= dpi;
      ss(3) /= dpi;
    }
  else if (xunits.compare ("centimeters"))
    {
      ss(0) = 0;
      ss(1) = 0;
      ss(2) *= 2.54 / dpi;
      ss(3) *= 2.54 / dpi;
    }
  else if (xunits.compare ("normalized"))
    {
      ss = Matrix (1, 4, 1.0);
    }
  else if (xunits.compare ("points"))
    {
      ss(0) = 0;
      ss(1) = 0;
      ss(2) *= 72 / dpi;
      ss(3) *= 72 / dpi;
    }

  set_screensize (ss);
}

void
root_figure::properties::remove_child (const graphics_handle& gh)
{
  gh_manager::pop_figure (gh);

  graphics_handle cf = gh_manager::current_figure ();

  xset (0, "currentfigure", cf.value ());
  
  base_properties::remove_child (gh);
}

property_list
root_figure::factory_properties = root_figure::init_factory_properties ();

// ---------------------------------------------------------------------

void
figure::properties::set_currentaxes (const octave_value& v)
{
  graphics_handle val (v);

  if (error_state)
    return;

  if (xisnan (val.value ()) || is_handle (val))
    currentaxes = val;
  else
    gripe_set_invalid ("currentaxes");
}

void
figure::properties::remove_child (const graphics_handle& gh)
{
  base_properties::remove_child (gh);

  if (gh == currentaxes.handle_value ())
    {
      graphics_handle new_currentaxes;

      for (octave_idx_type i = 0; i < children.numel (); i++)
	{
	  graphics_handle kid = children(i);

	  graphics_object go = gh_manager::get_object (kid);

	  if (go.isa ("axes"))
	    {
	      new_currentaxes = kid;
	      break;
	    }
	}

      currentaxes = new_currentaxes;
    }
}

void
figure::properties::set_visible (const octave_value& val)
{
  std::string s = val.string_value ();

  if (! error_state)
    {
      if (s == "on")
	xset (0, "currentfigure", __myhandle__.value ());

      visible = val;
    }
}

Matrix
figure::properties::get_boundingbox (bool) const
{
  Matrix screen_size = xget (0, "screensize").matrix_value ().extract_n (0, 2, 1, 2);
  Matrix pos;

  pos = convert_position (get_position ().matrix_value (), get_units (),
			  "pixels", screen_size);

  pos(0)--;
  pos(1)--;
  pos(1) = screen_size(1) - pos(1) - pos(3);

  return pos;
}

void
figure::properties::set_boundingbox (const Matrix& bb)
{
  Matrix screen_size = xget (0, "screensize").matrix_value ().extract_n (0, 2, 1, 2);
  Matrix pos = bb;

  pos(1) = screen_size(1) - pos(1) - pos(3);
  pos(1)++;
  pos(0)++;
  pos = convert_position (pos, "pixels", get_units (), screen_size);

  set_position (pos);
}

void
figure::properties::set_position (const octave_value& v)
{
  if (! error_state)
    {
      Matrix old_bb, new_bb;

      old_bb = get_boundingbox ();
      position = v;
      new_bb = get_boundingbox ();

      if (old_bb != new_bb)
	{
	  if (old_bb(2) != new_bb(2) || old_bb(3) != new_bb(3))
	    {
	      execute_resizefcn ();
	      update_boundingbox ();
	    }
	}

      mark_modified ();
    }
}

std::string
figure::properties::get_title (void) const
{
  if (is_numbertitle ())
    {
      std::ostringstream os;
      std::string nm = get_name ();

      os << "Figure " << __myhandle__.value ();
      if (! nm.empty ())
	os << ": " << get_name ();

      return os.str ();
    }
  else
    return get_name ();
}

octave_value
figure::get_default (const caseless_str& name) const
{
  octave_value retval = default_properties.lookup (name);

  if (retval.is_undefined ())
    {
      graphics_handle parent = get_parent ();
      graphics_object parent_obj = gh_manager::get_object (parent);

      retval = parent_obj.get_default (name);
    }

  return retval;
}

// ---------------------------------------------------------------------

void
axes::properties::init (void)
{
  position.add_constraint (dim_vector (1, 4));
  position.add_constraint (dim_vector (0, 0));
  outerposition.add_constraint (dim_vector (1, 4));
  colororder.add_constraint (dim_vector (-1, 3));
  dataaspectratio.add_constraint (dim_vector (1, 3));
  plotboxaspectratio.add_constraint (dim_vector (1, 3));
  xlim.add_constraint (2);
  ylim.add_constraint (2);
  zlim.add_constraint (2);
  clim.add_constraint (2);
  alim.add_constraint (2);
  xtick.add_constraint (dim_vector (1, -1));
  ytick.add_constraint (dim_vector (1, -1));
  ztick.add_constraint (dim_vector (1, -1));
  Matrix vw (1, 2, 0);
  vw(1) = 90;
  view = vw;
  view.add_constraint (dim_vector (1, 2));
  cameraposition.add_constraint (dim_vector (1, 3));
  Matrix upv (1, 3, 0.0);
  upv(2) = 1.0;
  cameraupvector = upv;
  cameraupvector.add_constraint (dim_vector (1, 3));
  currentpoint.add_constraint (dim_vector (2, 3));
  ticklength.add_constraint (dim_vector (1, 2));
  tightinset.add_constraint (dim_vector (1, 4));

  x_zlim.resize (1, 2);

  sx = "linear";
  sy = "linear";
  sz = "linear";

  calc_ticklabels (xtick, xticklabel, xscale.is ("log"));
  calc_ticklabels (ytick, yticklabel, yscale.is ("log"));
  calc_ticklabels (ztick, zticklabel, zscale.is ("log"));

  xset (xlabel.handle_value (), "handlevisibility", "off");
  xset (ylabel.handle_value (), "handlevisibility", "off");
  xset (zlabel.handle_value (), "handlevisibility", "off");
  xset (title.handle_value (), "handlevisibility", "off");

  xset (xlabel.handle_value (), "horizontalalignment", "center");
  xset (ylabel.handle_value (), "horizontalalignment", "center");
  xset (zlabel.handle_value (), "horizontalalignment", "right");
  xset (title.handle_value (), "horizontalalignment", "center");

  xset (xlabel.handle_value (), "verticalalignment", "cap");
  xset (ylabel.handle_value (), "verticalalignment", "bottom");
  xset (title.handle_value (), "verticalalignment", "bottom");

  xset (ylabel.handle_value (), "rotation", 90.0);
  xset (zlabel.handle_value (), "visible", "off");
  
  xset (xlabel.handle_value (), "clipping", "off");
  xset (ylabel.handle_value (), "clipping", "off");
  xset (zlabel.handle_value (), "clipping", "off");
  xset (title.handle_value (), "clipping", "off");

  adopt (xlabel.handle_value ());
  adopt (ylabel.handle_value ());
  adopt (zlabel.handle_value ());
  adopt (title.handle_value ());
}

void 
axes::properties::sync_positions (void)
{
#if 0
  // FIXME -- this should take font metrics into consideration,
  // and also the fact that the colorbox leaves the outerposition
  // alone but alters the position. For now just don't adjust the
  // positions relative to each other.

  if (activepositionproperty.is ("outerposition"))
    {
      Matrix outpos = outerposition.get ().matrix_value ();
      Matrix defpos = default_axes_position ();
      Matrix pos(outpos);
      pos(0) = outpos(0) + defpos(0) * outpos(2);
      pos(1) = outpos(1) + defpos(1) * outpos(3);
      pos(2) = outpos(2) * defpos(2);
      pos(3) = outpos(3) * defpos(3);
      position = pos;
    }
  else
    {
      Matrix pos = position.get ().matrix_value ();
      pos(0) -= pos(2)*0.05;
      pos(1) -= pos(3)*0.05;
      pos(2) *= 1.1;
      pos(3) *= 1.1;
      outerposition = pos;
    }
#endif

  update_transform ();
}

void
axes::properties::set_text_child (handle_property& hp,
				  const std::string& who,
				  const octave_value& v)
{
  graphics_handle val = ::reparent (v, "set", who, __myhandle__, false);

  if (! error_state)
    {
      xset (val, "handlevisibility", "off");

      gh_manager::free (hp.handle_value ());

      base_properties::remove_child (hp.handle_value ());

      hp = val;

      adopt (hp.handle_value ());
    }
}

void
axes::properties::set_xlabel (const octave_value& v)
{
  set_text_child (xlabel, "xlabel", v);
}

void
axes::properties::set_ylabel (const octave_value& v)
{
  set_text_child (ylabel, "ylabel", v);
}

void
axes::properties::set_zlabel (const octave_value& v)
{
  set_text_child (zlabel, "zlabel", v);
}

void
axes::properties::set_title (const octave_value& v)
{
  set_text_child (title, "title", v);
}

void
axes::properties::set_defaults (base_graphics_object& obj,
				const std::string& mode)
{
  box = "on";
  key = "off";
  keybox = "off";
  keyreverse = "off";
  keypos = 1.0;
  colororder = default_colororder ();
  dataaspectratio = Matrix (1, 3, 1.0);
  dataaspectratiomode = "auto";
  layer = "bottom";

  Matrix tlim (1, 2, 0.0);
  tlim(1) = 1;
  xlim = tlim;
  ylim = tlim;
  zlim = tlim;
  
  Matrix cl (1, 2, 0);
  cl(1) = 1;
  clim = cl;
  
  xlimmode = "auto";
  ylimmode = "auto";
  zlimmode = "auto";
  climmode = "auto";

  xgrid = "off";
  ygrid = "off";
  zgrid = "off";
  xminorgrid = "off";
  yminorgrid = "off";
  zminorgrid = "off";
  xtick = Matrix ();
  ytick = Matrix ();
  ztick = Matrix ();
  xtickmode = "auto";
  ytickmode = "auto";
  ztickmode = "auto";
  xticklabel = "";
  yticklabel = "";
  zticklabel = "";
  xticklabelmode = "auto";
  yticklabelmode = "auto";
  zticklabelmode = "auto";
  color = color_values (1, 1, 1);
  xcolor = color_values ("black");
  ycolor = color_values ("black");
  zcolor = color_values ("black");
  xscale = "linear";
  yscale = "linear";
  zscale = "linear";
  xdir = "normal";
  ydir = "normal";
  zdir = "normal";
  yaxislocation = "left";
  xaxislocation = "bottom";

  // Note: camera properties will be set through update_transform
  camerapositionmode = "auto";
  cameratargetmode = "auto";
  cameraupvectormode = "auto";
  cameraviewanglemode = "auto";
  plotboxaspectratio = Matrix (1, 3, 1.0);
  drawmode = "normal";
  gridlinestyle = ":";
  linestyleorder = "-";
  linewidth = 0.5;
  minorgridlinestyle = ":";
  // Note: plotboxaspectratio will be set through update_aspectratiors
  plotboxaspectratiomode = "auto";
  projection = "orthographic";
  tickdir = "in";
  tickdirmode = "auto";
  ticklength = default_axes_ticklength ();
  tightinset = Matrix (1, 4, 0.0);

  sx = "linear";
  sy = "linear";
  sz = "linear";

  Matrix tview (1, 2, 0.0);
  tview(1) = 90;
  view = tview;

  visible = "on";
  nextplot = "replace";

  if (mode != "replace")
    {
      fontangle = "normal";
      fontname = OCTAVE_DEFAULT_FONTNAME;
      fontsize = 12;
      fontunits = "points";
      fontweight = "normal";

      Matrix touterposition (1, 4, 0.0);
      touterposition(2) = 1;
      touterposition(3) = 1;
      outerposition = touterposition;

      position = default_axes_position ();

      activepositionproperty = "outerposition";
    }

  delete_children ();

  children = Matrix ();

  xlabel = gh_manager::make_graphics_handle ("text", __myhandle__, false);
  ylabel = gh_manager::make_graphics_handle ("text", __myhandle__, false);
  zlabel = gh_manager::make_graphics_handle ("text", __myhandle__, false);
  title = gh_manager::make_graphics_handle ("text", __myhandle__, false);

  xset (xlabel.handle_value (), "handlevisibility", "off");
  xset (ylabel.handle_value (), "handlevisibility", "off");
  xset (zlabel.handle_value (), "handlevisibility", "off");
  xset (title.handle_value (), "handlevisibility", "off");

  xset (xlabel.handle_value (), "horizontalalignment", "center");
  xset (ylabel.handle_value (), "horizontalalignment", "center");
  xset (zlabel.handle_value (), "horizontalalignment", "right");
  xset (title.handle_value (), "horizontalalignment", "center");

  xset (xlabel.handle_value (), "verticalalignment", "cap");
  xset (ylabel.handle_value (), "verticalalignment", "bottom");
  xset (title.handle_value (), "verticalalignment", "bottom");

  xset (ylabel.handle_value (), "rotation", 90.0);
  xset (zlabel.handle_value (), "visible", "off");
  
  xset (xlabel.handle_value (), "clipping", "off");
  xset (ylabel.handle_value (), "clipping", "off");
  xset (zlabel.handle_value (), "clipping", "off");
  xset (title.handle_value (), "clipping", "off");

  adopt (xlabel.handle_value ());
  adopt (ylabel.handle_value ());
  adopt (zlabel.handle_value ());
  adopt (title.handle_value ());

  update_transform ();

  override_defaults (obj);
}

void
axes::properties::delete_text_child (handle_property& hp)
{
  graphics_handle h = hp.handle_value ();

  if (h.ok ())
    {
      graphics_object go = gh_manager::get_object (h);

      if (go.valid_object ())
	gh_manager::free (h);

      base_properties::remove_child (h);
    }

  // FIXME -- is it necessary to check whether the axes object is
  // being deleted now?  I think this function is only called when an
  // individual child object is delete and not when the parent axes
  // object is deleted.

  if (! is_beingdeleted ())
    {
      hp = gh_manager::make_graphics_handle ("text", __myhandle__, false);

      xset (hp.handle_value (), "handlevisibility", "off");

      adopt (hp.handle_value ());
    }
}

void
axes::properties::remove_child (const graphics_handle& h)
{
  if (xlabel.handle_value ().ok () && h == xlabel.handle_value ())
    delete_text_child (xlabel);
  else if (ylabel.handle_value ().ok () && h == ylabel.handle_value ())
    delete_text_child (ylabel);
  else if (zlabel.handle_value ().ok () && h == zlabel.handle_value ())
    delete_text_child (zlabel);
  else if (title.handle_value ().ok () && h == title.handle_value ())
    delete_text_child (title);
  else
    base_properties::remove_child (h);
}

Matrix
base_properties::get_children (void) const
{
  Matrix retval = children;
  
  graphics_object go = gh_manager::get_object (0);

  root_figure::properties& props =
      dynamic_cast<root_figure::properties&> (go.get_properties ());

  if (! props.is_showhiddenhandles ())
    {
      octave_idx_type k = 0;

      for (octave_idx_type i = 0; i < children.numel (); i++)
	{
	  graphics_handle kid = children (i);

	  if (gh_manager::is_handle_visible (kid))
	    retval(k++) = children(i);
	}

      retval.resize (k, 1);
    }

  return retval;;
}

inline Matrix
xform_matrix (void)
{
  Matrix m (4, 4, 0.0);
  for (int i = 0; i < 4; i++)
    m(i,i) = 1;
  return m;
}

inline ColumnVector
xform_vector (void)
{
  ColumnVector v (4, 0.0);
  v(3) = 1;
  return v;
}

inline ColumnVector
xform_vector (double x, double y, double z)
{
  ColumnVector v (4, 1.0);
  v(0) = x; v(1) = y; v(2) = z;
  return v;
}

inline ColumnVector
transform (const Matrix& m, double x, double y, double z)
{
  return (m * xform_vector (x, y, z));
}

inline Matrix
xform_scale (double x, double y, double z)
{
  Matrix m (4, 4, 0.0);
  m(0,0) = x; m(1,1) = y; m(2,2) = z; m(3,3) = 1;
  return m;
}

inline Matrix
xform_translate (double x, double y, double z)
{
  Matrix m = xform_matrix ();
  m(0,3) = x; m(1,3) = y; m(2,3) = z; m(3,3) = 1;
  return m;
}

inline void
scale (Matrix& m, double x, double y, double z)
{
  m = m * xform_scale (x, y, z);
}

inline void
translate (Matrix& m, double x, double y, double z)
{
  m = m * xform_translate (x, y, z);
}

inline void
xform (ColumnVector& v, const Matrix& m)
{
  v = m*v;
}

inline void
scale (ColumnVector& v, double x, double y, double z)
{
  v(0) *= x;
  v(1) *= y;
  v(2) *= z;
}

inline void
translate (ColumnVector& v, double x, double y, double z)
{
  v(0) += x;
  v(1) += y;
  v(2) += z;
}

inline void
normalize (ColumnVector& v)
{
  double fact = 1.0/sqrt(v(0)*v(0)+v(1)*v(1)+v(2)*v(2));
  scale (v, fact, fact, fact);
}

inline double
dot (const ColumnVector& v1, const ColumnVector& v2)
{
  return (v1(0)*v2(0)+v1(1)*v2(1)+v1(2)*v2(2));
}

inline double
norm (const ColumnVector& v)
{
  return sqrt (dot (v, v));
}

inline ColumnVector
cross (const ColumnVector& v1, const ColumnVector& v2)
{
  ColumnVector r = xform_vector ();
  r(0) = v1(1)*v2(2)-v1(2)*v2(1);
  r(1) = v1(2)*v2(0)-v1(0)*v2(2);
  r(2) = v1(0)*v2(1)-v1(1)*v2(0);
  return r;
}

inline Matrix
unit_cube (void)
{
  static double data[32] = {
      0,0,0,1,
      1,0,0,1,
      0,1,0,1,
      0,0,1,1,
      1,1,0,1,
      1,0,1,1,
      0,1,1,1,
      1,1,1,1};
  Matrix m (4, 8);
  memcpy (m.fortran_vec (), data, sizeof(double)*32);
  return m;
}

inline ColumnVector
cam2xform (const Array<double>& m)
{
  ColumnVector retval (4, 1.0);
  memcpy (retval.fortran_vec (), m.fortran_vec (), sizeof(double)*3);
  return retval;
}

inline RowVector
xform2cam (const ColumnVector& v)
{
  return v.extract_n (0, 3).transpose ();
}

void
axes::properties::update_camera (void)
{
  double xd = (xdir_is ("normal") ? 1 : -1);
  double yd = (ydir_is ("normal") ? 1 : -1);
  double zd = (zdir_is ("normal") ? 1 : -1);

  Matrix xlimits = sx.scale (get_xlim ().matrix_value ());
  Matrix ylimits = sy.scale (get_ylim ().matrix_value ());
  Matrix zlimits = sz.scale (get_zlim ().matrix_value ());

  double xo = xlimits(xd > 0 ? 0 : 1);
  double yo = ylimits(yd > 0 ? 0 : 1);
  double zo = zlimits(zd > 0 ? 0 : 1);
  
  Matrix pb  = get_plotboxaspectratio ().matrix_value ();
  
  bool autocam = (camerapositionmode_is ("auto")
		  && cameratargetmode_is ("auto")
	    	  && cameraupvectormode_is ("auto")
		  && cameraviewanglemode_is ("auto"));
  bool dowarp = (autocam && dataaspectratiomode_is("auto")
		 && plotboxaspectratiomode_is ("auto"));

  ColumnVector c_eye (xform_vector ());
  ColumnVector c_center (xform_vector ());
  ColumnVector c_upv (xform_vector ());
  
  if (cameratargetmode_is ("auto"))
    {
      c_center(0) = (xlimits(0)+xlimits(1))/2;
      c_center(1) = (ylimits(0)+ylimits(1))/2;
      c_center(2) = (zlimits(0)+zlimits(1))/2;

      cameratarget = xform2cam (c_center);
    }
  else
    c_center = cam2xform (get_cameratarget ().matrix_value ());
  
  if (camerapositionmode_is ("auto"))
    {
      Matrix tview = get_view ().matrix_value ();
      double az = tview(0), el = tview(1);
      double d = 5*sqrt(pb(0)*pb(0)+pb(1)*pb(1)+pb(2)*pb(2));

      if (el == 90 || el == -90)
	c_eye(2) = d*signum(el);
      else
	{
	  az *= M_PI/180.0;
	  el *= M_PI/180.0;
	  c_eye(0) = d*cos(el)*sin(az);
	  c_eye(1) = -d*cos(el)*cos(az);
	  c_eye(2) = d*sin(el);
	}
      c_eye(0) = c_eye(0)*(xlimits(1)-xlimits(0))/(xd*pb(0))+c_center(0);
      c_eye(1) = c_eye(1)*(ylimits(1)-ylimits(0))/(yd*pb(1))+c_center(1);
      c_eye(2) = c_eye(2)*(zlimits(1)-zlimits(0))/(zd*pb(2))+c_center(2);

      cameraposition = xform2cam (c_eye);
    }
  else
    c_eye = cam2xform (get_cameraposition ().matrix_value ());

  if (cameraupvectormode_is ("auto"))
    {
      Matrix tview = get_view ().matrix_value ();
      double az = tview(0), el = tview(1);

      if (el == 90 || el == -90)
	{
	  c_upv(0) = -sin(az*M_PI/180.0)*(xlimits(1)-xlimits(0))/pb(0);
	  c_upv(1) = cos(az*M_PI/180.0)*(ylimits(1)-ylimits(0))/pb(1);
	}
      else
	c_upv(2) = 1;

      cameraupvector = xform2cam (c_upv);
    }
  else
    c_upv = cam2xform (get_cameraupvector ().matrix_value ());

  Matrix x_view = xform_matrix ();
  Matrix x_projection = xform_matrix ();
  Matrix x_viewport = xform_matrix ();
  Matrix x_normrender = xform_matrix ();
  Matrix x_pre = xform_matrix ();
  
  x_render = xform_matrix ();
  x_render_inv = xform_matrix ();

  scale (x_pre, pb(0), pb(1), pb(2));
  translate (x_pre, -0.5, -0.5, -0.5);
  scale (x_pre, xd/(xlimits(1)-xlimits(0)), yd/(ylimits(1)-ylimits(0)),
	 zd/(zlimits(1)-zlimits(0)));
  translate (x_pre, -xo, -yo, -zo);

  xform (c_eye, x_pre);
  xform (c_center, x_pre);
  scale (c_upv, pb(0)/(xlimits(1)-xlimits(0)), pb(1)/(ylimits(1)-ylimits(0)), 
	 pb(2)/(zlimits(1)-zlimits(0)));
  translate (c_center, -c_eye(0), -c_eye(1), -c_eye(2));

  ColumnVector F (c_center), f (F), UP (c_upv);
  normalize (f);
  normalize (UP);

  if (std::abs (dot (f, UP)) > 1e-15)
    {
      double fa = 1/sqrt(1-f(2)*f(2));
      scale (UP, fa, fa, fa);
    }

  ColumnVector s = cross (f, UP);
  ColumnVector u = cross (s, f);

  scale (x_view, 1, 1, -1);
  Matrix l = xform_matrix ();
  l(0,0) = s(0); l(0,1) = s(1); l(0,2) = s(2);
  l(1,0) = u(0); l(1,1) = u(1); l(1,2) = u(2);
  l(2,0) = -f(0); l(2,1) = -f(1); l(2,2) = -f(2);
  x_view = x_view * l;
  translate (x_view, -c_eye(0), -c_eye(1), -c_eye(2));
  scale (x_view, pb(0), pb(1), pb(2));
  translate (x_view, -0.5, -0.5, -0.5);

  Matrix x_cube = x_view * unit_cube ();
  ColumnVector cmin = x_cube.row_min (), cmax = x_cube.row_max ();
  double xM = cmax(0)-cmin(0);
  double yM = cmax(1)-cmin(1);

  Matrix bb = get_boundingbox (true);

  double v_angle;

  if (cameraviewanglemode_is ("auto"))
    {
      double af;

      // FIXME -- was this really needed?  When compared to Matlab, it
      // does not seem to be required. Need investigation with concrete
      // backend to see results visually.
      if (false && dowarp)
        af = 1.0 / (xM > yM ? xM : yM);
      else
        {
          if ((bb(2)/bb(3)) > (xM/yM))
            af = 1.0 / yM;
          else
            af = 1.0 / xM;
        }
      v_angle = 2 * (180.0 / M_PI) * atan (1 / (2 * af * norm (F)));

      cameraviewangle = v_angle;
    }
  else
    v_angle = get_cameraviewangle ();

  double pf = 1 / (2 * tan ((v_angle / 2) * M_PI / 180.0) * norm (F));
  scale (x_projection, pf, pf, 1);

  if (dowarp)
    {
      xM *= pf;
      yM *= pf;
      translate (x_viewport, bb(0)+bb(2)/2, bb(1)+bb(3)/2, 0);
      scale (x_viewport, bb(2)/xM, -bb(3)/yM, 1);
    }
  else
    {
      double pix = 1;
      if (autocam)
	{
	  if ((bb(2)/bb(3)) > (xM/yM))
	    pix = bb(3);
	  else
	    pix = bb(2);
	}
      else
	pix = (bb(2) < bb(3) ? bb(2) : bb(3));
      translate (x_viewport, bb(0)+bb(2)/2, bb(1)+bb(3)/2, 0);
      scale (x_viewport, pix, -pix, 1);
    }

  x_normrender = x_viewport * x_projection * x_view;

  x_cube = x_normrender * unit_cube ();
  cmin = x_cube.row_min ();
  cmax = x_cube.row_max ();
  x_zlim.resize (1, 2);
  x_zlim(0) = cmin(2);
  x_zlim(1) = cmax(2);

  x_render = x_normrender;
  scale (x_render, xd/(xlimits(1)-xlimits(0)), yd/(ylimits(1)-ylimits(0)),
	 zd/(zlimits(1)-zlimits(0)));
  translate (x_render, -xo, -yo, -zo);

  x_viewtransform = x_view;
  x_projectiontransform = x_projection;
  x_viewporttransform = x_viewport;
  x_normrendertransform = x_normrender;
  x_rendertransform = x_render;

  x_render_inv = x_render.inverse ();

  // Note: these matrices are a slight modified version of the regular
  // matrices, more suited for OpenGL rendering (x_gl_mat1 => light
  // => x_gl_mat2)
  x_gl_mat1 = x_view;
  scale (x_gl_mat1, xd/(xlimits(1)-xlimits(0)), yd/(ylimits(1)-ylimits(0)),
	 zd/(zlimits(1)-zlimits(0)));
  translate (x_gl_mat1, -xo, -yo, -zo);
  x_gl_mat2 = x_viewport * x_projection;
}

void
axes::properties::update_aspectratios (void)
{
  Matrix xlimits = get_xlim ().matrix_value ();
  Matrix ylimits = get_ylim ().matrix_value ();
  Matrix zlimits = get_zlim ().matrix_value ();

  double dx = (xlimits(1)-xlimits(0));
  double dy = (ylimits(1)-ylimits(0));
  double dz = (zlimits(1)-zlimits(0));

  if (dataaspectratiomode_is ("auto"))
    {
      double dmin = xmin (xmin (dx, dy), dz);
      Matrix da (1, 3, 0.0);

      da(0) = dx/dmin;
      da(1) = dy/dmin;
      da(2) = dz/dmin;

      dataaspectratio = da;
    }

  if (plotboxaspectratiomode_is ("auto"))
    {
      if (dataaspectratiomode_is ("auto"))
	plotboxaspectratio = Matrix (1, 3, 1.0);
      else
	{
	  Matrix da = get_dataaspectratio ().matrix_value ();
	  Matrix pba (1, 3, 0.0);

	  pba(0) = dx/da(0);
	  pba(1) = dy/da(1);
	  pba(2) = dz/da(2);
	}
    }
  
  // FIXME -- if plotboxaspectratiomode is "manual", limits
  // and/or dataaspectratio might be adapted.
}

// The INTERNAL flag defines whether position or outerposition is used.

Matrix
axes::properties::get_boundingbox (bool internal) const
{
  graphics_object obj = gh_manager::get_object (get_parent ());
  Matrix parent_bb = obj.get_properties ().get_boundingbox (true);
  Matrix pos = (internal ?
		  get_position ().matrix_value ()
		  : get_outerposition ().matrix_value ());


  pos = convert_position (pos, get_units (), "pixels",
			  parent_bb.extract_n (0, 2, 1, 2));
  pos(0)--;
  pos(1)--;
  pos(1) = parent_bb(3) - pos(1) - pos(3);

  return pos;
}

ColumnVector
graphics_xform::xform_vector (double x, double y, double z)
{
  return ::xform_vector (x, y, z);
}

Matrix
graphics_xform::xform_eye (void)
{
  return ::xform_matrix ();
}

ColumnVector
graphics_xform::transform (double x, double y, double z,
			   bool use_scale) const
{
  if (use_scale)
    {
      x = sx.scale (x);
      y = sy.scale (y);
      z = sz.scale (z);
    }

  return ::transform (xform, x, y, z);
}

ColumnVector
graphics_xform::untransform (double x, double y, double z,
			     bool use_scale) const
{
  ColumnVector v = ::transform (xform_inv, x, y, z);

  if (use_scale)
    {
      v(0) = sx.unscale (v(0));
      v(1) = sy.unscale (v(1));
      v(2) = sz.unscale (v(2));
    }

  return v;
}

octave_value
axes::get_default (const caseless_str& name) const
{
  octave_value retval = default_properties.lookup (name);

  if (retval.is_undefined ())
    {
      graphics_handle parent = get_parent ();
      graphics_object parent_obj = gh_manager::get_object (parent);

      retval = parent_obj.get_default (name);
    }

  return retval;
}

// FIXME -- remove.
// FIXME -- maybe this should go into array_property class?
/*
static void
check_limit_vals (double& min_val, double& max_val, double& min_pos,
		  const array_property& data)
{
  double val = data.min_val ();
  if (! (xisinf (val) || xisnan (val)) && val < min_val)
    min_val = val;
  val = data.max_val ();
  if (! (xisinf (val) || xisnan (val)) && val > max_val)
    max_val = val;
  val = data.min_pos ();
  if (! (xisinf (val) || xisnan (val)) && val > 0 && val < min_pos)
    min_pos = val;
}
*/

static void
check_limit_vals (double& min_val, double& max_val, double& min_pos,
		  const octave_value& data)
{
  if (data.is_matrix_type ())
    {
      Matrix m = data.matrix_value ();

      if (! error_state && m.numel () == 3)
	{
	  double val;

	  val = m(0);
	  if (! (xisinf (val) || xisnan (val)) && val < min_val)
	    min_val = val;

	  val = m(1);
	  if (! (xisinf (val) || xisnan (val)) && val > max_val)
	    max_val = val;

	  val = m(2);
	  if (! (xisinf (val) || xisnan (val)) && val > 0 && val < min_pos)
	    min_pos = val;
	}
    }
}

// magform(x) Returns (a, b), where x = a * 10^b, a >= 1., and b is
// integral.

static void
magform (double x, double& a, int& b)
{
  if (x == 0)
    {
      a = 0;
      b = 0;
    }
  else
    {
      double l = std::log10 (std::abs (x));
      double r = std::fmod (l, 1.);
      a = std::pow (10.0, r);
      b = static_cast<int> (l-r);
      if (a < 1)
	{
	  a *= 10;
	  b -= 1;
	}

      if (x < 0)
	a = -a;
    }
}

// A translation from Tom Holoryd's python code at
// http://kurage.nimh.nih.gov/tomh/tics.py
// FIXME -- add log ticks

double
axes::properties::calc_tick_sep (double lo, double hi)
{
  int ticint = 5;

  // Reference: Lewart, C. R., "Algorithms SCALE1, SCALE2, and
  // SCALE3 for Determination of Scales on Computer Generated
  // Plots", Communications of the ACM, 10 (1973), 639-640.
  // Also cited as ACM Algorithm 463.

  double a;
  int b, x;

  magform ((hi-lo)/ticint, a, b);

  static const double sqrt_2 = sqrt (2.0);
  static const double sqrt_10 = sqrt (10.0);
  static const double sqrt_50 = sqrt (50.0);

  if (a < sqrt_2)
    x = 1;
  else if (a < sqrt_10)
    x = 2;
  else if (a < sqrt_50)
    x = 5;
  else
    x = 10;

  return x * std::pow (10., b);

}

// Attempt to make "nice" limits from the actual max and min of the
// data.  For log plots, we will also use the smallest strictly positive
// value.

Matrix
axes::properties::get_axis_limits (double xmin, double xmax,
				   double min_pos, bool logscale)
{
  Matrix retval;

  double min_val = xmin;
  double max_val = xmax;

  if (! (xisinf (min_val) || xisinf (max_val)))
    {
      if (logscale)
	{
	  if (xisinf (min_pos))
	    {
	      // warning ("axis: logscale with no positive values to plot");
	      return retval;
	    }

	  if (min_val <= 0)
	    {
	      warning ("axis: omitting nonpositive data in log plot");
	      min_val = min_pos;
	    }
	  // FIXME -- maybe this test should also be relative?
	  if (std::abs (min_val - max_val) < sqrt (DBL_EPSILON))
	    {
	      min_val *= 0.9;
	      max_val *= 1.1;
	    }
	  min_val = pow (10, floor (log10 (min_val)));
	  max_val = pow (10, ceil (log10 (max_val)));
	}
      else
	{
	  if (min_val == 0 && max_val == 0)
	    {
	      min_val = -1;
	      max_val = 1;
	    }
	  // FIXME -- maybe this test should also be relative?
	  else if (std::abs (min_val - max_val) < sqrt (DBL_EPSILON))
	    {
	      min_val -= 0.1 * std::abs (min_val);
	      max_val += 0.1 * std::abs (max_val);
	    }

	  double tick_sep = calc_tick_sep (min_val , max_val);
	  min_val = tick_sep * std::floor (min_val / tick_sep);
	  max_val = tick_sep * ceil (max_val / tick_sep);
	}
    }

  retval.resize (1, 2);

  retval(0) = min_val;
  retval(1) = max_val;

  return retval;
}

void 
axes::properties::calc_ticks_and_lims (array_property& lims,
				       array_property& ticks,
				       bool limmode_is_auto, bool is_logscale)
{
  // FIXME -- add log ticks and lims

  if (lims.get ().is_empty ())
    return;

  double lo = (lims.get ().matrix_value ()) (0);
  double hi = (lims.get ().matrix_value ()) (1);
  // FIXME should this be checked for somewhere else? (i.e. set{x,y,z}lim)
  if (hi < lo) 
    {
      double tmp = hi;
      hi = lo;
      lo = tmp;
    }

  if (is_logscale)
    {
      // FIXME we should check for negtives here
      hi = std::log10 (hi);
      lo = std::log10 (lo);
    }

  double tick_sep = calc_tick_sep (lo , hi);

  int i1 = static_cast<int> (std::floor (lo / tick_sep));
  int i2 = static_cast<int> (std::ceil (hi / tick_sep));

  if (limmode_is_auto)
    {
      // adjust limits to include min and max tics
      Matrix tmp_lims (1,2);
      tmp_lims(0) = tick_sep * i1;
      tmp_lims(1) = tick_sep * i2;

      if (is_logscale) 
	{
	  tmp_lims(0) = std::pow (10.,tmp_lims(0));
	  tmp_lims(1) = std::pow (10.,tmp_lims(1));
	}
      lims = tmp_lims;
    }
  else
    {
      // adjust min and max tics if they are out of limits
      i1 = static_cast<int> (std::ceil (lo / tick_sep));
      i2 = static_cast<int> (std::floor (hi / tick_sep));
    }
      
  Matrix tmp_ticks (1, i2-i1+1);
  for (int i = 0; i <= i2-i1; i++) 
    {
      tmp_ticks (i) = tick_sep * (i+i1);
      if (is_logscale)
	tmp_ticks (i) = std::pow (10., tmp_ticks (i));
    }

  ticks = tmp_ticks;
}

void
axes::properties::calc_ticklabels (const array_property& ticks,
				   any_property& labels, bool /*logscale*/)
{
  Matrix values = ticks.get ().matrix_value ();
  Cell c (values.dims ());
  std::ostringstream os;

  for (int i = 0; i < values.numel (); i++)
    {
      os.str (std::string ());
      os << values(i);
      c(i) = os.str ();
    }

  labels = c;
}

void
get_children_limits (double& min_val, double& max_val, double& min_pos,
		     const Matrix& kids, char limit_type)
{
  octave_idx_type n = kids.numel ();

  switch (limit_type)
    {
    case 'x':
      for (octave_idx_type i = 0; i < n; i++)
	{
	  graphics_object obj = gh_manager::get_object (kids(i));

	  if (obj.is_xliminclude ())
	    {
	      octave_value lim = obj.get_xlim ();

	      check_limit_vals (min_val, max_val, min_pos, lim);
	    }
	}
      break;

    case 'y':
      for (octave_idx_type i = 0; i < n; i++)
	{
	  graphics_object obj = gh_manager::get_object (kids(i));

	  if (obj.is_yliminclude ())
	    {
	      octave_value lim = obj.get_ylim ();

	      check_limit_vals (min_val, max_val, min_pos, lim);
	    }
	}
      break;
    
    case 'z':
      for (octave_idx_type i = 0; i < n; i++)
	{
	  graphics_object obj = gh_manager::get_object (kids(i));

	  if (obj.is_zliminclude ())
	    {
	      octave_value lim = obj.get_zlim ();

	      check_limit_vals (min_val, max_val, min_pos, lim);
	    }
	}
      break;
    
    case 'c':
      for (octave_idx_type i = 0; i < n; i++)
	{
	  graphics_object obj = gh_manager::get_object (kids(i));

	  if (obj.is_climinclude ())
	    {
	      octave_value lim = obj.get_clim ();

	      check_limit_vals (min_val, max_val, min_pos, lim);
	    }
	}
      break;
    
    case 'a':
      for (octave_idx_type i = 0; i < n; i++)
	{
	  graphics_object obj = gh_manager::get_object (kids(i));

	  if (obj.is_aliminclude ())
	    {
	      octave_value lim = obj.get_alim ();

	      check_limit_vals (min_val, max_val, min_pos, lim);
	    }
	}
      break;

    default:
      break;
    }
}

static bool updating_axis_limits = false;

void
axes::update_axis_limits (const std::string& axis_type)
{
  if (updating_axis_limits)
    return;

  Matrix kids = xproperties.get_children ();

  double min_val = octave_Inf;
  double max_val = -octave_Inf;
  double min_pos = octave_Inf;

  char update_type = 0;

  Matrix limits;

  if (axis_type == "xdata" || axis_type == "xscale"
      || axis_type == "xldata" || axis_type == "xudata"
      || axis_type == "xlimmode" || axis_type == "xliminclude"
      || axis_type == "xlim")
    {
      if (xproperties.xlimmode_is ("auto"))
	{
	  get_children_limits (min_val, max_val, min_pos, kids, 'x');
	  
	  limits = xproperties.get_axis_limits (min_val, max_val, min_pos,
						xproperties.xscale_is ("log"));

	  update_type = 'x';
	}
    }
  else if (axis_type == "ydata" || axis_type == "yscale"
	   || axis_type == "ldata" || axis_type == "udata"
	   || axis_type == "ylimmode" || axis_type == "yliminclude"
	   || axis_type == "ylim")
    {
      if (xproperties.ylimmode_is ("auto"))
	{
	  get_children_limits (min_val, max_val, min_pos, kids, 'y');

	  limits = xproperties.get_axis_limits (min_val, max_val, min_pos,
						xproperties.yscale_is ("log"));

	  update_type = 'y';
	}
    }
  else if (axis_type == "zdata" || axis_type == "zscale"
	   || axis_type == "zlimmode" || axis_type == "zliminclude"
	   || axis_type == "zlim")
    {
      if (xproperties.zlimmode_is ("auto"))
	{
	  get_children_limits (min_val, max_val, min_pos, kids, 'z');

	  limits = xproperties.get_axis_limits (min_val, max_val, min_pos,
						xproperties.zscale_is ("log"));

	  update_type = 'z';
	}
    }
  else if (axis_type == "cdata" || axis_type == "climmode"
	   || axis_type == "cdatamapping" || axis_type == "climinclude"
	   || axis_type == "clim")
    {
      if (xproperties.climmode_is ("auto"))
	{
	  get_children_limits (min_val, max_val, min_pos, kids, 'c');

	  if (min_val > max_val)
	    {
	      min_val = min_pos = 0;
	      max_val = 1;
	    }
	  else if (min_val == max_val)
	    max_val = min_val + 1;

	  limits.resize (1, 2);

	  limits(0) = min_val;
	  limits(1) = max_val;

	  update_type = 'c';
	}

    }
  else if (axis_type == "alphadata" || axis_type == "alimmode"
	   || axis_type == "alphadatamapping" || axis_type == "aliminclude"
	   || axis_type == "alim")
    {
      if (xproperties.alimmode_is ("auto"))
	{
	  get_children_limits (min_val, max_val, min_pos, kids, 'a');

	  if (min_val > max_val)
	    {
	      min_val = min_pos = 0;
	      max_val = 1;
	    }
	  else if (min_val == max_val)
	    max_val = min_val + 1;

	  limits.resize (1, 2);

	  limits(0) = min_val;
	  limits(1) = max_val;

	  update_type = 'a';
	}

    }

  unwind_protect::protect_var (updating_axis_limits);
  updating_axis_limits = true;

  switch (update_type)
    {
    case 'x':
      xproperties.set_xlim (limits);
      xproperties.set_xlimmode ("auto");
      xproperties.update_xlim ();
      break;

    case 'y':
      xproperties.set_ylim (limits);
      xproperties.set_ylimmode ("auto");
      xproperties.update_ylim ();
      break;

    case 'z':
      xproperties.set_zlim (limits);
      xproperties.set_zlimmode ("auto");
      xproperties.update_zlim ();
      break;

    case 'c':
      xproperties.set_clim (limits);
      xproperties.set_climmode ("auto");
      break;

    case 'a':
      xproperties.set_alim (limits);
      xproperties.set_alimmode ("auto");
      break;

    default:
      break;
    }

  xproperties.update_transform ();

  unwind_protect::run ();
}

inline
double force_in_range (const double x, const double lower, const double upper)
{
  if (x < lower)
    { return lower; }
  else if (x > upper)
    { return upper; }
  else
    { return x; }  
}

void
axes::properties::zoom_about_point (double x, double y, double factor,
                                    bool push_to_zoom_stack)
{
  // FIXME: Do we need error checking here?
  Matrix xlims = get_xlim ().matrix_value ();
  Matrix ylims = get_ylim ().matrix_value ();
              
  // Get children axes limits
  Matrix kids = get_children ();
  double minx = octave_Inf;
  double maxx = -octave_Inf;
  double min_pos_x = octave_Inf;
  get_children_limits (minx, maxx, min_pos_x, kids, 'x');

  double miny = octave_Inf;
  double maxy = -octave_Inf;
  double min_pos_y = octave_Inf;
  get_children_limits (miny, maxy, min_pos_y, kids, 'y');
              
  // Perform the zooming
  xlims (0) = x + factor * (xlims (0) - x);
  xlims (1) = x + factor * (xlims (1) - x);
  ylims (0) = y + factor * (ylims (0) - y);
  ylims (1) = y + factor * (ylims (1) - y);
              
  // Make sure we stay within the range og the plot
  xlims (0) = force_in_range (xlims (0), minx, maxx);
  xlims (1) = force_in_range (xlims (1), minx, maxx);
  ylims (0) = force_in_range (ylims (0), miny, maxy);
  ylims (1) = force_in_range (ylims (1), miny, maxy);

  zoom (xlims, ylims, push_to_zoom_stack);
}

void
axes::properties::zoom (const Matrix& xl, const Matrix& yl, bool push_to_zoom_stack)
{
  if (push_to_zoom_stack)
    {
      zoom_stack.push_front (xlimmode.get ());
      zoom_stack.push_front (xlim.get ());
      zoom_stack.push_front (ylimmode.get ());
      zoom_stack.push_front (ylim.get ());
    }
  
  xlim = xl;
  xlimmode = "manual";
  ylim = yl;
  ylimmode = "manual";

  update_transform ();
  update_xlim (false);
  update_ylim (false);
}

void
axes::properties::translate_view (double delta_x, double delta_y)
{
  // FIXME: Do we need error checking here?
  Matrix xlims = get_xlim ().matrix_value ();
  Matrix ylims = get_ylim ().matrix_value ();
              
  // Get children axes limits
  Matrix kids = get_children ();
  double minx = octave_Inf;
  double maxx = -octave_Inf;
  double min_pos_x = octave_Inf;
  get_children_limits (minx, maxx, min_pos_x, kids, 'x');
        
  double miny = octave_Inf;
  double maxy = -octave_Inf;
  double min_pos_y = octave_Inf;
  get_children_limits (miny, maxy, min_pos_y, kids, 'y');
  
  // Make sure we don't exceed the borders
  if (delta_x > 0)
    delta_x = std::min (xlims (1) + delta_x, maxx) - xlims (1);
  else
    delta_x = std::max (xlims (0) + delta_x, minx) - xlims (0);
  xlims (0) = xlims (0) + delta_x;
  xlims (1) = xlims (1) + delta_x;
                
  if (delta_y > 0)
    delta_y = std::min (ylims (1) + delta_y, maxy) - ylims (1);
  else
    delta_y = std::max (ylims (0) + delta_y, miny) - ylims (0);
  ylims (0) = ylims (0) + delta_y;
  ylims (1) = ylims (1) + delta_y;
                
  zoom (xlims, ylims, false);
}

void
axes::properties::unzoom (void)
{
  if (zoom_stack.size () >= 4)
    {
      ylim = zoom_stack.front ();
      zoom_stack.pop_front ();
      ylimmode = zoom_stack.front ();
      zoom_stack.pop_front ();
      xlim = zoom_stack.front ();
      zoom_stack.pop_front ();
      xlimmode = zoom_stack.front ();
      zoom_stack.pop_front ();

      update_transform ();
      update_xlim (false);
      update_ylim (false);
    }
}

void
axes::properties::clear_zoom_stack (void)
{
  while (zoom_stack.size () > 4)
    zoom_stack.pop_front ();

  unzoom ();
}

// ---------------------------------------------------------------------

Matrix
line::properties::compute_xlim (void) const
{
  Matrix m (1, 3);

  m(0) = xmin (xdata.min_val (), xmin (xldata.min_val (), xudata.min_val ()));
  m(1) = xmax (xdata.max_val (), xmax (xldata.max_val (), xudata.max_val ()));
  m(2) = xmin (xdata.min_pos (), xmin (xldata.min_pos (), xudata.min_pos ()));

  return m;
}

Matrix
line::properties::compute_ylim (void) const
{
  Matrix m (1, 3);

  m(0) = xmin (ydata.min_val (), xmin (ldata.min_val (), udata.min_val ()));
  m(1) = xmax (ydata.max_val (), xmax (ldata.max_val (), udata.max_val ()));
  m(2) = xmin (ydata.min_pos (), xmin (ldata.min_pos (), udata.min_pos ()));

  return m;
}

// ---------------------------------------------------------------------

// Note: "text" code is entirely auto-generated

// ---------------------------------------------------------------------

octave_value
image::properties::get_color_data (void) const
{
  return convert_cdata (*this, get_cdata (),
			cdatamapping_is ("scaled"), 3);
}

// ---------------------------------------------------------------------

octave_value
patch::properties::get_color_data (void) const
{
  return convert_cdata (*this, get_facevertexcdata (),
			cdatamapping_is ("scaled"), 2);
}

// ---------------------------------------------------------------------

octave_value
surface::properties::get_color_data (void) const
{
  return convert_cdata (*this, get_cdata (), cdatamapping_is ("scaled"), 3);
}

inline void
cross_product (double x1, double y1, double z1,
	       double x2, double y2, double z2,
	       double& x, double& y, double& z)
{
  x += (y1 * z2 - z1 * y2);
  y += (z1 * x2 - x1 * z2);
  z += (x1 * y2 - y1 * x2);
}

void
surface::properties::update_normals (void)
{
  if (normalmode_is ("auto"))
    {
      Matrix x = get_xdata ().matrix_value ();
      Matrix y = get_ydata ().matrix_value ();
      Matrix z = get_zdata ().matrix_value ();


      int p = z.columns (), q = z.rows ();
      int i1 = 0, i2 = 0, i3 = 0;
      int j1 = 0, j2 = 0, j3 = 0;

      bool x_mat = (x.rows () == q);
      bool y_mat = (y.columns () == p);

      NDArray n (dim_vector (q, p, 3), 0.0);

      for (int i = 0; i < p; i++)
	{
	  if (y_mat)
	    {
	      i1 = i - 1;
	      i2 = i;
	      i3 = i + 1;
	    }

	  for (int j = 0; j < q; j++)
	    {
	      if (x_mat)
		{
		  j1 = j - 1;
		  j2 = j;
		  j3 = j + 1;
		}

	      double& nx = n(j, i, 0);
	      double& ny = n(j, i, 1);
	      double& nz = n(j, i, 2);

              if ((j > 0) && (i > 0))
                  // upper left quadrangle
	          cross_product (x(j1,i-1)-x(j2,i), y(j-1,i1)-y(j,i2), z(j-1,i-1)-z(j,i),
		                 x(j2,i-1)-x(j1,i), y(j,i1)-y(j-1,i2), z(j,i-1)-z(j-1,i),
			         nx, ny, nz);

              if ((j > 0) && (i < (p -1)))
                  // upper right quadrangle
                  cross_product (x(j1,i+1)-x(j2,i), y(j-1,i3)-y(j,i2), z(j-1,i+1)-z(j,i),
		                 x(j1,i)-x(j2,i+1), y(j-1,i2)-y(j,i3), z(j-1,i)-z(j,i+1),
			         nx, ny, nz);

              if ((j < (q - 1)) && (i > 0))
                  // lower left quadrangle
                  cross_product (x(j2,i-1)-x(j3,i), y(j,i1)-y(j+1,i2), z(j,i-1)-z(j+1,i),
		                 x(j3,i-1)-x(j2,i), y(j+1,i1)-y(j,i2), z(j+1,i-1)-z(j,i),
			         nx, ny, nz);

              if ((j < (q - 1)) && (i < (p -1)))
                  // lower right quadrangle
	          cross_product (x(j3,i)-x(j2,i+1), y(j+1,i2)-y(j,i3), z(j+1,i)-z(j,i+1),
                                 x(j3,i+1)-x(j2,i), y(j+1,i3)-y(j,i2), z(j+1,i+1)-z(j,i),
			         nx, ny, nz);

              double d = - std::max(std::max(fabs(nx), fabs(ny)), fabs(nz));

	      nx /= d;
	      ny /= d;
	      nz /= d;
	    }
	}
      vertexnormals = n;
    }
}

// ---------------------------------------------------------------------

void
hggroup::update_axis_limits (const std::string& axis_type)
{
  Matrix kids = xproperties.get_children ();

  double min_val = octave_Inf;
  double max_val = -octave_Inf;
  double min_pos = octave_Inf;

  char update_type = 0;

  if (axis_type == "xlim" || axis_type == "xliminclude")
    {
      get_children_limits (min_val, max_val, min_pos, kids, 'x');
      
      update_type = 'x';
    }
  else if (axis_type == "ylim" || axis_type == "yliminclude")
    {
      get_children_limits (min_val, max_val, min_pos, kids, 'y');

      update_type = 'y';
    }
  else if (axis_type == "zlim" || axis_type == "zliminclude")
    {
      get_children_limits (min_val, max_val, min_pos, kids, 'z');

      update_type = 'z';
    }
  else if (axis_type == "clim" || axis_type == "climinclude")
    {
      get_children_limits (min_val, max_val, min_pos, kids, 'c');

      update_type = 'c';

    }
  else if (axis_type == "alim" || axis_type == "aliminclude")
    {
      get_children_limits (min_val, max_val, min_pos, kids, 'a');

      update_type = 'a';
    }

  Matrix limits (1, 3, 0.0);

  limits(0) = min_val;
  limits(1) = max_val;
  limits(2) = min_pos;

  switch (update_type)
    {
    case 'x':
      xproperties.set_xlim (limits);
      break;

    case 'y':
      xproperties.set_ylim (limits);
      break;

    case 'z':
      xproperties.set_zlim (limits);
      break;

    case 'c':
      xproperties.set_clim (limits);
      break;

    case 'a':
      xproperties.set_alim (limits);
      break;

    default:
      break;
    }

  base_graphics_object::update_axis_limits (axis_type);
}

// ---------------------------------------------------------------------

octave_value
base_graphics_object::get_default (const caseless_str& name) const
{
  graphics_handle parent = get_parent ();
  graphics_object parent_obj = gh_manager::get_object (parent);

  return parent_obj.get_default (type () + name);
}

octave_value
base_graphics_object::get_factory_default (const caseless_str& name) const
{
  graphics_object parent_obj = gh_manager::get_object (0);

  return parent_obj.get_factory_default (type () + name);
}

// We use a random value for the handle to avoid issues with plots and
// scalar values for the first argument.
gh_manager::gh_manager (void)
  : handle_map (), handle_free_list (),
    next_handle (-1.0 - (rand () + 1.0) / (RAND_MAX + 2.0))
{
  handle_map[0] = graphics_object (new root_figure ());

  // Make sure the default backend is registered.
  graphics_backend::default_backend ();
}

graphics_handle
gh_manager::do_make_graphics_handle (const std::string& go_name,
				     const graphics_handle& p, bool do_createfcn)
{
  graphics_handle h = get_handle (go_name);

  base_graphics_object *go = 0;

  go = make_graphics_object_from_type (go_name, h, p);
  
  if (go)
    {
      graphics_object obj (go);

      handle_map[h] = obj;
      if (do_createfcn)
        go->get_properties ().execute_createfcn ();

      // notify backend
      graphics_backend backend = go->get_backend ();
      if (backend)
        backend.object_created (obj);
    }
  else
    error ("gh_manager::do_make_graphics_handle: invalid object type `%s'",
	   go_name.c_str ());

  return h;
}

graphics_handle
gh_manager::do_make_figure_handle (double val)
{
  graphics_handle h = val;

  base_graphics_object* go = new figure (h, 0);
  graphics_object obj (go);

  handle_map[h] = obj;

  // notify backend
  graphics_backend backend = go->get_backend ();
  if (backend)
    backend.object_created (obj);
  
  return h;
}

void
gh_manager::do_push_figure (const graphics_handle& h)
{
  do_pop_figure (h);

  figure_list.push_front (h);
}

void
gh_manager::do_pop_figure (const graphics_handle& h)
{
  for (figure_list_iterator p = figure_list.begin ();
       p != figure_list.end ();
       p++)
    {
      if (*p == h)
	{
	  figure_list.erase (p);
	  break;
	}
    }
}

class
callback_event : public base_graphics_event
{
public:
  callback_event (const graphics_handle& h, const std::string& name,
		  const octave_value& data = Matrix ())
      : base_graphics_event (), handle (h), callback_name (name),
        callback_data (data) { }

  void execute (void)
    {
      gh_manager::execute_callback (handle, callback_name, callback_data);
    }

private:
  callback_event (void)
      : base_graphics_event () { }

private:
  graphics_handle handle;
  std::string callback_name;
  octave_value callback_data;
};

class
function_event : public base_graphics_event
{
public:
  function_event (graphics_event::event_fcn fcn, void* data = 0)
      : base_graphics_event (), function (fcn),
        function_data (data) { }

  void execute (void)
    {
      function (function_data);
    }

private:
  function_event (void)
      : base_graphics_event () { }

private:
  graphics_event::event_fcn function;
  void* function_data;
};

class
set_event : public base_graphics_event
{
public:
  set_event (const graphics_handle& h, const std::string& name,
	     const octave_value& value)
      : base_graphics_event (), handle (h), property_name (name),
        property_value (value) { }

  void execute (void)
    {
      gh_manager::autolock guard;

      xset (handle, property_name, property_value);
    }

private:
  set_event (void)
      : base_graphics_event () { }

private:
  graphics_handle handle;
  std::string property_name;
  octave_value property_value;
};

graphics_event
graphics_event::create_callback_event (const graphics_handle& h,
				       const std::string& name,
				       const octave_value& data)
{
  graphics_event e;

  e.rep = new callback_event (h, name, data);

  return e;
}

graphics_event
graphics_event::create_function_event (graphics_event::event_fcn fcn,
				       void *data)
{
  graphics_event e;

  e.rep = new function_event (fcn, data);

  return e;
}

graphics_event
graphics_event::create_set_event (const graphics_handle& h,
				  const std::string& name,
				  const octave_value& data)
{
  graphics_event e;

  e.rep = new set_event (h, name, data);

  return e;
}

static void
xset_gcbo (const graphics_handle& h)
{
  graphics_object go = gh_manager::get_object (0);
  root_figure::properties& props =
      dynamic_cast<root_figure::properties&> (go.get_properties ());

  props.set_callbackobject (h.as_octave_value ());
}

void
gh_manager::do_restore_gcbo (void)
{
  gh_manager::autolock guard;

  callback_objects.pop_front ();

  xset_gcbo (callback_objects.empty ()
	     ? graphics_handle ()
	     : callback_objects.front ().get_handle ());
}

void
gh_manager::do_execute_callback (const graphics_handle& h,
				 const octave_value& cb_arg,
				 const octave_value& data)
{
  octave_value_list args;
  octave_function *fcn = 0;

  args(0) = h.as_octave_value ();
  if (data.is_defined ())
    args(1) = data;
  else
    args(1) = Matrix ();

  unwind_protect::frame_id_t uwp_frame = unwind_protect::begin_frame ();
  unwind_protect::add_fcn (gh_manager::restore_gcbo);

  if (true)
    {
      gh_manager::autolock guard;
  
      callback_objects.push_front (get_object (h));
      xset_gcbo (h);
    }

  BEGIN_INTERRUPT_WITH_EXCEPTIONS;

  // Copy CB because "function_value" method is non-const.

  octave_value cb = cb_arg;

  if (cb.is_function_handle ())
    fcn = cb.function_value ();
  else if (cb.is_string ())
    {
      int status;
      std::string s = cb.string_value ();

      eval_string (s, false, status);
    }
  else if (cb.is_cell () && cb.length () > 0
           && (cb.rows () == 1 || cb.columns () == 1)
           && cb.cell_value ()(0).is_function_handle ())
    {
      Cell c = cb.cell_value ();

      fcn = c(0).function_value ();
      if (! error_state)
        {
          for (int i = 1; i < c.length () ; i++)
            args(1+i) = c(i);
        }
    }
  else
    {
      std::string nm = cb.class_name ();
      error ("trying to execute non-executable object (class = %s)",
	     nm.c_str ());
    }

  if (fcn && ! error_state)
    feval (fcn, args);
  
  END_INTERRUPT_WITH_EXCEPTIONS;

  unwind_protect::run_frame (uwp_frame);
}

void
gh_manager::do_post_event (const graphics_event& e)
{
  event_queue.push_back (e);

  command_editor::add_event_hook (gh_manager::process_events);
}

void
gh_manager::do_post_callback (const graphics_handle& h, const std::string name,
			      const octave_value& data)
{
  gh_manager::autolock guard;

  graphics_object go = get_object (h);

  if (go.valid_object ())
    {
      if (callback_objects.empty ())
	do_post_event (graphics_event::create_callback_event (h, name, data));
      else
	{
	  const graphics_object& current = callback_objects.front ();

	  if (current.get_properties ().is_interruptible ())
	    do_post_event (graphics_event::create_callback_event (h, name, data));
	  else
	    {
	      caseless_str busy_action (go.get_properties ().get_busyaction ());

	      if (busy_action.compare ("queue"))
		do_post_event (graphics_event::create_callback_event (h, name, data));
	      else
		{
		  caseless_str cname (name);

		  if (cname.compare ("deletefcn")
		      || cname.compare ("createfcn")
		      || (go.isa ("figure")
			  && (cname.compare ("closerequestfcn")
			      || cname.compare ("resizefcn"))))
		    do_post_event (graphics_event::create_callback_event (h, name, data));
		}
	    }
	}
    }
}

void
gh_manager::do_post_function (graphics_event::event_fcn fcn, void* fcn_data)
{
  gh_manager::autolock guard;

  do_post_event (graphics_event::create_function_event (fcn, fcn_data));
}

void
gh_manager::do_post_set (const graphics_handle& h, const std::string name,
			 const octave_value& value)
{
  gh_manager::autolock guard;

  do_post_event (graphics_event::create_set_event (h, name, value));
}

int
gh_manager::do_process_events (bool force)
{
  graphics_event e;

  do
    {
      e = graphics_event ();

      gh_manager::lock ();

      if (! event_queue.empty ())
	{
	  if (callback_objects.empty () || force)
	    {
	      e = event_queue.front ();
	      
	      event_queue.pop_front ();
	    }
	  else
	    {
	      const graphics_object& go = callback_objects.front ();

	      if (go.get_properties ().is_interruptible ())
		{
		  e = event_queue.front ();

		  event_queue.pop_front ();
		}
	    }
	}

      gh_manager::unlock ();

      if (e.ok ())
	e.execute ();
    }
  while (e.ok ());

  gh_manager::lock ();

  if (event_queue.empty ())
    command_editor::remove_event_hook (gh_manager::process_events);

  gh_manager::unlock ();

  return 0;
}

property_list::plist_map_type
root_figure::init_factory_properties (void)
{
  property_list::plist_map_type plist_map;

  plist_map["figure"] = figure::properties::factory_defaults ();
  plist_map["axes"] = axes::properties::factory_defaults ();
  plist_map["line"] = line::properties::factory_defaults ();
  plist_map["text"] = text::properties::factory_defaults ();
  plist_map["image"] = image::properties::factory_defaults ();
  plist_map["patch"] = patch::properties::factory_defaults ();
  plist_map["surface"] = surface::properties::factory_defaults ();
  plist_map["hggroup"] = hggroup::properties::factory_defaults ();

  return plist_map;
}

// ---------------------------------------------------------------------

DEFUN (ishandle, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ishandle (@var{h})\n\
Return true if @var{h} is a graphics handle and false otherwise.\n\
@end deftypefn")
{
  gh_manager::autolock guard;

  octave_value retval;

  if (args.length () == 1)
    retval = is_handle (args(0));
  else
    print_usage ();

  return retval;
}

DEFUN (set, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} set (@var{h}, @var{p}, @var{v}, @dots{})\n\
Set the named property value or vector @var{p} to the value @var{v}\n\
for the graphics handle @var{h}.\n\
@end deftypefn")
{
  gh_manager::autolock guard;

  octave_value retval;

  int nargin = args.length ();

  if (nargin > 0)
    {
      ColumnVector hcv (args(0).vector_value ());

      if (! error_state)
        {
	  bool request_drawnow = false;

          for (octave_idx_type n = 0; n < hcv.length (); n++) 
            {
              graphics_object obj = gh_manager::get_object (hcv(n));

              if (obj)
                {
                  obj.set (args.splice (0, 1));

                  request_drawnow = true;
                }
              else
		{
		  error ("set: invalid handle (= %g)", hcv(n));
		  break;
		}
            }

	  if (! error_state && request_drawnow)
	    Vdrawnow_requested = true;
        }
      else
        error ("set: expecting graphics handle as first argument");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (get, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} get (@var{h}, @var{p})\n\
Return the named property @var{p} from the graphics handle @var{h}.\n\
If @var{p} is omitted, return the complete property list for @var{h}.\n\
If @var{h} is a vector, return a cell array including the property\n\
values or lists respectively.\n\
@end deftypefn")
{
  gh_manager::autolock guard;

  octave_value retval;

  Cell vals;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      ColumnVector hcv (args(0).vector_value ());

      if (! error_state)
        {
	  octave_idx_type len = hcv.length ();

	  vals.resize (dim_vector (len, 1));

          for (octave_idx_type n = 0; n < len; n++)
            {
              graphics_object obj = gh_manager::get_object (hcv(n));

              if (obj)
                {
                  if (nargin == 1)
                    vals(n) = obj.get ();
                  else
                    {
                      caseless_str property = args(1).string_value ();

                      if (! error_state)
                        vals(n) = obj.get (property);
                      else
			{
			  error ("get: expecting property name as second argument");
			  break;
			}
                    }
                }
              else
		{
		  error ("get: invalid handle (= %g)", hcv(n));
		  break;
		}
            }
        }
      else
        error ("get: expecting graphics handle as first argument");
    }
  else
    print_usage ();

  if (! error_state)
    {
      octave_idx_type len = vals.numel ();

      if (len > 1)
	retval = vals;
      else if (len == 1)
	retval = vals(0);
    }

  return retval;
}

// Return all properties from the graphics handle @var{h}.
// If @var{h} is a vector, return a cell array including the
// property values or lists respectively.

DEFUN (__get__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __get__ (@var{h})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  gh_manager::autolock guard;

  octave_value retval;

  Cell vals;

  int nargin = args.length ();

  if (nargin == 1)
    {
      ColumnVector hcv (args(0).vector_value ());

      if (! error_state)
        {
          octave_idx_type len = hcv.length ();

          vals.resize (dim_vector (len, 1));

          for (octave_idx_type n = 0; n < len; n++)
            {
              graphics_object obj = gh_manager::get_object (hcv(n));

              if (obj)
                vals(n) = obj.get (true);
              else
                {
                  error ("get: invalid handle (= %g)", hcv(n));
                  break;
                }
            }
        }
      else
        error ("get: expecting graphics handle as first argument");
    }
  else
    print_usage ();

  if (! error_state)
    {
      octave_idx_type len = vals.numel ();

      if (len > 1)
        retval = vals;
      else if (len == 1)
        retval = vals(0);
    }

  return retval;
}

static octave_value
make_graphics_object (const std::string& go_name,
		      const octave_value_list& args)
{
  octave_value retval;

  double val = octave_NaN;

  octave_value_list xargs = args.splice (0, 1);

  caseless_str p ("parent");

  for (int i = 0; i < xargs.length (); i++)
    if (xargs(i).is_string ()
	&& p.compare (xargs(i).string_value ()))
      {
	if (i < (xargs.length () - 1))
	  {
	    val = xargs(i+1).double_value ();

	    if (! error_state)
	      {
		xargs = xargs.splice (i, 2);
		break;
	      }
	  }
	else
	  error ("__go_%s__: missing value for parent property",
		 go_name.c_str ());
      }

  if (! error_state && xisnan (val))
    val = args(0).double_value ();

  if (! error_state)
    {
      graphics_handle parent = gh_manager::lookup (val);

      if (parent.ok ())
	{
	  graphics_handle h
	    = gh_manager::make_graphics_handle (go_name, parent, false);

	  if (! error_state)
	    {
	      adopt (parent, h);

	      xset (h, xargs);
	      xcreatefcn (h);

	      retval = h.value ();

	      if (! error_state)
		Vdrawnow_requested = true;
	    }
	  else
	    error ("__go%s__: unable to create graphics handle",
		   go_name.c_str ());
	}
      else
	error ("__go_%s__: invalid parent", go_name.c_str ());
    }
  else
    error ("__go_%s__: invalid parent", go_name.c_str ());

  return retval;
}

DEFUN (__go_figure__, args, ,
   "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_figure__ (@var{fignum})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  gh_manager::autolock guard;

  octave_value retval;

  if (args.length () > 0)
    {
      double val = args(0).double_value ();

      if (! error_state)
	{
	  if (is_figure (val))
	    {
	      graphics_handle h = gh_manager::lookup (val);

	      xset (h, args.splice (0, 1));

	      retval = h.value ();
	    }
	  else
	    {
	      graphics_handle h = octave_NaN;

	      if (xisnan (val))
		h = gh_manager::make_graphics_handle ("figure", 0, false);
	      else if (val > 0 && D_NINT (val) == val)
		h = gh_manager::make_figure_handle (val);
	      else
		error ("__go_figure__: invalid figure number");

	      if (! error_state && h.ok ())
		{
		  adopt (0, h);

		  xset (h, args.splice (0, 1));
		  xcreatefcn (h);

		  retval = h.value ();
		}
	      else
		error ("__go_figure__: failed to create figure handle");
	    }
	}
      else
	error ("__go_figure__: expecting figure number to be double value");
    }
  else
    print_usage ();

  return retval;
}

#define GO_BODY(TYPE) \
  gh_manager::autolock guard; \
 \
  octave_value retval; \
 \
  if (args.length () > 0) \
    retval = make_graphics_object (#TYPE, args); \
  else \
    print_usage (); \
 \
  return retval

DEFUN (__go_axes__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_axes__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (axes);
}

DEFUN (__go_line__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_line__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (line);
}

DEFUN (__go_text__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_text__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (text);
}

DEFUN (__go_image__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_image__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (image);
}

DEFUN (__go_surface__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_surface__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (surface);
}

DEFUN (__go_patch__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_patch__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (patch);
}

DEFUN (__go_hggroup__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_hggroup__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (hggroup);
}

DEFUN (__go_delete__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_delete__ (@var{h})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  gh_manager::autolock guard;

  octave_value_list retval;

  if (args.length () == 1)
    {
      graphics_handle h = octave_NaN;

      const NDArray vals = args (0).array_value ();

      if (! error_state)
	{
	  // Check is all the handles to delete are valid first
	  // as callbacks might delete one of the handles we
	  // later want to delete
	  for (octave_idx_type i = 0; i < vals.numel (); i++)
	    {
	      h = gh_manager::lookup (vals.elem (i));

	      if (! h.ok ())
		{
		  error ("delete: invalid graphics object (= %g)", 
			 vals.elem (i));
		  break;
		}
	    }

	  if (! error_state)
	    {
	      for (octave_idx_type i = 0; i < vals.numel (); i++)
		{
		  h = gh_manager::lookup (vals.elem (i));

		  if (h.ok ())
		    {
		      graphics_object obj = gh_manager::get_object (h);

		      // Don't do recursive deleting, due to callbacks
		      if (! obj.get_properties ().is_beingdeleted ())
			{
			  graphics_handle parent_h = obj.get_parent ();

			  graphics_object parent_obj = 
			    gh_manager::get_object (parent_h);

			  // NOTE: free the handle before removing it from its
			  //       parent's children, such that the object's 
			  //       state is correct when the deletefcn callback
			  //       is executed

			  gh_manager::free (h);

			  // A callback function might have already deleted 
			  // the parent
			  if (parent_obj.valid_object ())
			    parent_obj.remove_child (h);

			  Vdrawnow_requested = true;
			}
		    }
		}
	    }
	}
      else
	error ("delete: invalid graphics object");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (__go_axes_init__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_axes_init__ (@var{h}, @var{mode})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  gh_manager::autolock guard;

  octave_value retval;

  int nargin = args.length ();

  std::string mode = "";

  if (nargin == 2)
    {
      mode = args(1).string_value ();

      if (error_state)
	return retval;
    }

  if (nargin == 1 || nargin == 2)
    {
      graphics_handle h = octave_NaN;

      double val = args(0).double_value ();

      if (! error_state)
	{
	  h = gh_manager::lookup (val);

	  if (h.ok ())
	    {
	      graphics_object obj = gh_manager::get_object (h);

	      obj.set_defaults (mode);

	      h = gh_manager::lookup (val);
	      if (! h.ok ())
		error ("__go_axes_init__: axis deleted during initialization (= %g)", val);
	    }
	  else
	    error ("__go_axes_init__: invalid graphics object (= %g)", val);
	}
      else
	error ("__go_axes_init__: invalid graphics object");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (__go_handles__, , ,
   "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_handles__ ()\n\
Undocumented internal function.\n\
@end deftypefn")
{
  gh_manager::autolock guard;

  return octave_value (gh_manager::handle_list ());
}

DEFUN (__go_figure_handles__, , ,
   "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_figure_handles__ ()\n\
Undocumented internal function.\n\
@end deftypefn")
{
  gh_manager::autolock guard;

  return octave_value (gh_manager::figure_handle_list ());
}

DEFUN (__go_execute_callback__, args, ,
   "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_execute_callback__ (@var{h}, @var{name})\n\
@deftypefnx {Built-in Function} {} __go_execute_callback__ (@var{h}, @var{name}, @var{param})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2 || nargin == 3)
    {
      double val = args(0).double_value ();

      if (! error_state)
	{
	  graphics_handle h = gh_manager::lookup (val);

	  if (h.ok ())
	    {
	      std::string name = args(1).string_value ();

	      if (! error_state)
		{
		  if (nargin == 2)
		    gh_manager::execute_callback (h, name);
		  else
		    gh_manager::execute_callback (h, name, args(2));
		}
	      else
		error ("__go_execute_callback__: invalid callback name");
	    }
	  else
	    error ("__go_execute_callback__: invalid graphics object (= %g)",
		   val);
	}
      else
	error ("__go_execute_callback__: invalid graphics object");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (available_backends, , ,
   "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} available_backends ()\n\
Return a cell array of registered graphics backends.\n\
@end deftypefn")
{
  gh_manager::autolock guard;

  return octave_value (graphics_backend::available_backends_list ());
}

DEFUN (drawnow, args, ,
   "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} drawnow ()\n\
@deftypefnx {Built-in Function} {} drawnow (\"expose\")\n\
@deftypefnx {Built-in Function} {} drawnow (@var{term}, @var{file}, @var{mono}, @var{debug_file})\n\
Update figure windows and their children.  The event queue is flushed and\n\
any callbacks generated are executed.  With the optional argument\n\
@code{\"expose\"}, only graphic objects are updated and no other events or\n\
callbacks are processed.\n\
The third calling form of @code{drawnow} is for debugging and is\n\
undocumented.\n\
@end deftypefn")
{
  static int drawnow_executing = 0;
  static bool __go_close_all_registered__ = false;

  octave_value retval;

  gh_manager::lock ();

  unwind_protect::frame_id_t uwp_frame = unwind_protect::begin_frame ();
  unwind_protect::protect_var (Vdrawnow_requested, false);

  unwind_protect::protect_var (drawnow_executing);

  if (++drawnow_executing <= 1)
    {
      if (! __go_close_all_registered__)
	{
	  octave_add_atexit_function ("__go_close_all__");

	  __go_close_all_registered__ = true;
	}

      if (args.length () == 0 || args.length () == 1)
	{
	  Matrix hlist = gh_manager::figure_handle_list ();

	  for (int i = 0; ! error_state && i < hlist.length (); i++)
	    {
	      graphics_handle h = gh_manager::lookup (hlist(i));

	      if (h.ok () && h != 0)
		{
		  graphics_object go = gh_manager::get_object (h);
		  figure::properties& fprops = dynamic_cast <figure::properties&> (go.get_properties ());

		  if (fprops.is_modified ())
		    {
		      if (fprops.is_visible ())
			{
			  gh_manager::unlock ();

			  fprops.get_backend ().redraw_figure (go);

			  gh_manager::lock ();
			}

		      fprops.set_modified (false);
		    }
		}
	    }

	  bool do_events = true;

	  if (args.length () == 1)
	    {
	      caseless_str val (args(0).string_value ());

	      if (! error_state && val.compare ("expose"))
		do_events = false;
	      else
		{
		  error ("drawnow: invalid argument, expected `expose' as argument");
		  return retval;
		}
	    }

	  if (do_events)
	    {
	      gh_manager::unlock ();

	      gh_manager::process_events ();

	      gh_manager::lock ();
	    }
	}
      else if (args.length () >= 2 && args.length () <= 4)
	{
	  std::string term, file, debug_file;
	  bool mono;

	  term = args(0).string_value ();

	  if (! error_state)
	    {
	      file = args(1).string_value ();

	      if (! error_state)
		{
		  size_t pos = file.find_last_of (file_ops::dir_sep_chars ());

		  if (pos != std::string::npos)
		    {
		      std::string dirname = file.substr (0, pos+1);

		      file_stat fs (dirname);

		      if (! (fs && fs.is_dir ()))
			{
			  error ("drawnow: nonexistent directory `%s'",
				 dirname.c_str ());

			  return retval;
			}
		    }

		  mono = (args.length () >= 3 ? args(2).bool_value () : false);

		  if (! error_state)
		    {
		      debug_file = (args.length () > 3 ? args(3).string_value ()
				    : "");

		      if (! error_state)
			{
			  graphics_handle h = gcf ();

			  if (h.ok ())
			    {
			      graphics_object go = gh_manager::get_object (h);

			      gh_manager::unlock ();

			      go.get_backend ()
				.print_figure (go, term, file, mono, debug_file);

			      gh_manager::lock ();
			    }
			  else
			    error ("drawnow: nothing to draw");
			}
		      else
			error ("drawnow: invalid debug_file, expected a string value");
		    }
		  else
		    error ("drawnow: invalid colormode, expected a boolean value");
		}
	      else
		error ("drawnow: invalid file, expected a string value");
	    }
	  else
	    error ("drawnow: invalid terminal, expected a string value");
	}
      else
	print_usage ();
    }

  unwind_protect::run_frame (uwp_frame);

  gh_manager::unlock ();

  return retval;
}

DEFUN (addlistener, args, ,
   "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} addlistener (@var{h}, @var{prop}, @var{fcn})\n\
Register @var{fcn} as listener for the property @var{prop} of the graphics\n\
object @var{h}.  Property listeners are executed (in order of registration)\n\
when the property is set.  The new value is already available when the\n\
listeners are executed.\n\
\n\
@var{prop} must be a string naming a valid property in @var{h}.\n\
\n\
@var{fcn} can be a function handle, a string or a cell array whose first\n\
element is a function handle.  If @var{fcn} is a function handle, the\n\
corresponding function should accept at least 2 arguments, that will be\n\
set to the object handle and the empty matrix respectively.  If @var{fcn}\n\
is a string, it must be any valid octave expression.  If @var{fcn} is a cell\n\
array, the first element must be a function handle with the same signature\n\
as described above.  The next elements of the cell array are passed\n\
as additional arguments to the function.\n\
\n\
Example:\n\
\n\
@example\n\
@group\n\
function my_listener (h, dummy, p1)\n\
  fprintf (\"my_listener called with p1=%s\\n\", p1);\n\
endfunction\n\
\n\
addlistener (gcf, \"position\", @{@@my_listener, \"my string\"@})\n\
@end group\n\
@end example\n\
\n\
@end deftypefn")
{
  gh_manager::autolock guard;

  octave_value retval;

  if (args.length () == 3)
    {
      double h = args(0).double_value ();

      if (! error_state)
	{
	  std::string pname = args(1).string_value ();

	  if (! error_state)
	    {
	      graphics_handle gh = gh_manager::lookup (h);

	      if (gh.ok ())
		{
		  graphics_object go = gh_manager::get_object (gh);

		  go.add_property_listener (pname, args(2), POSTSET);
		}
	      else
		error ("addlistener: invalid graphics object (= %g)",
		       h);
	    }
	  else
	    error ("addlistener: invalid property name, expected a string value");
	}
      else
	error ("addlistener: invalid handle");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (dellistener, args, ,
   "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} dellistener (@var{h}, @var{prop}, @var{fcn})\n\
Remove the registration of @var{fcn} as a listener for the property\n\
@var{prop} of the graphics object @var{h}.  The function @var{fcn} must\n\
be the same variable (not just the same value), as was passed to the\n\
original call to @code{addlistener}.\n\
\n\
If @var{fcn} is not defined then all listener functions of @var{prop}\n\
are removed.\n\
\n\
Example:\n\
\n\
@example\n\
@group\n\
function my_listener (h, dummy, p1)\n\
  fprintf (\"my_listener called with p1=%s\\n\", p1);\n\
endfunction\n\
\n\
c = @{@@my_listener, \"my string\"@};\n\
addlistener (gcf, \"position\", c);\n\
dellistener (gcf, \"position\", c);\n\
@end group\n\
@end example\n\
\n\
@end deftypefn")
{
  gh_manager::autolock guard;

  octave_value retval;

  if (args.length () == 3 || args.length () == 2)
    {
      double h = args(0).double_value ();

      if (! error_state)
	{
	  std::string pname = args(1).string_value ();

	  if (! error_state)
	    {
	      graphics_handle gh = gh_manager::lookup (h);

	      if (gh.ok ())
		{
		  graphics_object go = gh_manager::get_object (gh);

		  if (args.length () == 2)
		    go.delete_property_listener (pname, octave_value (), POSTSET);
		  else
		    go.delete_property_listener (pname, args(2), POSTSET);
		}
	      else
		error ("dellistener: invalid graphics object (= %g)",
		       h);
	    }
	  else
	    error ("dellistener: invalid property name, expected a string value");
	}
      else
	error ("dellistener: invalid handle");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (addproperty, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} addproperty (@var{name}, @var{h}, @var{type}, [@var{arg}, @dots{}])\n\
Create a new property named @var{name} in graphics object @var{h}.\n\
@var{type} determines the type of the property to create.  @var{args}\n\
usually contains the default value of the property, but additional\n\
arguments might be given, depending on the type of the property.\n\
\n\
The supported property types are:\n\
\n\
@table @code\n\
@item string\n\
A string property.  @var{arg} contains the default string value.\n\
@item any\n\
An un-typed property.  This kind of property can hold any octave\n\
value.  @var{args} contains the default value.\n\
@item radio\n\
A string property with a limited set of accepted values.  The first\n\
argument must be a string with all accepted values separated by\n\
a vertical bar ('|').  The default value can be marked by enclosing\n\
it with a '@{' '@}' pair.  The default value may also be given as\n\
an optional second string argument.\n\
@item boolean\n\
A boolean property.  This property type is equivalent to a radio\n\
property with \"on|off\" as accepted values.  @var{arg} contains\n\
the default property value.\n\
@item double\n\
A scalar double property.  @var{arg} contains the default value.\n\
@item handle\n\
A handle property.  This kind of property holds the handle of a\n\
graphics object.  @var{arg} contains the default handle value.\n\
When no default value is given, the property is initialized to\n\
the empty matrix.\n\
@item data\n\
A data (matrix) property.  @var{arg} contains the default data\n\
value.  When no default value is given, the data is initialized to\n\
the empty matrix.\n\
@item color\n\
A color property.  @var{arg} contains the default color value.\n\
When no default color is given, the property is set to black.\n\
An optional second string argument may be given to specify an\n\
additional set of accepted string values (like a radio property).\n\
@end table\n\
\n\
@var{type} may also be the concatenation of a core object type and\n\
a valid property name for that object type.  The property created\n\
then has the same characteristics as the referenced property (type,\n\
possible values, hidden state@dots{}).  This allows to clone an existing\n\
property into the graphics object @var{h}.\n\
\n\
Examples:\n\
\n\
@example\n\
@group\n\
addproperty (\"my_property\", gcf, \"string\", \"a string value\");\n\
addproperty (\"my_radio\", gcf, \"radio\", \"val_1|val_2|@{val_3@}\");\n\
addproperty (\"my_style\", gcf, \"linelinestyle\", \"--\");\n\
@end group\n\
@end example\n\
\n\
@end deftypefn")
{
  gh_manager::autolock guard;

  octave_value retval;

  if (args.length () >= 3)
    {
      std::string name = args(0).string_value ();

      if (! error_state)
	{
	  double h = args(1).double_value ();

	  if (! error_state)
	    {
	      graphics_handle gh = gh_manager::lookup (h);

	      if (gh.ok ())
		{
		  graphics_object go = gh_manager::get_object (gh);

		  std::string type = args(2).string_value ();

		  if (! error_state)
		    {
		      if (! go.get_properties ().has_property (name))
			{
			  property p = property::create (name, gh, type,
							 args.splice (0, 3));

			  if (! error_state)
			    go.get_properties ().insert_property (name, p);
			}
		      else
			error ("addproperty: a `%s' property already exists in the graphics object",
			       name.c_str ());
		    }
		  else
		    error ("addproperty: invalid property type, expected a string value");
		}
	      else
		error ("addproperty: invalid graphics object (= %g)", h);
	    }
	  else
	    error ("addproperty: invalid handle value");
	}
      else
	error ("addproperty: invalid property name, expected a string value");
    }
  else
    print_usage ();

  return retval;
}

octave_value
get_property_from_handle (double handle, const std::string& property,
			  const std::string& func)
{
  gh_manager::autolock guard;

  graphics_object obj = gh_manager::get_object (handle);
  octave_value retval;

  if (obj)
    retval = obj.get (caseless_str (property));
  else
    error ("%s: invalid handle (= %g)", func.c_str(), handle);

  return retval;
}

bool
set_property_in_handle (double handle, const std::string& property,
			const octave_value& arg, const std::string& func)
{
  gh_manager::autolock guard;

  graphics_object obj = gh_manager::get_object (handle);
  int ret = false;

  if (obj)
    {
      obj.set (caseless_str (property), arg);

      if (! error_state)
	ret = true;
    }
  else
    error ("%s: invalid handle (= %g)", func.c_str(), handle);

  return ret;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
