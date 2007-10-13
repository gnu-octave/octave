/*

Copyright (C) 2007 John W. Eaton

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

#include <algorithm>
#include <list>
#include <map>
#include <set>
#include <string>

#include "defun.h"
#include "error.h"
#include "graphics.h"
#include "ov.h"
#include "oct-obj.h"
#include "oct-map.h"
#include "ov-fcn-handle.h"
#include "parse.h"

static void
gripe_set_invalid (const std::string& pname)
{
  error ("set: invalid value for %s property", pname.c_str ());
}

// ---------------------------------------------------------------------

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
  else if (str.compare(0, len, "black", 0, len) == 0 || 
	   str.compare(0, len, "k", 0, len) == 0)
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
  else if (str.compare(0, len, "white", 0, len) == 0 ||
	   str.compare(0, len, "w", 0, len) == 0)
    tmp_rgb[0] = tmp_rgb[1] = tmp_rgb[2] = 1;
  else	
    retval = false;

  if (retval)
    {
      for (int i = 0; i < 3; i++)
	xrgb[i] = tmp_rgb[i];
    }

  return retval;
}

color_property::color_property (const octave_value& val)
  : radio_val (), current_val ()
{
  // FIXME -- need some error checking here.

  if (val.is_string ())
    {
      std::string s = val.string_value ();

      if (! s.empty ())
	{
	  color_values col (s);
	  if (! error_state)
	    {
	      color_val = col;
	      current_type = color_t;
	    }
	}
      else
	error ("invalid color specification");	  
    }
  else if (val.is_real_matrix ())
    {
      Matrix m = val.matrix_value ();

      if (m.numel () == 3)
	{
	  color_values col (m (0), m (1), m(2));
	  if (! error_state)
	    {
	      color_val = col;
	      current_type = color_t;
	    }
	}
      else
	error ("invalid color specification");
    }
  else 
    error ("invalid color specification");
}

// We also provide this assignment operator so that assignment from an
// octave_value object can happen without wiping out list of possible
// radio_values set in color_property constructor.

color_property&
color_property::operator = (const octave_value& val)
{
  if (val.is_string ())
    {
      std::string s = val.string_value ();

      if (! s.empty ())
	{
	  if (radio_val.contains (s))
	    {
	      current_val = s;
	      current_type = radio_t;
	    }
          else
	    {
	      color_values col (s);
	      if (! error_state)
		{
		  color_val = col;
		  current_type = color_t;
		}
	      else
		error ("invalid color specification");	  
	    }	
	}
      else
	error ("invalid color specification");	  
    }
  else if (val.is_real_matrix ())
    {
      Matrix m = val.matrix_value ();

      if (m.numel () == 3)
	{
	  color_values col (m (0), m (1), m(2));
	  if (! error_state)
	    {
	      color_val = col;
	      current_type = color_t;
	    }
	}
      else
	error ("invalid color specification");
    }
  else 
    error ("invalid color specification");

  return *this;
}


void
property_list::set (const property_name& name, const octave_value& val)
{
  size_t offset = 0;

  size_t len = name.length ();

  if (len > 4)
    {
      property_name pfx = name.substr (0, 4);

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

		  if (pfx.compare ("surface"))
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

	  bool remove = false;
	  if (val.is_string ())
	    {
	      property_name tval = val.string_value ();

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
    }

  if (offset == 0)
    error ("invalid default property specification");
}

octave_value
property_list::lookup (const property_name& name) const
{
  octave_value retval;

  size_t offset = 0;

  size_t len = name.length ();

  if (len > 4)
    {
      property_name pfx = name.substr (0, 4);

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

		  if (pfx.compare ("surface"))
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
	  property_name name = args(i).string_value ();

	  if (! error_state)
	    {
	      octave_value val = args(i+1);

	      if (val.is_string ())
		{
		  property_name tval = val.string_value ();

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


graphics_handle
gh_manager::get_handle (const std::string& go_name)
{
  graphics_handle retval;

  if (go_name == "figure")
    {
      // We always want the lowest unused figure number.

      retval = 1;

      while (handle_map.find (retval) != handle_map.end ())
	retval++;
    }
  else
    {
      free_list_iterator p = handle_free_list.begin ();

      if (p != handle_free_list.end ())
	{
	  retval = *p;
	  handle_free_list.erase (p);
	}
      else
	retval = next_handle--;
    }

  return retval;
}

void
gh_manager::do_free (const graphics_handle& h)
{
  if (h)
    {
      if (h.value () != 0)
	{
	  iterator p = handle_map.find (h);

	  if (p != handle_map.end ())
	    {
	      handle_map.erase (p);

	      if (h.value () < 0)
		handle_free_list.insert (h);
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
xset (const graphics_handle& h, const property_name& name,
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
xget (const graphics_handle& h, const property_name& name)
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

      if (h)
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
is_handle (double val)
{
  graphics_handle h = gh_manager::lookup (val);

  return h.ok ();
}

static bool
is_handle (const octave_value& val)
{
  return val.is_real_type () && is_handle (val.double_value ());
}

static bool
is_figure (double val)
{
  graphics_object obj = gh_manager::get_object (val);

  return obj && obj.isa ("figure");
}

// ---------------------------------------------------------------------

static int
compare (const void *a_arg, const void *b_arg)
{
  double a = *(static_cast<const double *> (a_arg));
  double b = *(static_cast<const double *> (b_arg));

  return a > b ? 1 : (a < b) ? -1 : 0;
}

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

	  t1.qsort (compare);
	  t2.qsort (compare);

	  if (t1 != t2)
	    ok = false;
	}      else
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
      Matrix new_kids (1, n-1);
      octave_idx_type j = 0;
      for (octave_idx_type i = 0; i < n; i++)
	{
	  if (i != k)
	    new_kids(j++) = children(i);
	}
      children = new_kids;
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

      if (new_parent)
	{
	  graphics_object parent_obj = gh_manager::get_object (parent);

	  parent_obj.remove_child (__myhandle__);

	  parent = new_parent;

	  ::adopt (parent, __myhandle__);
	}
      else
	error ("set: invalid graphics handle (= %g) for parent", tmp);
    }
  else
    error ("set: expecting parent to be a graphics handle");
}

void
base_properties::mark_modified (void)
{
  __modified__ = true;
  graphics_object parent_obj = gh_manager::get_object (parent);
  parent_obj.mark_modified ();
}

void
base_properties::override_defaults (base_graphics_object& obj)
{
  graphics_object parent_obj = gh_manager::get_object (parent);
  parent_obj.override_defaults (obj);
}

void
base_properties::delete_children (void)
{
  octave_idx_type n = children.numel ();

  for (octave_idx_type i = 0; i < n; i++)
    gh_manager::free (children(i));
}

void
root_figure::properties::set_currentfigure (const graphics_handle& val)
{
  if (error_state)
    return;

  if (is_handle (val))
    {
      currentfigure = val;

      gh_manager::push_figure (currentfigure);
    }
  else
    gripe_set_invalid ("currentfigure");
}

void
root_figure::properties::set (const property_name& name,
			      const octave_value& val)
{
  if (name.compare ("currentfigure"))
    set_currentfigure (val);
  else if (name.compare ("children"))
    children = maybe_set_children (children, val);
  else if (name.compare ("visible"))
    set_visible (val);
  else
    warning ("set: invalid property `%s'", name.c_str ());
}

octave_value root_figure::properties::get (void) const
{
  Octave_map m;

  m.assign ("type", type);
  m.assign ("currentfigure", currentfigure.as_octave_value ());
  m.assign ("children", children);
  m.assign ("visible", visible);

  return m;
}

octave_value 
root_figure::properties::get (const property_name& name) const
{
  octave_value retval;

  if (name.compare ("type"))
    retval = type;
  else if (name.compare ("currentfigure"))
    retval = currentfigure.as_octave_value ();
  else if (name.compare ("children"))
    retval = children;
  else if (name.compare ("visible"))
    retval = visible;
  else
    warning ("get: invalid property `%s'", name.c_str ());

  return retval;
}

property_list
root_figure::factory_properties = root_figure::init_factory_properties ();

std::string root_figure::properties::go_name ("root figure");

// ---------------------------------------------------------------------

figure::properties::properties (const graphics_handle& mh,
				const graphics_handle& p)
  : base_properties (go_name, mh, p),
    __plot_stream__ (Matrix ()),
    nextplot ("replace"),
    closerequestfcn (make_fcn_handle ("closereq")),
    currentaxes (octave_NaN),
    colormap (),
    visible ("on"),
    paperorientation ("portrait")
{ }

void
figure::properties::set_currentaxes (const graphics_handle& val)
{
  if (error_state)
    return;

  if (is_handle (val))
    currentaxes = val;
  else
    gripe_set_invalid ("currentaxes");
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

void
figure::properties::set (const property_name& name, const octave_value& val)
{
  bool modified = true;

  if (name.compare ("children"))
    children = maybe_set_children (children, val);
  else if (name.compare ("__modified__"))
    {
      __modified__ = val.bool_value ();
      modified = false;
    }
  else if (name.compare ("__plot_stream__"))
    set___plot_stream__ (val);
  else if (name.compare ("nextplot"))
    set_nextplot (val);
  else if (name.compare ("closerequestfcn"))
    set_closerequestfcn (val);
  else if (name.compare ("currentaxes"))
    set_currentaxes (val);
  else if (name.compare ("colormap"))
    set_colormap (val);
  else if (name.compare ("visible"))
    set_visible (val);
  else if (name.compare ("paperorientation"))
    set_paperorientation (val);
  else
    {
      modified = false;
      warning ("set: invalid property `%s'", name.c_str ());
    }

  if (modified)
    mark_modified ();
}

octave_value
figure::properties::get (void) const
{
  Octave_map m;

  m.assign ("type", type);
  m.assign ("parent", parent.as_octave_value ());
  m.assign ("children", children);
  m.assign ("__modified__", __modified__);
  m.assign ("__plot_stream__", __plot_stream__);
  m.assign ("nextplot", nextplot);
  m.assign ("closerequestfcn", closerequestfcn);
  m.assign ("currentaxes", currentaxes.as_octave_value ());
  m.assign ("colormap", colormap);
  m.assign ("visible", visible);
  m.assign ("paperorientation", paperorientation);

  return m;
}

octave_value
figure::properties::get (const property_name& name) const
{
  octave_value retval;

  if (name.compare ("type"))
    retval = type;
  else if (name.compare ("parent"))
    retval = parent.as_octave_value ();
  else if (name.compare ("children"))
    retval = children;
  else if (name.compare ("__modified__"))
    retval = __modified__;
  else if (name.compare ("__plot_stream__"))
    retval = __plot_stream__;
  else if (name.compare ("nextplot"))
    retval = nextplot;
  else if (name.compare ("closerequestfcn"))
    retval = closerequestfcn;
  else if (name.compare ("currentaxes"))
    retval = currentaxes.as_octave_value ();
  else if (name.compare ("colormap"))
    retval = colormap;
  else if (name.compare ("visible"))
    retval = visible;
  else if (name.compare ("paperorientation"))
    retval = paperorientation;
  else
    warning ("get: invalid property `%s'", name.c_str ());

  return retval;
}

void
figure::properties::close (void)
{
  if (! __plot_stream__.is_empty ())
    {
      octave_value_list args;
      args(1) = "\nquit;\n";
      args(0) = __plot_stream__;
      feval ("fputs", args);
      args.resize (1);
      feval ("fflush", args);
      feval ("pclose", args);
    }

  gh_manager::pop_figure (__myhandle__);

  graphics_handle cf = gh_manager::current_figure ();

  xset (0, "currentfigure", cf.value ());
}

property_list::pval_map_type
figure::properties::factory_defaults (void)
{
  property_list::pval_map_type m;

  m["nextplot"] = "replace";
  // m["closerequestfcn"] = make_fcn_handle ("closereq");
  m["colormap"] = colormap_property ();
  m["visible"] = "on";
  m["paperorientation"] = "portrait";

  return m;
}

octave_value
figure::get_default (const property_name& name) const
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

std::string figure::properties::go_name ("figure");

// ---------------------------------------------------------------------

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

axes::properties::properties (const graphics_handle& mh,
					const graphics_handle& p)
  : base_properties (go_name, mh, p),
    position (Matrix ()),
    title (octave_NaN),
    box ("on"),
    key ("off"),
    keybox ("off"),
    keypos (1),
    colororder (default_colororder ()),
    dataaspectratio (Matrix (1, 3, 1.0)),
    dataaspectratiomode ("auto"),
    xlim (),
    ylim (),
    zlim (),
    clim (),
    xlimmode ("auto"),
    ylimmode ("auto"),
    zlimmode ("auto"),
    climmode ("auto"),
    xlabel (octave_NaN),
    ylabel (octave_NaN),
    zlabel (octave_NaN),
    xgrid ("off"),
    ygrid ("off"),
    zgrid ("off"),
    xminorgrid ("off"),
    yminorgrid ("off"),
    zminorgrid ("off"),
    xtick (Matrix ()),
    ytick (Matrix ()),
    ztick (Matrix ()),
    xtickmode ("auto"),
    ytickmode ("auto"),
    ztickmode ("auto"),
    xticklabel (""),
    yticklabel (""),
    zticklabel (""),
    xticklabelmode ("auto"),
    yticklabelmode ("auto"),
    zticklabelmode ("auto"),
    xscale ("linear"),
    yscale ("linear"),
    zscale ("linear"),
    xdir ("normal"),
    ydir ("normal"),
    zdir ("normal"),
    xaxislocation ("bottom"),
    yaxislocation ("left"),
    view (),
    visible ("on"),
    nextplot ("replace"),
    outerposition ()
{
  Matrix tlim (1, 2, 0.0);
  tlim(1) = 1;
  xlim = tlim;
  ylim = tlim;
  zlim = tlim;
  Matrix cl (1, 2, 0);
  cl(1) = 1;
  clim = cl;

  Matrix tview (1, 2, 0.0);
  tview(1) = 90;
  view = tview;

  Matrix touterposition (1, 4, 0.0);
  touterposition(2) = 1;
  touterposition(3) = 1;
  outerposition = touterposition;
}

void
axes::properties::set_title (const graphics_handle& val)
{
  if (! error_state)
    {
      gh_manager::free (title);
      title = val;
    }
}

void
axes::properties::set_title (const octave_value& val)
{
  set_title (::reparent (val, "set", "title", __myhandle__, false));
}

void
axes::properties::set_xlabel (const graphics_handle& val)
{
  if (! error_state)
    {
      gh_manager::free (xlabel);
      xlabel = val;
    }
}

void
axes::properties::set_xlabel (const octave_value& val)
{
  set_xlabel (::reparent (val, "set", "xlabel", __myhandle__, false));
}

void
axes::properties::set_ylabel (const graphics_handle& val)
{
  if (! error_state)
    {
      gh_manager::free (ylabel);
      ylabel = val;
    }
}

void
axes::properties::set_ylabel (const octave_value& val)
{
  set_ylabel (::reparent (val, "set", "ylabel", __myhandle__, false));
}

void
axes::properties::set_zlabel (const graphics_handle& val)
{
  if (! error_state)
    {
      gh_manager::free (zlabel);
      zlabel = val;
    }
}

void
axes::properties::set_zlabel (const octave_value& val)
{
  set_zlabel (::reparent (val, "set", "zlabel", __myhandle__, false));
}

void
axes::properties::set (const property_name& name, const octave_value& val)
{
  bool modified = true;

  if (name.compare ("parent"))
    set_parent (val);
  else if (name.compare ("children"))
    children = maybe_set_children (children, val);
  else if (name.compare ("__modified__"))
    {
      __modified__ = val.bool_value ();
      modified = false;
    }
  else if (name.compare ("position"))
    set_position (val);
  else if (name.compare ("title"))
    set_title (val);
  else if (name.compare ("box"))
    set_box (val);
  else if (name.compare ("key"))
    set_key (val);
  else if (name.compare ("keybox"))
    set_keybox (val);
  else if (name.compare ("keypos"))
    set_keypos (val);
  else if (name.compare ("colororder"))
    set_colororder (val);
  else if (name.compare ("dataaspectratio"))
    set_dataaspectratio (val);
  else if (name.compare ("dataaspectratiomode"))
    set_dataaspectratiomode (val);
  else if (name.compare ("xlim"))
    set_xlim (val);
  else if (name.compare ("ylim"))
    set_ylim (val);
  else if (name.compare ("zlim"))
    set_zlim (val);
  else if (name.compare ("clim"))
    set_clim (val);
  else if (name.compare ("xlimmode"))
    set_xlimmode (val);
  else if (name.compare ("ylimmode"))
    set_ylimmode (val);
  else if (name.compare ("zlimmode"))
    set_zlimmode (val);
  else if (name.compare ("climmode"))
    set_climmode (val);
  else if (name.compare ("xlabel"))
    set_xlabel (val);
  else if (name.compare ("ylabel"))
    set_ylabel (val);
  else if (name.compare ("zlabel"))
    set_zlabel (val);
  else if (name.compare ("xgrid"))
    set_xgrid (val);
  else if (name.compare ("ygrid"))
    set_ygrid (val);
  else if (name.compare ("zgrid"))
    set_zgrid (val);
  else if (name.compare ("xminorgrid"))
    set_xminorgrid (val);
  else if (name.compare ("yminorgrid"))
    set_yminorgrid (val);
  else if (name.compare ("zminorgrid"))
    set_zminorgrid (val);
  else if (name.compare ("xtick"))
    set_xtick (val);
  else if (name.compare ("ytick"))
    set_xtick (val);
  else if (name.compare ("ztick"))
    set_ztick (val);
  else if (name.compare ("xtickmode"))
    set_xtickmode (val);
  else if (name.compare ("ytickmode"))
    set_ytickmode (val);
  else if (name.compare ("ztickmode"))
    set_ztickmode (val);
  else if (name.compare ("xticklabel"))
    set_xticklabel (val);
  else if (name.compare ("yticklabel"))
    set_yticklabel (val);
  else if (name.compare ("zticklabel"))
    set_zticklabel (val);
  else if (name.compare ("xticklabelmode"))
    set_xticklabelmode (val);
  else if (name.compare ("yticklabelmode"))
    set_yticklabelmode (val);
  else if (name.compare ("zticklabelmode"))
    set_zticklabelmode (val);
  else if (name.compare ("xscale"))
    set_xscale (val);
  else if (name.compare ("yscale"))
    set_yscale (val);
  else if (name.compare ("zscale"))
    set_zscale (val);
  else if (name.compare ("xdir"))
    set_xdir (val);
  else if (name.compare ("ydir"))
    set_ydir (val);
  else if (name.compare ("zdir"))
    set_zdir (val);
  else if (name.compare ("xaxislocation"))
    set_xaxislocation (val);
  else if (name.compare ("yaxislocation"))
    set_yaxislocation (val);
  else if (name.compare ("view"))
    set_view (val);
  else if (name.compare ("visible"))
    set_visible (val);
  else if (name.compare ("nextplot"))
    set_nextplot (val);
  else if (name.compare ("outerposition"))
    set_outerposition (val);
  else
    {
      modified = false;
      warning ("set: invalid property `%s'", name.c_str ());
    }

  if (modified)
    mark_modified ();
}

void
axes::properties::set_defaults (base_graphics_object& obj,
				const std::string& mode)
{
  position = Matrix ();
  title = octave_NaN;
  box = "on";
  key = "off";
  keybox = "off";
  keypos = 1;
  colororder = default_colororder ();
  dataaspectratio = Matrix (1, 3, 1.0);
  dataaspectratiomode = "auto";

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
  xlabel = octave_NaN;
  ylabel = octave_NaN;
  zlabel = octave_NaN;
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
  xscale = "linear";
  yscale = "linear";
  zscale = "linear";
  xdir = "normal";
  ydir = "normal";
  zdir = "normal";
  xaxislocation = "left";
  yaxislocation = "bottom";

  Matrix tview (1, 2, 0.0);
  tview(1) = 90;
  view = tview;

  visible = "on";
  nextplot = "replace";

  // FIXME -- this is not quite right; we should preserve
  // "position" and "units".

  if (mode != "replace")
    {
      Matrix touterposition (1, 4, 0.0);
      touterposition(2) = 1;
      touterposition(3) = 1;
      outerposition = touterposition;
    }

  delete_children ();

  children = Matrix ();

  override_defaults (obj);
}

graphics_handle
axes::properties::get_title (void) const
{
  if (! title)
    title = gh_manager::make_graphics_handle ("text", __myhandle__);

  return title;
}

graphics_handle
axes::properties::get_xlabel (void) const
{
  if (! xlabel)
    xlabel = gh_manager::make_graphics_handle ("text", __myhandle__);

  return xlabel;
}

graphics_handle
axes::properties::get_ylabel (void) const
{
  if (! ylabel)
    ylabel = gh_manager::make_graphics_handle ("text", __myhandle__);

  return ylabel;
}

graphics_handle
axes::properties::get_zlabel (void) const
{
  if (! zlabel)
    zlabel = gh_manager::make_graphics_handle ("text", __myhandle__);

  return zlabel;
}

octave_value
axes::properties::get (void) const
{
  Octave_map m;

  m.assign ("type", type);
  m.assign ("parent", parent.as_octave_value ());
  m.assign ("children", children);
  m.assign ("__modified__", __modified__);
  m.assign ("position", position);
  m.assign ("title", get_title().as_octave_value ());
  m.assign ("box", box);
  m.assign ("key", key);
  m.assign ("keybox", keybox);
  m.assign ("keypos", keypos);
  m.assign ("colororder", colororder);
  m.assign ("dataaspectratio", dataaspectratio);
  m.assign ("dataaspectratiomode", dataaspectratiomode);
  m.assign ("xlim", xlim);
  m.assign ("ylim", ylim);
  m.assign ("zlim", zlim);
  m.assign ("clim", clim);
  m.assign ("xlimmode", xlimmode);
  m.assign ("ylimmode", ylimmode);
  m.assign ("zlimmode", zlimmode);
  m.assign ("climmode", climmode);
  m.assign ("xlabel", get_xlabel().as_octave_value ());
  m.assign ("ylabel", get_ylabel().as_octave_value ());
  m.assign ("zlabel", get_zlabel().as_octave_value ());
  m.assign ("xgrid", xgrid);
  m.assign ("ygrid", ygrid);
  m.assign ("zgrid", zgrid);
  m.assign ("xminorgrid", xminorgrid);
  m.assign ("yminorgrid", yminorgrid);
  m.assign ("zminorgrid", zminorgrid);
  m.assign ("xtick", xtick);
  m.assign ("ytick", ytick);
  m.assign ("ztick", ztick);
  m.assign ("xtickmode", xtickmode);
  m.assign ("ytickmode", ytickmode);
  m.assign ("ztickmode", ztickmode);
  m.assign ("xticklabel", xticklabel);
  m.assign ("yticklabel", yticklabel);
  m.assign ("zticklabel", zticklabel);
  m.assign ("xticklabelmode", xticklabelmode);
  m.assign ("yticklabelmode", yticklabelmode);
  m.assign ("zticklabelmode", zticklabelmode);
  m.assign ("xscale", xscale);
  m.assign ("yscale", yscale);
  m.assign ("zscale", zscale);
  m.assign ("xdir", xdir);
  m.assign ("ydir", ydir);
  m.assign ("zdir", zdir);
  m.assign ("xaxislocation", xaxislocation);
  m.assign ("yaxislocation", yaxislocation);
  m.assign ("view", view);
  m.assign ("visible", visible);
  m.assign ("nextplot", nextplot);
  m.assign ("outerposition", outerposition);

  return m;
}

octave_value
axes::properties::get (const property_name& name) const
{
  octave_value retval;

  if (name.compare ("type"))
    retval = type;
  else if (name.compare ("parent"))
    retval = parent.value ();
  else if (name.compare ("children"))
    retval = children;
  else if (name.compare ("__modified__"))
    retval = __modified__;
  else if (name.compare ("position"))
    retval = position;
  else if (name.compare ("title"))
    retval = get_title().as_octave_value ();
  else if (name.compare ("box"))
    retval = box;
  else if (name.compare ("key"))
    retval = key;
  else if (name.compare ("keybox"))
    retval = keybox;
  else if (name.compare ("keypos"))
    retval = keypos;
  else if (name.compare ("colororder"))
    retval = colororder;
  else if (name.compare ("dataaspectratio"))
    retval = dataaspectratio;
  else if (name.compare ("dataaspectratiomode"))
    retval = dataaspectratiomode;
  else if (name.compare ("xlim"))
    retval = xlim;
  else if (name.compare ("ylim"))
    retval = ylim;
  else if (name.compare ("zlim"))
    retval = zlim;
  else if (name.compare ("clim"))
    retval = clim;
  else if (name.compare ("xlimmode"))
    retval = xlimmode;
  else if (name.compare ("ylimmode"))
    retval = ylimmode;
  else if (name.compare ("zlimmode"))
    retval = zlimmode;
  else if (name.compare ("climmode"))
    retval = climmode;
  else if (name.compare ("xlabel"))
    retval = get_xlabel().as_octave_value ();
  else if (name.compare ("ylabel"))
    retval = get_ylabel().as_octave_value ();
  else if (name.compare ("zlabel"))
    retval = get_zlabel().as_octave_value ();
  else if (name.compare ("xgrid"))
    retval = xgrid;
  else if (name.compare ("ygrid"))
    retval = ygrid;
  else if (name.compare ("zgrid"))
    retval = zgrid;
  else if (name.compare ("xminorgrid"))
    retval = xminorgrid;
  else if (name.compare ("yminorgrid"))
    retval = yminorgrid;
  else if (name.compare ("zminorgrid"))
    retval = zminorgrid;
  else if (name.compare ("xtick"))
    retval = xtick;
  else if (name.compare ("ytick"))
    retval = ytick;
  else if (name.compare ("ztick"))
    retval = ztick;
  else if (name.compare ("xtickmode"))
    retval = xtickmode;
  else if (name.compare ("ytickmode"))
    retval = ytickmode;
  else if (name.compare ("ztickmode"))
    retval = ztickmode;
  else if (name.compare ("xticklabel"))
    retval = xticklabel;
  else if (name.compare ("yticklabel"))
    retval = yticklabel;
  else if (name.compare ("zticklabel"))
    retval = zticklabel;
  else if (name.compare ("xticklabelmode"))
    retval = xticklabelmode;
  else if (name.compare ("yticklabelmode"))
    retval = yticklabelmode;
  else if (name.compare ("zticklabelmode"))
    retval = zticklabelmode;
  else if (name.compare ("xscale"))
    retval = xscale;
  else if (name.compare ("yscale"))
    retval = yscale;
  else if (name.compare ("zscale"))
    retval = zscale;
  else if (name.compare ("xdir"))
    retval = xdir;
  else if (name.compare ("ydir"))
    retval = ydir;
  else if (name.compare ("zdir"))
    retval = zdir;
  else if (name.compare ("xaxislocation"))
    retval = xaxislocation;
  else if (name.compare ("yaxislocation"))
    retval = yaxislocation;
  else if (name.compare ("view"))
    retval = view;
  else if (name.compare ("visible"))
    retval = visible;
  else if (name.compare ("nextplot"))
    retval = nextplot;
  else if (name.compare ("outerposition"))
    retval = outerposition;
  else
    warning ("get: invalid property `%s'", name.c_str ());

  return retval;
}

void
axes::properties::remove_child (const graphics_handle& h)
{
  if (title && h == title)
    title = gh_manager::make_graphics_handle ("text", __myhandle__);
  else if (xlabel && h == xlabel)
    xlabel = gh_manager::make_graphics_handle ("text", __myhandle__);
  else if (ylabel && h == ylabel)
    ylabel = gh_manager::make_graphics_handle ("text", __myhandle__);
  else if (zlabel && h == zlabel)
    zlabel = gh_manager::make_graphics_handle ("text", __myhandle__);
  else
    base_properties::remove_child (h);
}

void
axes::properties::delete_children (void)
{
  base_properties::delete_children ();

  gh_manager::free (title);
  gh_manager::free (xlabel);
  gh_manager::free (ylabel);
  gh_manager::free (zlabel);
}

property_list::pval_map_type
axes::properties::factory_defaults (void)
{
  property_list::pval_map_type m;

  m["position"] = Matrix ();
  m["title"] = octave_NaN;
  m["box"] = "on";
  m["key"] = "off";
  m["keybox"] = "off";
  m["keypos"] = 1;
  m["colororder"] = default_colororder ();
  m["dataaspectratio"] = Matrix (1, 3, 1.0);
  m["dataaspectratiomode"] = "auto";

  Matrix tlim (1, 2, 0.0);
  tlim(1) = 1;

  m["xlim"] = tlim;
  m["ylim"] = tlim;
  m["zlim"] = tlim;
  
  Matrix cl(1, 2, 0);
  cl(1) = 1;
  
  m["clim"] = cl;

  m["xlimmode"] = "auto";
  m["ylimmode"] = "auto";
  m["zlimmode"] = "auto";
  m["climmode"] = "auto";
  m["xlabel"] = octave_NaN;
  m["ylabel"] = octave_NaN;
  m["zlabel"] = octave_NaN;
  m["xgrid"] = "off";
  m["ygrid"] = "off";
  m["zgrid"] = "off";
  m["xminorgrid"] = "off";
  m["yminorgrid"] = "off";
  m["zminorgrid"] = "off";
  m["xtick"] = Matrix ();
  m["ytick"] = Matrix ();
  m["ztick"] = Matrix ();
  m["xtickmode"] = "auto";
  m["ytickmode"] = "auto";
  m["ztickmode"] = "auto";
  m["xticklabel"] = "";
  m["yticklabel"] = "";
  m["zticklabel"] = "";
  m["xticklabelmode"] = "auto";
  m["yticklabelmode"] = "auto";
  m["zticklabelmode"] = "auto";
  m["xscale"] = "linear";
  m["yscale"] = "linear";
  m["zscale"] = "linear";
  m["xdir"] = "normal";
  m["ydir"] = "normal";
  m["zdir"] = "normal";
  m["xaxislocation"] = "bottom";
  m["yaxislocation"] = "left";

  Matrix tview (1, 2, 0.0);
  tview(1) = 90;

  m["view"] = tview;

  m["visible"] = "on";
  m["nextplot"] = "replace";

  Matrix touterposition (1, 4, 0.0);
  touterposition(2) = 1;
  touterposition(3) = 1;

  m["outerposition"] = touterposition;

  return m;
}

octave_value
axes::get_default (const property_name& name) const
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

std::string axes::properties::go_name ("axes");

// ---------------------------------------------------------------------

static Matrix
default_data (void)
{
  Matrix retval (1, 2);

  retval(0) = 0;
  retval(1) = 1;

  return retval;
}

line::properties::properties (const graphics_handle& mh,
			      const graphics_handle& p)
  : base_properties (go_name, mh, p),
    xdata (default_data ()),
    ydata (default_data ()),
    zdata (Matrix ()),
    ldata (Matrix ()),
    udata (Matrix ()),
    xldata (Matrix ()),
    xudata (Matrix ()),
    color (),
    linestyle ("-"),
    linewidth (0.5),
    marker ("none"),
    markeredgecolor ("auto"),
    markerfacecolor ("none"),
    markersize (1),
    keylabel ("")
{ }

void
line::properties::set (const property_name& name, const octave_value& val)
{
  bool modified = true;

  if (name.compare ("parent"))
    set_parent (val);
  else if (name.compare ("children"))
    children = maybe_set_children (children, val);
  else if (name.compare ("__modified__"))
    {
      __modified__ = val.bool_value ();
      modified = false;
    }
  else if (name.compare ("xdata"))
    set_xdata (val);
  else if (name.compare ("ydata"))
    set_ydata (val);
  else if (name.compare ("zdata"))
    set_zdata (val);
  else if (name.compare ("ldata"))
    set_ldata (val);
  else if (name.compare ("udata"))
    set_udata (val);
  else if (name.compare ("xldata"))
    set_xldata (val);
  else if (name.compare ("xudata"))
    set_xudata (val);
  else if (name.compare ("color"))
    set_color (val);
  else if (name.compare ("linestyle"))
    set_linestyle (val);
  else if (name.compare ("linewidth"))
    set_linewidth (val);
  else if (name.compare ("marker"))
    set_marker (val);
  else if (name.compare ("markeredgecolor"))
    set_markeredgecolor (val);
  else if (name.compare ("markerfacecolor"))
    set_markerfacecolor (val);
  else if (name.compare ("markersize"))
    set_markersize (val);
  else if (name.compare ("keylabel"))
    set_keylabel (val);
  else
    {
      modified = false;
      warning ("set: invalid property `%s'", name.c_str ());
    }

  if (modified)
    mark_modified ();
}

octave_value
line::properties::get (void) const
{
  Octave_map m;

  m.assign ("type", type);
  m.assign ("parent", parent.as_octave_value ());
  m.assign ("children", children);
  m.assign ("__modified__", __modified__);
  m.assign ("xdata", xdata);
  m.assign ("ydata", ydata);
  m.assign ("zdata", zdata);
  m.assign ("ldata", ldata);
  m.assign ("udata", udata);
  m.assign ("xldata", xldata);
  m.assign ("xudata", xudata);
  m.assign ("color", color);
  m.assign ("linestyle", linestyle);
  m.assign ("linewidth", linewidth);
  m.assign ("marker", marker);
  m.assign ("markeredgecolor", markeredgecolor);
  m.assign ("markerface", markerfacecolor);
  m.assign ("markersize", markersize);
  m.assign ("keylabel", keylabel);

  return m;
}

octave_value
line::properties::get (const property_name& name) const
{
  octave_value retval;

  if (name.compare ("type"))
    retval = type;
  else if (name.compare ("parent"))
    retval = parent.as_octave_value ();
  else if (name.compare ("children"))
    retval = children;
  else if (name.compare ("__modified__"))
    retval = __modified__;
  else if (name.compare ("xdata"))
    retval = xdata;
  else if (name.compare ("ydata"))
    retval = ydata;
  else if (name.compare ("zdata"))
    retval = zdata;
  else if (name.compare ("ldata"))
    retval = ldata;
  else if (name.compare ("udata"))
    retval = udata;
  else if (name.compare ("xldata"))
    retval = xldata;
  else if (name.compare ("xudata"))
    retval = xudata;
  else if (name.compare ("color"))
    retval = color;
  else if (name.compare ("linestyle"))
    retval = linestyle;
  else if (name.compare ("linewidth"))
    retval = linewidth;
  else if (name.compare ("marker"))
    retval = marker;
  else if (name.compare ("markeredgecolor"))
    retval = markeredgecolor;
  else if (name.compare ("markerfacecolor"))
    retval = markerfacecolor;
  else if (name.compare ("markersize"))
    retval = markersize;
  else if (name.compare ("keylabel"))
    retval = keylabel;
  else
    warning ("get: invalid property `%s'", name.c_str ());

  return retval;
}

property_list::pval_map_type
line::properties::factory_defaults (void)
{
  property_list::pval_map_type m;

  m["xdata"] = default_data ();
  m["ydata"] = default_data ();
  m["zdata"] = Matrix ();
  m["ldata"] = Matrix ();
  m["udata"] = Matrix ();
  m["xldata"] = Matrix ();
  m["xudata"] = Matrix ();
  m["color"] = color_property ();
  m["linestyle"] = "-";
  m["linewidth"] = 0.5;
  m["marker"] = "none";
  m["markeredgecolor"] = "auto";
  m["markerfacecolor"] = "none";
  m["markersize"] = 1;
  m["keylabel"] = "";

  return m;
}

std::string line::properties::go_name ("line");

// ---------------------------------------------------------------------

text::properties::properties (const graphics_handle& mh,
			      const graphics_handle& p)
  : base_properties (go_name, mh, p),
    string (""),
    units ("data"),
    position (Matrix (1, 3, 0.0)),
    rotation (0),
    horizontalalignment ("left"),
    color (Matrix (1, 3, 0.0))
{ }

void
text::properties::set (const property_name& name, const octave_value& val)
{
  bool modified = true;

  if (name.compare ("parent"))
    set_parent (val);
  else if (name.compare ("children"))
    children = maybe_set_children (children, val);
  else if (name.compare ("__modified__"))
    {
      __modified__ = val.bool_value ();
      modified = false;
    }
  else if (name.compare ("string"))
    set_string (val);
  else if (name.compare ("units"))
    set_units (val);
  else if (name.compare ("position"))
    set_position (val);
  else if (name.compare ("rotation"))
    set_rotation (val);
  else if (name.compare ("horizontalalignment"))
    set_horizontalalignment (val);
  else if (name.compare ("color"))
    set_color (val);
  else
    {
      modified = false;
      warning ("set: invalid property `%s'", name.c_str ());
    }

  if (modified)
    mark_modified ();
}

octave_value
text::properties::get (void) const
{
  Octave_map m;

  m.assign ("type", type);
  m.assign ("parent", parent.as_octave_value ());
  m.assign ("children", children);
  m.assign ("__modified__", __modified__);
  m.assign ("string", string);
  m.assign ("units", units);
  m.assign ("position", position);
  m.assign ("rotation", rotation);
  m.assign ("horizontalalignment", horizontalalignment);
  m.assign ("color", color);

  return m;
}

octave_value
text::properties::get (const property_name& name) const
{
  octave_value retval;

  if (name.compare ("type"))
    retval = type;
  else if (name.compare ("parent"))
    retval = parent.as_octave_value ();
  else if (name.compare ("children"))
    retval = children;
  else if (name.compare ("__modified__"))
    retval = __modified__;
  else if (name.compare ("string"))
    retval = string;
  else if (name.compare ("units"))
    retval = units;
  else if (name.compare ("position"))
    retval = position;
  else if (name.compare ("rotation"))
    retval = rotation;
  else if (name.compare ("horizontalalignment"))
    retval = horizontalalignment;
  else if (name.compare ("color"))
    retval = color;
  else
    warning ("get: invalid property `%s'", name.c_str ());

  return retval;
}

property_list::pval_map_type
text::properties::factory_defaults (void)
{
  property_list::pval_map_type m;

  m["string"] = "";
  m["units"] = "data";
  m["position"] = Matrix (1, 3, 0.0);
  m["rotation"] = 0;
  m["horizontalalignment"] = "left";
  m["color"] = Matrix (1, 3, 1.0);

  return m;
}

std::string text::properties::go_name ("text");

// ---------------------------------------------------------------------

image::properties::properties (const graphics_handle& mh,
			       const graphics_handle& p)
  : base_properties (go_name, mh, p),
    cdata (Matrix ()),
    xdata (Matrix ()),
    ydata (Matrix ())
{ }

void
image::properties::set (const property_name& name,
			const octave_value& val)
{
  bool modified = true;

  if (name.compare ("parent"))
    set_parent (val);
  else if (name.compare ("children"))
    children = maybe_set_children (children, val);
  else if (name.compare ("__modified__"))
    {
      __modified__ = val.bool_value ();
      modified = false;
    }
  else if (name.compare ("cdata"))
    set_cdata (val);
  else if (name.compare ("xdata"))
    set_xdata (val);
  else if (name.compare ("ydata"))
    set_ydata (val);
  else
    {
      modified = false;
      warning ("set: invalid property `%s'", name.c_str ());
    }

  if (modified)
    mark_modified ();
}

octave_value
image::properties::get (void) const
{
  Octave_map m;

  m.assign ("type", type);
  m.assign ("parent", parent.as_octave_value ());
  m.assign ("children", children);
  m.assign ("__modified__", __modified__);
  m.assign ("cdata", cdata);
  m.assign ("xdata", xdata);
  m.assign ("ydata", ydata);

  return m;
}

octave_value
image::properties::get (const property_name& name) const
{
  octave_value retval;

  if (name.compare ("type"))
    retval = type;
  else if (name.compare ("parent"))
    retval = parent.as_octave_value ();
  else if (name.compare ("children"))
    retval = children;
  else if (name.compare ("__modified__"))
    retval = __modified__;
  else if (name.compare ("cdata"))
    retval = cdata;
  else if (name.compare ("xdata"))
    retval = xdata;
  else if (name.compare ("ydata"))
    retval = ydata;
  else
    warning ("get: invalid property `%s'", name.c_str ());

  return retval;
}

property_list::pval_map_type
image::properties::factory_defaults (void)
{
  property_list::pval_map_type m;

  m["cdata"] = Matrix ();
  m["xdata"] = Matrix ();
  m["ydata"] = Matrix ();

  return m;
}

std::string image::properties::go_name ("image");

// ---------------------------------------------------------------------

patch::properties::properties (const graphics_handle& mh,
			       const graphics_handle& p)
  : base_properties (go_name, mh, p),
    cdata (Matrix ()),
    xdata (Matrix ()),
    ydata (Matrix ()),
    zdata (Matrix ()),
    faces (Matrix ()),
    vertices (Matrix ()),
    facecolor (radio_values ("{flat}|none|interp")),
    facealpha (1.0),
    edgecolor (color_values(0, 0, 0), radio_values ("flat|none|interp")),
    linestyle ("-"),
    linewidth (0.5),
    marker ("none"),
    markeredgecolor ("auto"),
    markerfacecolor ("none"),
    markersize (1)
{ }

void
patch::properties::set (const property_name& name,
			const octave_value& val)
{
  bool modified = true;

  if (name.compare ("parent"))
    set_parent (val);
  else if (name.compare ("children"))
    children = maybe_set_children (children, val);
  else if (name.compare ("__modified__"))
    {
      __modified__ = val.bool_value ();
      modified = false;
    }
  else if (name.compare ("cdata"))
    set_cdata (val);
  else if (name.compare ("xdata"))
    set_xdata (val);
  else if (name.compare ("ydata"))
    set_ydata (val);
  else if (name.compare ("zdata"))
    set_zdata (val);
  else if (name.compare ("faces"))
    set_faces (val);
  else if (name.compare ("vertices"))
    set_vertices (val);
  else if (name.compare ("facecolor"))
    set_facecolor (val);
  else if (name.compare ("facealpha"))
    set_facealpha (val);
  else if (name.compare ("edgecolor"))
    set_edgecolor (val);
  else if (name.compare ("linestyle"))
    set_linestyle (val);
  else if (name.compare ("linewidth"))
    set_linewidth (val);
  else if (name.compare ("marker"))
    set_marker (val);
  else if (name.compare ("markeredgecolor"))
    set_markeredgecolor (val);
  else if (name.compare ("markerfacecolor"))
    set_markerfacecolor (val);
  else if (name.compare ("markersize"))
    set_markersize (val);
  else
    {
      modified = false;
      warning ("set: invalid property `%s'", name.c_str ());
    }

  if (modified)
    mark_modified ();
}

octave_value
patch::properties::get (void) const
{
  Octave_map m;

  m.assign ("type", type);
  m.assign ("parent", parent.as_octave_value ());
  m.assign ("children", children);
  m.assign ("__modified__", __modified__);
  m.assign ("cdata", cdata);
  m.assign ("xdata", xdata);
  m.assign ("ydata", ydata);
  m.assign ("zdata", zdata);
  m.assign ("faces", faces);
  m.assign ("vertices", vertices);
  m.assign ("facecolor", facecolor);
  m.assign ("facealpha", facealpha);
  m.assign ("edgecolor", edgecolor);
  m.assign ("linestyle", linestyle);
  m.assign ("linewidth", linewidth);
  m.assign ("marker", marker);
  m.assign ("markeredgecolor", markeredgecolor);
  m.assign ("markerface", markerfacecolor);
  m.assign ("markersize", markersize);

  return m;
}

octave_value
patch::properties::get (const property_name& name) const
{
  octave_value retval;

  if (name.compare ("type"))
    retval = type;
  else if (name.compare ("parent"))
    retval = parent.as_octave_value ();
  else if (name.compare ("children"))
    retval = children;
  else if (name.compare ("__modified__"))
    retval = __modified__;
  else if (name.compare ("cdata"))
    retval = cdata;
  else if (name.compare ("xdata"))
    retval = xdata;
  else if (name.compare ("ydata"))
    retval = ydata;
  else if (name.compare ("zdata"))
    retval = zdata;
  else if (name.compare ("faces"))
    retval = faces;
  else if (name.compare ("vertices"))
    retval = vertices;
  else if (name.compare ("facecolor"))
    retval = facecolor;
  else if (name.compare ("facealpha"))
    retval = facecolor;
  else if (name.compare ("egdecolor"))
    retval = edgecolor;
  else if (name.compare ("linestyle"))
    retval = linestyle;
  else if (name.compare ("linewidth"))
    retval = linewidth;
  else if (name.compare ("marker"))
    retval = marker;
  else if (name.compare ("markeredgecolor"))
    retval = markeredgecolor;
  else if (name.compare ("markerfacecolor"))
    retval = markerfacecolor;
  else if (name.compare ("markersize"))
    retval = markersize;
  else
    warning ("get: invalid property `%s'", name.c_str ());

  return retval;
}

property_list::pval_map_type
patch::properties::factory_defaults (void)
{
  property_list::pval_map_type m;

  m["cdata"] = Matrix ();
  m["xdata"] = Matrix ();
  m["ydata"] = Matrix ();
  m["zdata"] = Matrix ();
  m["faces"] = Matrix ();
  m["vertices"] = Matrix ();
  m["facecolor"] = color_property();
  m["facealpha"] = 1.0;
  m["edgecolor"] = color_property("black");
  m["linestyle"] = "-";
  m["linewidth"] = 0.5;
  m["marker"] = "none";
  m["markeredgecolor"] = "auto";
  m["markerfacecolor"] = "none";
  m["markersize"] = 1;


  return m;
}

std::string patch::properties::go_name ("patch");

// ---------------------------------------------------------------------

surface::properties::properties (const graphics_handle& mh,
				 const graphics_handle& p)
  : base_properties (go_name, mh, p),
    xdata (Matrix ()),
    ydata (Matrix ()),
    zdata (Matrix ()),
    keylabel ("")
{ }

void
surface::properties::set (const property_name& name,
			  const octave_value& val)
{
  bool modified = true;

  if (name.compare ("parent"))
    set_parent (val);
  else if (name.compare ("children"))
    children = maybe_set_children (children, val);
  else if (name.compare ("__modified__"))
    {
      __modified__ = val.bool_value ();
      modified = false;
    }
  else if (name.compare ("xdata"))
    set_xdata (val);
  else if (name.compare ("ydata"))
    set_ydata (val);
  else if (name.compare ("zdata"))
    set_zdata (val);
  else if (name.compare ("keylabel"))
    set_keylabel (val);
  else
    {
      modified = false;
      warning ("set: invalid property `%s'", name.c_str ());
    }

  if (modified)
    mark_modified ();
}

octave_value
surface::properties::get (void) const
{
  Octave_map m;

  m.assign ("type", type);
  m.assign ("parent", parent.as_octave_value ());
  m.assign ("children", children);
  m.assign ("__modified__", __modified__);
  m.assign ("xdata", xdata);
  m.assign ("ydata", ydata);
  m.assign ("zdata", zdata);
  m.assign ("keylabel", keylabel);

  return m;
}

octave_value
surface::properties::get (const property_name& name) const
{
  octave_value retval;

  if (name.compare ("type"))
    retval = type;
  else if (name.compare ("parent"))
    retval = parent.as_octave_value ();
  else if (name.compare ("children"))
    retval = children;
  else if (name.compare ("__modified__"))
    retval = __modified__;
  else if (name.compare ("xdata"))
    retval = xdata;
  else if (name.compare ("ydata"))
    retval = ydata;
  else if (name.compare ("zdata"))
    retval = zdata;
  else if (name.compare ("keylabel"))
    retval = keylabel;
  else
    warning ("get: invalid property `%s'", name.c_str ());

  return retval;
}

property_list::pval_map_type
surface::properties::factory_defaults (void)
{
  property_list::pval_map_type m;

  m["xdata"] = Matrix ();
  m["ydata"] = Matrix ();
  m["zdata"] = Matrix ();
  m["keylabel"] = "";

  return m;
}

std::string surface::properties::go_name ("surface");

// ---------------------------------------------------------------------

octave_value
base_graphics_object::get_default (const property_name& name) const
{
  graphics_handle parent = get_parent ();
  graphics_object parent_obj = gh_manager::get_object (parent);

  return parent_obj.get_default (type () + name);
}

octave_value
base_graphics_object::get_factory_default (const property_name& name) const
{
  graphics_object parent_obj = gh_manager::get_object (0);

  return parent_obj.get_factory_default (type () + name);
}

gh_manager::gh_manager (void)
  : handle_map (), handle_free_list (), next_handle (-1)
{
  handle_map[0] = graphics_object (new root_figure ());
}

graphics_handle
gh_manager::do_make_graphics_handle (const std::string& go_name,
				     const graphics_handle& p)
{
  graphics_handle h = get_handle (go_name);

  base_graphics_object *go = 0;

  if (go_name == "figure")
    go = new figure (h, p);
  else if (go_name == "axes")
    go = new axes (h, p);
  else if (go_name == "line")
    go = new line (h, p);
  else if (go_name == "text")
    go = new text (h, p);
  else if (go_name == "image")
    go = new image (h, p);
  else if (go_name == "patch")
    go = new patch (h, p);
  else if (go_name == "surface")
    go = new surface (h, p);
  if (go)
    handle_map[h] = graphics_object (go);
  else
    error ("gh_manager::do_make_graphics_handle: invalid object type `%s'",
	   go_name.c_str ());

  return h;
}

graphics_handle
gh_manager::do_make_figure_handle (double val)
{
  graphics_handle h = val;

  handle_map[h] = graphics_object (new figure (h, 0));

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

  return plist_map;
}

// ---------------------------------------------------------------------

DEFUN (ishandle, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ishandle (@var{h})\n\
Return true if @var{h} is a graphics handle and false otherwise.\n\
@end deftypefn")
{
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
	    feval ("__request_drawnow__");
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
  octave_value retval;
  octave_value_list vlist;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      ColumnVector hcv (args(0).vector_value ());

      if (! error_state)
        {
	  octave_idx_type len = hcv.length ();

	  vlist.resize (len);

          for (octave_idx_type n = 0; n < len; n++)
            {
              graphics_object obj = gh_manager::get_object (hcv(n));

              if (obj)
                {
                  if (nargin == 1)
                    vlist(n) = obj.get ();
                  else
                    {
                      property_name property = args(1).string_value ();

                      if (! error_state)
                        vlist(n) = obj.get (property);
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
      octave_idx_type len = vlist.length ();

      if (len > 1)
	retval = Cell (vlist);
      else if (len == 1)
	retval = vlist(0);
    }

  return retval;
}

static octave_value
make_graphics_object (const std::string& go_name,
		      const octave_value_list& args)
{
  octave_value retval;

  double val = args(0).double_value ();

  if (! error_state)
    {
      graphics_handle parent = gh_manager::lookup (val);

      if (parent)
	{
	  graphics_handle h
	    = gh_manager::make_graphics_handle (go_name, parent);

	  if (! error_state)
	    {
	      adopt (parent, h);

	      xset (h, args.splice (0, 1));

	      retval = h.value ();
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
		h = gh_manager::make_graphics_handle ("figure", 0);
	      else if (val > 0 && D_NINT (val) == val)
		h = gh_manager::make_figure_handle (val);
	      else
		error ("__go_figure__: invalid figure number");

	      if (! error_state && h)
		{
		  adopt (0, h);

		  xset (h, args.splice (0, 1));

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

DEFUN (__go_delete__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_delete__ (@var{h})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      graphics_handle h = octave_NaN;

      double val = args(0).double_value ();

      if (! error_state)
	{
	  h = gh_manager::lookup (val);

	  if (h)
	    {
	      graphics_object obj = gh_manager::get_object (h);

	      graphics_handle parent_h = obj.get_parent ();

	      graphics_object parent_obj = gh_manager::get_object (parent_h);

	      parent_obj.remove_child (h);

	      gh_manager::free (h);
	    }
	  else
	    error ("delete: invalid graphics object (= %g)", val);
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

	  if (h)
	    {
	      graphics_object obj = gh_manager::get_object (h);

	      obj.set_defaults (mode);
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
  return octave_value (gh_manager::handle_list ());
}

DEFUN (__go_figure_handles__, , ,
   "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_figure_handles__ ()\n\
Undocumented internal function.\n\
@end deftypefn")
{
  return octave_value (gh_manager::figure_handle_list ());
}

octave_value
get_property_from_handle (double handle, const std::string &property,
			  const std::string &func)
{
  graphics_object obj = gh_manager::get_object (handle);
  octave_value retval;

  if (obj)
    {
      property_name p = std::string (property);
      retval = obj.get (p);
    }
  else
    error ("%s: invalid handle (= %g)", func.c_str(), handle);

  return retval;
}

bool
set_property_in_handle (double handle, const std::string &property,
			const octave_value &arg, const std::string &func)
{
  graphics_object obj = gh_manager::get_object (handle);
  int ret = false;

  if (obj)
    {
      property_name p = std::string (property);
      obj.set (p, arg);
      if (!error_state)
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
