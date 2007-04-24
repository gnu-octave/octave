/*

Copyright (C) 2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

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

#include <defun.h>
#include <ov.h>
#include <oct-obj.h>
#include <oct-map.h>
#include <ov-fcn-handle.h>
#include <parse.h>

static void
gripe_set_invalid (const std::string& pname)
{
  error ("set: invalid value for %s property", pname.c_str ());
}

static octave_value
nan_to_empty (double val)
{
  return xisnan (val) ? octave_value (Matrix ()) : octave_value (val);
}

static octave_value
empty_to_nan (const octave_value& val)
{
  return val.is_empty () ? octave_value (octave_NaN) : val;
}

// ---------------------------------------------------------------------

class color_property
{
public:
  color_property (double r = 0, double g = 0, double b = 1)
  {
    xrgb[0] = r;
    xrgb[1] = g;
    xrgb[2] = b;

    validate ();
  }

  color_property (char c)
  {
    c2rgb (c);
  }

  color_property (const octave_value& val)
  {
    // FIXME -- need some error checking here.

    Matrix m = val.matrix_value ();

    if (! error_state && m.numel () == 3)
      {
	for (int i = 0; i < m.numel (); i++)
	  xrgb[i] = m(i);

	validate ();
      }
    else 
      {
	std::string c = val.string_value ();

	if (! error_state && c.length () == 1)
	  c2rgb (c[0]);
	else
	  error ("invalid color specification");
      }
  }

  void validate (void) const
  {
    for (int i = 0; i < 3; i++)
      {
	if (xrgb[i] < 0 ||  xrgb[i] > 1)
	  {
	    error ("invalid RGB color specification");
	    break;
	  }
      }
  }

  operator octave_value (void) const
  {
    Matrix retval (1, 3);

    for (int i = 0; i < 3 ; i++)
      retval(i) = xrgb[i];

    return retval;
  }

  const double* rgb (void) const
  {
    return xrgb;
  }

private:
  double xrgb[3];

  void c2rgb (char c)
  {
    double tmp_rgb[4] = {0, 0, 0};

    switch(c) 
      {
      case 'r':
	tmp_rgb[0] = 1;	
	break;	

      case 'g': 
	tmp_rgb[1] = 1;
	break;

      case 'b':
	tmp_rgb[2] = 1; 
	break;

      case 'c': 	
	tmp_rgb[1] = tmp_rgb[2] = 1;
	break;

      case 'm':
	tmp_rgb[0] = tmp_rgb[2] = 1;
	break;

      case 'y': 
	tmp_rgb[0] = tmp_rgb[1] = 1;
	break;

      case 'w': 
	tmp_rgb[0] = tmp_rgb[1] = tmp_rgb[2] = 1;
	break;

      default:
	error ("invalid color specification");
      }

    if (! error_state)
      {
	for (int i = 0; i < 4; i++)
	  xrgb[i] = tmp_rgb[i];
      }
  }
};

class colormap_property
{
public:
  colormap_property (const Matrix& m = Matrix ())
    : cmap (m)
  {
    if (cmap.is_empty ())
      {
	cmap = Matrix (64, 3);

	for (octave_idx_type i = 0; i < 64; i++)
	  cmap(i,0) = cmap(i,1) = cmap(i,2) = i / 64.0;
      }

    validate ();
  }

  colormap_property (const octave_value& val)
  {
    cmap = val.matrix_value ();

    validate ();
  }

  void validate (void) const
  {
    if (error_state || cmap.columns () != 3)
      error ("invalid colormap specification");
  }

  operator octave_value (void) const { return cmap; }

private:
  Matrix cmap;
};

// ---------------------------------------------------------------------

class property_name : public std::string
{
public:
  typedef std::string::iterator iterator;
  typedef std::string::const_iterator const_iterator;

  property_name (void) : std::string () { }
  property_name (const std::string& s) : std::string (s) { }
  property_name (const char *s) : std::string (s) { }

  property_name (const property_name& name) : std::string (name) { }

  property_name& operator = (const property_name& pname)
  {
    std::string::operator = (pname);
    return *this;
  }

  operator std::string (void) const { return *this; }

  // Case-insensitive comparison.
  bool compare (const std::string& s, size_t limit = NPOS) const
  {
    const_iterator p1 = begin ();
    const_iterator p2 = s.begin ();

    size_t k = 0;

    while (p1 != end () && p2 != s.end () && k++ < limit)
      {
	if (std::tolower (*p1) != std::tolower (*p2))
	  return false;

	*p1++;
	*p2++;
      }

    return (limit == NPOS) ? size () == s.size () : k == limit;
  }
};

// ---------------------------------------------------------------------

class property_list
{
public:
  typedef std::map<std::string, octave_value> pval_map_type;
  typedef std::map<std::string, pval_map_type> plist_map_type;
  
  typedef pval_map_type::iterator pval_map_iterator;
  typedef pval_map_type::const_iterator pval_map_const_iterator;

  typedef plist_map_type::iterator plist_map_iterator;
  typedef plist_map_type::const_iterator plist_map_const_iterator;

  property_list (const plist_map_type& m = plist_map_type ())
    : plist_map (m) { }

  ~property_list (void) { }

  void set (const property_name& name, const octave_value& val)
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

	    if (pfx.compare ("image"))
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

  octave_value lookup (const property_name& name) const
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

	    if (pfx.compare ("image"))
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

  plist_map_iterator begin (void) { return plist_map.begin (); }
  plist_map_const_iterator begin (void) const { return plist_map.begin (); }

  plist_map_iterator end (void) { return plist_map.end (); }
  plist_map_const_iterator end (void) const { return plist_map.end (); }

  plist_map_iterator find (const std::string& go_name)
  {
    return plist_map.find (go_name);
  }

  plist_map_const_iterator find (const std::string& go_name) const
  {
    return plist_map.find (go_name);
  }

  Octave_map as_struct (const std::string& prefix_arg) const
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

private:
  plist_map_type plist_map;
};

// ---------------------------------------------------------------------

typedef double graphics_handle;

// ---------------------------------------------------------------------

class base_graphics_object
{
public:
  friend class graphics_object;

  base_graphics_object (void) : count (1) { }

  base_graphics_object (const base_graphics_object&) { }

  virtual ~base_graphics_object (void) { }

  virtual void mark_modified (void)
  {
    error ("base_graphics_object::mark_modified: invalid graphics object");
  }

  virtual void override_defaults (base_graphics_object&)
  {
    error ("base_graphics_object::override_defaults: invalid graphics object");
  }

  virtual void set_from_list (property_list&)
  {
    error ("base_graphics_object::set_from_list: invalid graphics object");
  }

  virtual void set (const property_name&, const octave_value&)
  {
    error ("base_graphics_object::set: invalid graphics object");
  }

  virtual void set_defaults (const std::string&)
  {
    error ("base_graphics_object::set_defaults: invalid graphics object");
  }

  virtual octave_value get (void) const
  {
    error ("base_graphics_object::get: invalid graphics object");
    return octave_value ();
  }

  virtual octave_value get (const property_name&) const
  {
    error ("base_graphics_object::get: invalid graphics object");
    return octave_value ();
  }

  virtual octave_value get_default (const property_name&) const;

  virtual octave_value get_factory_default (const property_name&) const;

  virtual octave_value get_defaults (void) const
  {
    error ("base_graphics_object::get_defaults: invalid graphics object");
    return octave_value ();
  }

  virtual octave_value get_factory_defaults (void) const
  {
    error ("base_graphics_object::get_factory_defaults: invalid graphics object");
    return octave_value ();
  }

  virtual graphics_handle get_parent (void) const
  {
    error ("base_graphics_object::get_parent: invalid graphics object");
    return octave_NaN;
  }

  virtual void remove_child (const graphics_handle&)
  {
    error ("base_graphics_object::remove_child: invalid graphics object");
  }

  virtual void adopt (const graphics_handle&)
  {
    error ("base_graphics_object::adopt: invalid graphics object");
  }

  virtual void reparent (const graphics_handle&)
  {
    error ("base_graphics_object::reparent: invalid graphics object");
  }

  virtual void defaults (void) const
  {
    error ("base_graphics_object::default: invalid graphics object");
  }

  virtual bool valid_object (void) const { return false; }

  virtual std::string type (void) const { return "unknown"; }

  bool isa (const std::string& go_name) const
  {
    return type () == go_name;
  }

protected:
  // A reference count.
  int count;
};

class graphics_object
{
public:
  graphics_object (void) : rep (new base_graphics_object ()) { }

  graphics_object (base_graphics_object *new_rep)
    : rep (new_rep) { }

  graphics_object (const graphics_object& obj)
  {
    rep = obj.rep;
    rep->count++;
  }

  graphics_object& operator = (const graphics_object& obj)
  {
    if (rep != obj.rep)
      {
	if (--rep->count == 0)
	  delete rep;

	rep = obj.rep;
	rep->count++;
      }

    return *this;
  }

  ~graphics_object (void)
  {
    if (--rep->count == 0)
      delete rep;
  }

  void mark_modified (void) { rep->mark_modified (); }

  void override_defaults (base_graphics_object& obj)
  {
    rep->override_defaults (obj);
  }

  void set_from_list (property_list& plist)
  {
    rep->set_from_list (plist);
  }

  void set (const property_name& name, const octave_value& val)
  {
    rep->set (name, val);
  }

  void set (const octave_value_list& args)
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

  void set_defaults (const std::string& mode)
  {
    rep->set_defaults (mode);
  }

  octave_value get (void) const
  {
    return rep->get ();
  }

  octave_value get (const property_name& name) const
  {
    return name.compare ("default")
      ? get_defaults ()
      : (name.compare ("factory")
	 ? get_factory_defaults () : rep->get (name));
  }

  octave_value get_default (const property_name& name) const
  {
    return rep->get_default (name);
  }

  octave_value get_factory_default (const property_name& name) const
  {
    return rep->get_factory_default (name);
  }

  octave_value get_defaults (void) const { return rep->get_defaults (); }

  octave_value get_factory_defaults (void) const
  {
    return rep->get_factory_defaults ();
  }

  graphics_handle get_parent (void) const { return rep->get_parent (); }

  void remove_child (const graphics_handle& h) { return rep->remove_child (h); }

  void adopt (const graphics_handle& h) { return rep->adopt (h); }

  void reparent (const graphics_handle& h) { return rep->reparent (h); }

  void defaults (void) const { rep->defaults (); }

  bool isa (const std::string& go_name) const { return rep->isa (go_name); }

  bool valid_object (void) const { return rep->valid_object (); }

  operator bool (void) const { return rep->valid_object (); }

private:
  base_graphics_object *rep;
};

// ---------------------------------------------------------------------

class gh_manager
{
protected:

  gh_manager (void);

public:

  static bool instance_ok (void)
  {
    bool retval = true;

    if (! instance)
      instance = new gh_manager ();

    if (! instance)
      {
	::error ("unable to create gh_manager!");

	retval = false;
      }

    return retval;
  }

  static void free (const graphics_handle& h)
  {
    if (instance_ok ())
      instance->do_free (h);
  }

  static graphics_handle lookup (double val)
  {
    return instance_ok () ? instance->do_lookup (val) : graphics_handle ();
  }

  static graphics_object get_object (const graphics_handle& h)
  {
    return instance_ok () ? instance->do_get_object (h) : graphics_object ();
  }

  static graphics_handle
  make_graphics_handle (const std::string& go_name,
			const graphics_handle& parent)
  {
    return instance_ok ()
      ? instance->do_make_graphics_handle (go_name, parent) : octave_NaN;
  }

  static graphics_handle make_figure_handle (double val)
  {
    return instance_ok ()
      ? instance->do_make_figure_handle (val) : octave_NaN;
  }

  static void push_figure (const graphics_handle& h)
  {
    if (instance_ok ())
      instance->do_push_figure (h);
  }

  static void pop_figure (const graphics_handle& h)
  {
    if (instance_ok ())
      instance->do_pop_figure (h);
  }

  static graphics_handle current_figure (void)
  {
    return instance_ok () ? instance->do_current_figure () : octave_NaN;
  }

  static Matrix handle_list (void)
  {
    return instance_ok () ? instance->do_handle_list () : Matrix ();
  }

  static Matrix figure_handle_list (void)
  {
    return instance_ok () ? instance->do_figure_handle_list () : Matrix ();
  }

private:

  static gh_manager *instance;

  typedef std::map<graphics_handle, graphics_object>::iterator iterator;
  typedef std::map<graphics_handle, graphics_object>::const_iterator const_iterator;

  typedef std::set<graphics_handle>::iterator free_list_iterator;
  typedef std::set<graphics_handle>::const_iterator const_free_list_iterator;

  typedef std::list<graphics_handle>::iterator figure_list_iterator;
  typedef std::list<graphics_handle>::const_iterator const_figure_list_iterator;

  // A map of handles to graphics objects.
  std::map<graphics_handle, graphics_object> handle_map;

  // The available graphics handles.
  std::set<graphics_handle> handle_free_list;

  // The next handle available if handle_free_list is empty.
  graphics_handle next_handle;

  // The allocated figure handles.  Top of the stack is most recently
  // created.
  std::list<graphics_handle> figure_list;

  graphics_handle get_handle (const std::string& go_name)
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

  void do_free (const graphics_handle& h)
  {
    if (h != 0)
      {
	iterator p = handle_map.find (h);

	if (p != handle_map.end ())
	  {
	    handle_map.erase (p);

	    if (h < 0)
	      handle_free_list.insert (h);
	  }
	else
	  error ("graphics_handle::free: invalid object %g", h);
      }
    else
      error ("graphics_handle::free: can't delete root figure");
  }

  graphics_handle do_lookup (double val)
  {
    iterator p = handle_map.find (val);

    return (p != handle_map.end ()) ? p->first : octave_NaN;
  }

  graphics_object do_get_object (const graphics_handle& h)
  {
    iterator p = handle_map.find (h);

    return (p != handle_map.end ()) ? p->second : graphics_object ();
  }

  graphics_handle do_make_graphics_handle (const std::string& go_name,
					   const graphics_handle& p);

  graphics_handle do_make_figure_handle (double val);

  Matrix do_handle_list (void)
  {
    Matrix retval (1, handle_map.size ());
    octave_idx_type i = 0;
    for (const_iterator p = handle_map.begin (); p != handle_map.end (); p++)
      retval(i++) = p->first;
    return retval;
  }

  Matrix do_figure_handle_list (void)
  {
    Matrix retval (1, figure_list.size ());
    octave_idx_type i = 0;
    for (const_figure_list_iterator p = figure_list.begin ();
	 p != figure_list.end ();
	 p++)
      retval(i++) = *p;
    return retval;
  }

  void do_push_figure (const graphics_handle& h);

  void do_pop_figure (const graphics_handle& h);

  graphics_handle do_current_figure (void) const
  {
    return figure_list.empty () ? octave_NaN : figure_list.front ();
  }
};

gh_manager *gh_manager::instance = 0;

// ---------------------------------------------------------------------

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

      if (! xisnan (h))
	{
	  graphics_object obj = gh_manager::get_object (h);
	  
	  graphics_handle parent_h = obj.get_parent ();

	  graphics_object parent_obj = gh_manager::get_object (parent_h);

	  parent_obj.remove_child (h);

	  if (adopt)
	    obj.set ("parent", new_parent);
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
  return ! xisnan (gh_manager::lookup (val));
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

class base_properties
{
public:
  base_properties (const std::string& t = "unknown",
		   const graphics_handle& mh = octave_NaN,
		   const graphics_handle& p = octave_NaN)
    : type (t), __modified__ (true), __myhandle__ (mh), parent (p),
      children () { }

  virtual ~base_properties (void) { }

  virtual std::string graphics_object_name (void) const = 0;

  void mark_modified (void)
  {
    __modified__ = true;
    graphics_object parent_obj = gh_manager::get_object (parent);
    parent_obj.mark_modified ();
  }

  void override_defaults (base_graphics_object& obj)
  {
    graphics_object parent_obj = gh_manager::get_object (parent);
    parent_obj.override_defaults (obj);
  }

  // Look through DEFAULTS for properties with given CLASS_NAME, and
  // apply them to the current object with set (virtual method).

  void set_from_list (base_graphics_object& obj, property_list& defaults)
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

  virtual void set (const property_name& name, const octave_value& val) = 0;

  graphics_handle get_parent (void) const { return parent; }

  void remove_child (const graphics_handle& h)
  {
    octave_idx_type k = -1;
    octave_idx_type n = children.numel ();
    for (octave_idx_type i = 0; i < n; i++)
      {
	if (h == children(i))
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

  void adopt (const graphics_handle& h)
  {
    octave_idx_type n = children.numel ();
    children.resize (1, n+1);
    children(n) = h;
  }

  void set_parent (const octave_value& val)
  {
    double tmp = val.double_value ();

    graphics_handle new_parent = octave_NaN;

    if (! error_state)
      {
	new_parent = gh_manager::lookup (tmp);

	if (! xisnan (new_parent))
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

  void reparent (const graphics_handle& new_parent) { parent = new_parent; }

  virtual void delete_children (void)
  {
    octave_idx_type n = children.numel ();

    for (octave_idx_type i = 0; i < n; i++)
      gh_manager::free (children(i));
  }

protected:
  std::string type;
  bool __modified__;
  graphics_handle __myhandle__;
  graphics_handle parent;
  Matrix children;
};

// ---------------------------------------------------------------------

class root_figure : public base_graphics_object
{
public:
  class root_figure_properties : public base_properties
  {
  public:
    root_figure_properties (void)
      : base_properties ("root figure", 0, octave_NaN),
	currentfigure (octave_NaN),
	visible ("on")
    { }

    ~root_figure_properties (void) { }

    void set (const property_name& name, const octave_value& val)
    {
      if (name.compare ("currentfigure"))
	{
	  octave_value tval = empty_to_nan (val);

	  if (is_handle (tval))
	    {
	      currentfigure = tval.double_value ();

	      gh_manager::push_figure (currentfigure);
	    }
	  else
	    gripe_set_invalid ("currentfigure");
	}
      else if (name.compare ("children"))
	children = maybe_set_children (children, val);
      else if (name.compare ("visible"))
	visible = val;
      else
	warning ("set: invalid property `%s'", name.c_str ());
    }

    octave_value get (void) const
    {
      Octave_map m;

      m.assign ("type", type);
      m.assign ("currentfigure", nan_to_empty (currentfigure));
      m.assign ("children", children);
      m.assign ("visible", visible);

      return m;
    }

    octave_value get (const property_name& name) const
    {
      octave_value retval;

      if (name.compare ("type"))
	retval = type;
      else if (name.compare ("currentfigure"))
	retval = nan_to_empty (currentfigure);
      else if (name.compare ("children"))
	retval = children;
      else if (name.compare ("visible"))
	retval = visible;
      else
	warning ("get: invalid property `%s'", name.c_str ());

      return retval;
    }

    std::string graphics_object_name (void) const { return go_name; }

  private:
    graphics_handle currentfigure;
    octave_value visible;

    static std::string go_name;
  };

  root_figure_properties properties;

public:

  root_figure (void) : properties (), default_properties () { }

  ~root_figure (void) { properties.delete_children (); }

  std::string type (void) const { return properties.graphics_object_name (); }

  void mark_modified (void) { }

  void override_defaults (base_graphics_object& obj)
  {
    // Now override with our defaults.  If the default_properties
    // list includes the properties for all defaults (line,
    // surface, etc.) then we don't have to know the type of OBJ
    // here, we just call its set function and let it decide which
    // properties from the list to use.
    obj.set_from_list (default_properties);
  }

  void set_from_list (property_list& plist)
  {
    properties.set_from_list (*this, plist);
  }

  void set (const property_name& name, const octave_value& value)
  {
    if (name.compare ("default", 7))
      // strip "default", pass rest to function that will
      // parse the remainder and add the element to the
      // default_properties map.
      default_properties.set (name.substr (7), value);
    else
      properties.set (name, value);
  }

  octave_value get (void) const
  {
    return properties.get ();
  }

  octave_value get (const property_name& name) const
  {
    octave_value retval;

    if (name.compare ("default", 7))
      return get_default (name.substr (7));
    else if (name.compare ("factory", 7))
      return get_factory_default (name.substr (7));
    else
      retval = properties.get (name);

    return retval;
  }

  octave_value get_default (const property_name& name) const
  {
    octave_value retval = default_properties.lookup (name);

    if (retval.is_undefined ())
      error ("get: invalid default property `%s'", name.c_str ());

    return retval;
  }

  octave_value get_factory_default (const property_name& name) const
  {
    octave_value retval = factory_properties.lookup (name);

    if (retval.is_undefined ())
      error ("get: invalid factory default property `%s'", name.c_str ());

    return retval;
  }

  octave_value get_defaults (void) const
  {
    return default_properties.as_struct ("default");
  }

  octave_value get_factory_defaults (void) const
  {
    return factory_properties.as_struct ("factory");
  }

  graphics_handle get_parent (void) const { return properties.get_parent (); }

  void remove_child (const graphics_handle& h) { properties.remove_child (h); }

  void adopt (const graphics_handle& h) { properties.adopt (h); }

  void reparent (const graphics_handle& np) { properties.reparent (np); }

  bool valid_object (void) const { return true; }

private:
  property_list default_properties;

  static property_list factory_properties;

  static property_list::plist_map_type init_factory_properties (void);
};

property_list
root_figure::factory_properties = root_figure::init_factory_properties ();

std::string root_figure::root_figure_properties::go_name ("root figure");

// ---------------------------------------------------------------------

class figure : public base_graphics_object
{
public:
  class figure_properties : public base_properties
  {
  public:
    figure_properties (const graphics_handle& mh, const graphics_handle& p)
      : base_properties (go_name, mh, p),
	__plot_stream__ (Matrix ()),
	nextplot ("replace"),
	closerequestfcn (make_fcn_handle ("closereq")),
	currentaxes (octave_NaN),
	colormap (),
	visible ("on"),
	paperorientation ("portrait")
    { }

    ~figure_properties (void) { }

    void set (const property_name& name, const octave_value& val)
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
	__plot_stream__ = val;
      else if (name.compare ("nextplot"))
	nextplot = val;
      else if (name.compare ("closerequestfcn"))
	closerequestfcn = val;
      else if (name.compare ("currentaxes"))
	{
	  octave_value tval = empty_to_nan (val);
	    
	  if (is_handle (tval))
	    currentaxes = tval.double_value ();
	  else
	    gripe_set_invalid ("currentaxes");
	}
      else if (name.compare ("colormap"))
	colormap = colormap_property (val);
      else if (name.compare ("visible"))
	{
	  std::string s = val.string_value ();

	  if (! error_state)
	    {
	      if (s == "on")
		xset (0, "currentfigure", __myhandle__);

	      visible = val;
	    }
	}
      else if (name.compare ("paperorientation"))
	paperorientation = val;
      else
	{
	  modified = false;
	  warning ("set: invalid property `%s'", name.c_str ());
	}

      if (modified)
	mark_modified ();
    }

    octave_value get (void) const
    {
      Octave_map m;

      m.assign ("type", type);
      m.assign ("parent", parent);
      m.assign ("children", children);
      m.assign ("__modified__", __modified__);
      m.assign ("__plot_stream__", __plot_stream__);
      m.assign ("nextplot", nextplot);
      m.assign ("closerequestfcn", closerequestfcn);
      m.assign ("currentaxes", nan_to_empty (currentaxes));
      m.assign ("colormap", colormap);
      m.assign ("visible", visible);
      m.assign ("paperorientation", paperorientation);

      return m;
    }

    octave_value get (const property_name& name) const
    {
      octave_value retval;

      if (name.compare ("type"))
	retval = type;
      else if (name.compare ("parent"))
	retval = parent;
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
	retval = nan_to_empty (currentaxes);
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

    void close (void)
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

      xset (0, "currentfigure", gh_manager::current_figure ());
    }

    std::string graphics_object_name (void) const { return go_name; }

    static property_list::pval_map_type factory_defaults (void)
    {
      property_list::pval_map_type m;

      m["nextplot"] = "replace";
      // m["closerequestfcn"] = make_fcn_handle ("closereq");
      m["colormap"] = colormap_property ();
      m["visible"] = "on";
      m["paperorientation"] = "portrait";

      return m;
    }

  private:
    octave_value __plot_stream__;
    octave_value nextplot;
    octave_value closerequestfcn;
    graphics_handle currentaxes;
    colormap_property colormap;
    octave_value visible;
    octave_value paperorientation;

    static std::string go_name;
  };

  figure_properties properties;

public:
  figure (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), properties (mh, p), default_properties ()
  {
    properties.override_defaults (*this);
  }

  ~figure (void)
  {
    properties.delete_children ();
    properties.close ();
  }

  std::string type (void) const { return properties.graphics_object_name (); }

  void mark_modified (void) { properties.mark_modified (); }

  void override_defaults (base_graphics_object& obj)
  {
    // Allow parent (root figure) to override first (properties knows how
    // to find the parent object).
    properties.override_defaults (obj);

    // Now override with our defaults.  If the default_properties
    // list includes the properties for all defaults (line,
    // surface, etc.) then we don't have to know the type of OBJ
    // here, we just call its set function and let it decide which
    // properties from the list to use.
    obj.set_from_list (default_properties);
  }

  void set_from_list (property_list& plist)
  {
    properties.set_from_list (*this, plist);
  }

  void set (const property_name& name, const octave_value& value)
  {
    if (name.compare ("default", 7))
      // strip "default", pass rest to function that will
      // parse the remainder and add the element to the
      // default_properties map.
      default_properties.set (name.substr (7), value);
    else
      properties.set (name, value);
  }

  octave_value get (void) const
  {
    return properties.get ();
  }

  octave_value get (const property_name& name) const
  {
    octave_value retval;

    if (name.compare ("default", 7))
      retval = get_default (name.substr (7));
    else
      retval = properties.get (name);

    return retval;
  }

  octave_value get_default (const property_name& name) const
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

  octave_value get_defaults (void) const
  {
    return default_properties.as_struct ("default");
  }

  graphics_handle get_parent (void) const { return properties.get_parent (); }

  void remove_child (const graphics_handle& h) { properties.remove_child (h); }

  void adopt (const graphics_handle& h) { properties.adopt (h); }

  void reparent (const graphics_handle& np) { properties.reparent (np); }

  bool valid_object (void) const { return true; }

private:
  property_list default_properties;
};

std::string figure::figure_properties::go_name ("figure");

// ---------------------------------------------------------------------

class axes : public base_graphics_object
{
public:
  class axes_properties : public base_properties
  {
  public:
    axes_properties (const graphics_handle& mh, const graphics_handle& p)
      : base_properties (go_name, mh, p),
	position (Matrix ()),
	title (octave_NaN),
	box ("on"),
	key ("off"),
	keybox ("off"),
	keypos (1),
	dataaspectratio (Matrix (1, 3, 1.0)),
	dataaspectratiomode ("auto"),
	xlim (),
	ylim (),
	zlim (),
	xlimmode ("auto"),
	ylimmode ("auto"),
	zlimmode ("auto"),
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
	view (),
	nextplot ("replace"),
	outerposition ()
    {
      Matrix tlim (1, 2, 0.0);
      tlim(1) = 1;
      xlim = tlim;
      ylim = tlim;
      zlim = tlim;

      Matrix tview (1, 2, 0.0);
      tview(1) = 90;
      view = tview;

      Matrix touterposition (1, 4, 0.0);
      touterposition(2) = 1;
      touterposition(3) = 1;
      outerposition = touterposition;
    }

    ~axes_properties (void) { }

    void set (const property_name& name, const octave_value& val)
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
	position = val;
      else if (name.compare ("title"))
	{
	  graphics_handle h = ::reparent (val, "set", "title",
					  __myhandle__, false);
	  if (! error_state)
	    {
	      if (! xisnan (title))
		gh_manager::free (title);

	      title = h;
	    }
	}
      else if (name.compare ("box"))
	box = val;
      else if (name.compare ("key"))
	key = val;
      else if (name.compare ("keybox"))
	keybox = val;
      else if (name.compare ("keypos"))
	keypos = val;
      else if (name.compare ("dataaspectratio"))
	{
	  dataaspectratio = val;
	  dataaspectratiomode = "manual";
	}
      else if (name.compare ("dataaspectratiomode"))
	dataaspectratiomode = val;
      else if (name.compare ("xlim"))
	{
	  xlim = val;
	  xlimmode = "manual";
	}
      else if (name.compare ("ylim"))
	{
	  ylim = val;
	  ylimmode = "manual";
	}
      else if (name.compare ("zlim"))
	{
	  zlim = val;
	  zlimmode = "manual";
	}
      else if (name.compare ("xlimmode"))
	xlimmode = val;
      else if (name.compare ("ylimmode"))
	ylimmode = val;
      else if (name.compare ("zlimmode"))
	zlimmode = val;
      else if (name.compare ("xlabel"))
	{
	  graphics_handle h = ::reparent (val, "set", "xlabel",
					  __myhandle__, false);
	  if (! error_state)
	    {
	      if (! xisnan (xlabel))
		gh_manager::free (xlabel);

	      xlabel = h;
	    }
	}
      else if (name.compare ("ylabel"))
	{
	  graphics_handle h = ::reparent (val, "set", "ylabel",
					  __myhandle__, false);
	  if (! error_state)
	    {
	      if (! xisnan (ylabel))
		gh_manager::free (ylabel);

	      ylabel = h;
	    }
	}
      else if (name.compare ("zlabel"))
	{
	  graphics_handle h = ::reparent (val, "set", "zlabel",
					  __myhandle__, false);
	  if (! error_state)
	    {
	      if (! xisnan (zlabel))
		gh_manager::free (zlabel);

	      zlabel = h;
	    }
	}
      else if (name.compare ("xgrid"))
	xgrid = val;
      else if (name.compare ("ygrid"))
	ygrid = val;
      else if (name.compare ("zgrid"))
	zgrid = val;
      else if (name.compare ("xminorgrid"))
	xminorgrid = val;
      else if (name.compare ("yminorgrid"))
	yminorgrid = val;
      else if (name.compare ("zminorgrid"))
	zminorgrid = val;
      else if (name.compare ("xtick"))
	{
	  xtick = val;
	  xtickmode = "manual";
	}
      else if (name.compare ("ytick"))
	{
	  ytick = val;
	  ytickmode = "manual";
	}
      else if (name.compare ("ztick"))
	{
	  ztick = val;
	  ztickmode = "manual";
	}
      else if (name.compare ("xtickmode"))
	xtickmode = val;
      else if (name.compare ("ytickmode"))
	ytickmode = val;
      else if (name.compare ("ztickmode"))
	ztickmode = val;
      else if (name.compare ("xticklabel"))
	{
	  xticklabel = val;
	  xticklabelmode = "manual";
	}
      else if (name.compare ("yticklabel"))
	{
	  yticklabel = val;
	  yticklabelmode = "manual";
	}
      else if (name.compare ("zticklabel"))
	{
	  zticklabel = val;
	  zticklabelmode = "manual";
	}
      else if (name.compare ("xticklabelmode"))
	xticklabelmode = val;
      else if (name.compare ("yticklabelmode"))
	yticklabelmode = val;
      else if (name.compare ("zticklabelmode"))
	zticklabelmode = val;
      else if (name.compare ("xscale"))
	xscale = val;
      else if (name.compare ("yscale"))
	yscale = val;
      else if (name.compare ("zscale"))
	zscale = val;
      else if (name.compare ("xdir"))
	xdir = val;
      else if (name.compare ("ydir"))
	ydir = val;
      else if (name.compare ("zdir"))
	zdir = val;
      else if (name.compare ("view"))
	view = val;
      else if (name.compare ("nextplot"))
	nextplot = val;
      else if (name.compare ("outerposition"))
	outerposition = val;
      else
	{
	  modified = false;
	  warning ("set: invalid property `%s'", name.c_str ());
	}

      if (modified)
	mark_modified ();
    }

    void set_defaults (base_graphics_object& obj, const std::string& mode)
    {
      position = Matrix ();
      title = octave_NaN;
      box = "on";
      key = "off";
      keybox = "off";
      keypos = 1;
      dataaspectratio = Matrix (1, 3, 1.0);
      dataaspectratiomode = "auto";

      Matrix tlim (1, 2, 0.0);
      tlim(1) = 1;
      xlim = tlim;
      ylim = tlim;
      zlim = tlim;

      xlimmode = "auto";
      ylimmode = "auto";
      zlimmode = "auto";
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

      Matrix tview (1, 2, 0.0);
      tview(1) = 90;
      view = tview;

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

    octave_value get (void) const
    {
      Octave_map m;
      
      if (xisnan (title))
	title = gh_manager::make_graphics_handle ("text", __myhandle__);

      if (xisnan (xlabel))
	xlabel = gh_manager::make_graphics_handle ("text", __myhandle__);

      if (xisnan (ylabel))
	ylabel = gh_manager::make_graphics_handle ("text", __myhandle__);

      if (xisnan (zlabel))
	zlabel = gh_manager::make_graphics_handle ("text", __myhandle__);

      m.assign ("type", type);
      m.assign ("parent", parent);
      m.assign ("children", children);
      m.assign ("__modified__", __modified__);
      m.assign ("position", position);
      m.assign ("title", title);
      m.assign ("box", box);
      m.assign ("key", key);
      m.assign ("keybox", keybox);
      m.assign ("keypos", keypos);
      m.assign ("dataaspectratio", dataaspectratio);
      m.assign ("dataaspectratiomode", dataaspectratiomode);
      m.assign ("xlim", xlim);
      m.assign ("ylim", ylim);
      m.assign ("zlim", zlim);
      m.assign ("xlimmode", xlimmode);
      m.assign ("ylimmode", ylimmode);
      m.assign ("zlimmode", zlimmode);
      m.assign ("xlabel", xlabel);
      m.assign ("ylabel", ylabel);
      m.assign ("zlabel", zlabel);
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
      m.assign ("view", view);
      m.assign ("nextplot", nextplot);
      m.assign ("outerposition", outerposition);

      return m;
    }

    octave_value get (const property_name& name) const
    {
      octave_value retval;

      if (name.compare ("type"))
	retval = type;
      else if (name.compare ("parent"))
	retval = parent;
      else if (name.compare ("children"))
	retval = children;
      else if (name.compare ("__modified__"))
	retval = __modified__;
      else if (name.compare ("position"))
	retval = position;
      else if (name.compare ("title"))
	{
	  if (xisnan (title))
	    title = gh_manager::make_graphics_handle ("text", __myhandle__);

	  retval = title;
	}
      else if (name.compare ("box"))
	retval = box;
      else if (name.compare ("key"))
	retval = key;
      else if (name.compare ("keybox"))
	retval = keybox;
      else if (name.compare ("keypos"))
	retval = keypos;
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
      else if (name.compare ("xlimmode"))
	retval = xlimmode;
      else if (name.compare ("ylimmode"))
	retval = ylimmode;
      else if (name.compare ("zlimmode"))
	retval = zlimmode;
      else if (name.compare ("xlabel"))
	{
	  if (xisnan (xlabel))
	    xlabel = gh_manager::make_graphics_handle ("text", __myhandle__);

	  retval = xlabel;
	}
      else if (name.compare ("ylabel"))
	{
	  if (xisnan (ylabel))
	    ylabel = gh_manager::make_graphics_handle ("text", __myhandle__);

	  retval = ylabel;
	}
      else if (name.compare ("zlabel"))
	{
	  if (xisnan (zlabel))
	    zlabel = gh_manager::make_graphics_handle ("text", __myhandle__);

	  retval = zlabel;
	}
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
      else if (name.compare ("view"))
	retval = view;
      else if (name.compare ("nextplot"))
	retval = nextplot;
      else if (name.compare ("outerposition"))
	retval = outerposition;
      else
	warning ("get: invalid property `%s'", name.c_str ());

      return retval;
    }

    void remove_child (const graphics_handle& h)
    {
      if (! xisnan (title) && h == title)
	title = gh_manager::make_graphics_handle ("text", __myhandle__);
      else if (! xisnan (xlabel) && h == xlabel)
	xlabel = gh_manager::make_graphics_handle ("text", __myhandle__);
      else if (! xisnan (ylabel) && h == ylabel)
	ylabel = gh_manager::make_graphics_handle ("text", __myhandle__);
      else if (! xisnan (zlabel) && h == zlabel)
	zlabel = gh_manager::make_graphics_handle ("text", __myhandle__);
      else
	base_properties::remove_child (h);
    }

    void delete_children (void)
    {
      base_properties::delete_children ();

      if (! xisnan (title))
	gh_manager::free (title);

      if (! xisnan (xlabel))
	gh_manager::free (xlabel);

      if (! xisnan (ylabel))
	gh_manager::free (ylabel);

      if (! xisnan (zlabel))
	gh_manager::free (zlabel);
    }

    std::string graphics_object_name (void) const { return go_name; }

    static property_list::pval_map_type factory_defaults (void)
    {
      property_list::pval_map_type m;

      m["position"] = Matrix ();
      m["title"] = octave_NaN;
      m["box"] = "on";
      m["key"] = "off";
      m["keybox"] = "off";
      m["keypos"] = 1;
      m["dataaspectratio"] = Matrix (1, 3, 1.0);
      m["dataaspectratiomode"] = "auto";

      Matrix tlim (1, 2, 0.0);
      tlim(1) = 1;

      m["xlim"] = tlim;
      m["ylim"] = tlim;
      m["zlim"] = tlim;

      m["xlimmode"] = "auto";
      m["ylimmode"] = "auto";
      m["zlimmode"] = "auto";
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

      Matrix tview (1, 2, 0.0);
      tview(1) = 90;

      m["view"] = tview;

      m["nextplot"] = "replace";

      Matrix touterposition (1, 4, 0.0);
      touterposition(2) = 1;
      touterposition(3) = 1;

      m["outerposition"] = touterposition;

      return m;
    }

  private:
    octave_value position;
    mutable graphics_handle title;
    octave_value box;
    octave_value key;
    octave_value keybox;
    octave_value keypos;
    octave_value dataaspectratio;
    octave_value dataaspectratiomode;
    octave_value xlim;
    octave_value ylim;
    octave_value zlim;
    octave_value xlimmode;
    octave_value ylimmode;
    octave_value zlimmode;
    mutable graphics_handle xlabel;
    mutable graphics_handle ylabel;
    mutable graphics_handle zlabel;
    octave_value xgrid;
    octave_value ygrid;
    octave_value zgrid;
    octave_value xminorgrid;
    octave_value yminorgrid;
    octave_value zminorgrid;
    octave_value xtick;
    octave_value ytick;
    octave_value ztick;
    octave_value xtickmode;
    octave_value ytickmode;
    octave_value ztickmode;
    octave_value xticklabel;
    octave_value yticklabel;
    octave_value zticklabel;
    octave_value xticklabelmode;
    octave_value yticklabelmode;
    octave_value zticklabelmode;
    octave_value xscale;
    octave_value yscale;
    octave_value zscale;
    octave_value xdir;
    octave_value ydir;
    octave_value zdir;
    octave_value view;
    octave_value nextplot;
    octave_value outerposition;

    static std::string go_name;
  };

  axes_properties properties;

public:
  axes (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), properties (mh, p), default_properties ()
  {
    properties.override_defaults (*this);
  }

  ~axes (void) { properties.delete_children (); }

  std::string type (void) const { return properties.graphics_object_name (); }

  void mark_modified (void) { properties.mark_modified (); }

  void override_defaults (base_graphics_object& obj)
  {
    // Allow parent (figure) to override first (properties knows how
    // to find the parent object).
    properties.override_defaults (obj);

    // Now override with our defaults.  If the default_properties
    // list includes the properties for all defaults (line,
    // surface, etc.) then we don't have to know the type of OBJ
    // here, we just call its set function and let it decide which
    // properties from the list to use.
    obj.set_from_list (default_properties);
  }

  void set_from_list (property_list& plist)
  {
    properties.set_from_list (*this, plist);
  }

  void set (const property_name& name, const octave_value& value)
  {
    if (name.compare ("default", 7))
      // strip "default", pass rest to function that will
      // parse the remainder and add the element to the
      // default_properties map.
      default_properties.set (name.substr (7), value);
    else
      properties.set (name, value);
  }

  void set_defaults (const std::string& mode)
  {
    properties.set_defaults (*this, mode);
  }

  octave_value get (void) const
  {
    return properties.get ();
  }

  octave_value get (const property_name& name) const
  {
    octave_value retval;

    // FIXME -- finish this.
    if (name.compare ("default", 7))
      retval = get_default (name.substr (7));
    else
      retval = properties.get (name);

    return retval;
  }

  octave_value get_default (const property_name& name) const
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

  octave_value get_defaults (void) const
  {
    return default_properties.as_struct ("default");
  }

  graphics_handle get_parent (void) const { return properties.get_parent (); }

  void remove_child (const graphics_handle& h) { properties.remove_child (h); }

  void adopt (const graphics_handle& h) { properties.adopt (h); }

  void reparent (const graphics_handle& np) { properties.reparent (np); }

  bool valid_object (void) const { return true; }

private:
  property_list default_properties;
};

std::string axes::axes_properties::go_name ("axes");

// ---------------------------------------------------------------------

static Matrix
default_data (void)
{
  Matrix retval (1, 2);

  retval(0) = 0;
  retval(1) = 1;

  return retval;
}

class line : public base_graphics_object
{
public:
  class line_properties : public base_properties
  {
  public:
    line_properties (const graphics_handle& mh, const graphics_handle& p)
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
	keylabel ("") { }

    ~line_properties (void) { }

    void set (const property_name& name, const octave_value& val)
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
	xdata = val;
      else if (name.compare ("ydata"))
	ydata = val;
      else if (name.compare ("zdata"))
	zdata = val;
      else if (name.compare ("ldata"))
	ldata = val;
      else if (name.compare ("udata"))
	udata = val;
      else if (name.compare ("xldata"))
	xldata = val;
      else if (name.compare ("xudata"))
	xudata = val;
      else if (name.compare ("color"))
	color = color_property (val);
      else if (name.compare ("linestyle"))
	linestyle = val;
      else if (name.compare ("linewidth"))
	linewidth = val;
      else if (name.compare ("marker"))
	marker = val;
      else if (name.compare ("markeredgecolor"))
	markeredgecolor = val;
      else if (name.compare ("markerfacecolor"))
	markerfacecolor = val;
      else if (name.compare ("markersize"))
	markersize = val;
      else if (name.compare ("keylabel"))
	keylabel = val;
      else
	{
	  modified = false;
	  warning ("set: invalid property `%s'", name.c_str ());
	}

      if (modified)
	mark_modified ();
    }

    octave_value get (void) const
    {
      Octave_map m;

      m.assign ("type", type);
      m.assign ("parent", parent);
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

    octave_value get (const property_name& name) const
    {
      octave_value retval;

      if (name.compare ("type"))
	retval = type;
      else if (name.compare ("parent"))
	retval = parent;
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

    std::string graphics_object_name (void) const { return go_name; }

    static property_list::pval_map_type factory_defaults (void)
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

  private:
    octave_value xdata;
    octave_value ydata;
    octave_value zdata;
    octave_value ldata;
    octave_value udata;
    octave_value xldata;
    octave_value xudata;
    color_property color;
    octave_value linestyle;
    octave_value linewidth;
    octave_value marker;
    octave_value markeredgecolor;
    octave_value markerfacecolor;
    octave_value markersize;
    octave_value keylabel;

    static std::string go_name;
  };

  line_properties properties;

public:
  line (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), properties (mh, p)
  {
    properties.override_defaults (*this);
  }

  ~line (void) { properties.delete_children (); }

  std::string type (void) const { return properties.graphics_object_name (); }

  void mark_modified (void) { properties.mark_modified (); }

  void override_defaults (base_graphics_object& obj)
  {
    // Allow parent (figure) to override first (properties knows how
    // to find the parent object).
    properties.override_defaults (obj);
  }

  void set_from_list (property_list& plist)
  {
    properties.set_from_list (*this, plist);
  }

  void set (const property_name& name, const octave_value& val)
  {
    properties.set (name, val);
  }

  octave_value get (void) const
  {
    return properties.get ();
  }

  octave_value get (const property_name& name) const
  {
    return properties.get (name);
  }

  graphics_handle get_parent (void) const { return properties.get_parent (); }

  void remove_child (const graphics_handle& h) { properties.remove_child (h); }

  void adopt (const graphics_handle& h) { properties.adopt (h); }

  void reparent (const graphics_handle& h) { properties.reparent (h); }

  bool valid_object (void) const { return true; }
};

std::string line::line_properties::go_name ("line");

// ---------------------------------------------------------------------

class text : public base_graphics_object
{
public:
  class text_properties : public base_properties
  {
  public:
    text_properties (const graphics_handle& mh, const graphics_handle& p)
      : base_properties (go_name, mh, p),
	string (""),
	units ("data"),
	position (Matrix (1, 3, 0.0)),
	horizontalalignment ("left")
    { }

    ~text_properties (void) { }

    void set (const property_name& name, const octave_value& val)
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
	string = val;
      else if (name.compare ("units"))
	units = val;
      else if (name.compare ("position"))
	position = val;
      else if (name.compare ("horizontalalignment"))
	horizontalalignment = val;
      else
	{
	  modified = false;
	  warning ("set: invalid property `%s'", name.c_str ());
	}

      if (modified)
	mark_modified ();
    }

    octave_value get (void) const
    {
      Octave_map m;

      m.assign ("type", type);
      m.assign ("parent", parent);
      m.assign ("children", children);
      m.assign ("__modified__", __modified__);
      m.assign ("string", string);
      m.assign ("units", units);
      m.assign ("position", position);
      m.assign ("horizontalalignment", horizontalalignment);

      return m;
    }

    octave_value get (const property_name& name) const
    {
      octave_value retval;

      if (name.compare ("type"))
	retval = type;
      else if (name.compare ("parent"))
	retval = parent;
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
      else if (name.compare ("horizontalalignment"))
	retval = horizontalalignment;
      else
	warning ("get: invalid property `%s'", name.c_str ());

      return retval;
    }

    std::string graphics_object_name (void) const { return go_name; }

    static property_list::pval_map_type factory_defaults (void)
    {
      property_list::pval_map_type m;

      m["string"] = "";
      m["units"] = "data";
      m["position"] = Matrix (1, 3, 0.0);
      m["horizontalalignment"] = "left";

      return m;
    }

  private:
    octave_value string;
    octave_value units;
    octave_value position;
    octave_value horizontalalignment;

    static std::string go_name;
  };

  text_properties properties;

public:
  text (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), properties (mh, p)
  {
    properties.override_defaults (*this);
  }

  ~text (void) { properties.delete_children (); }

  std::string type (void) const { return properties.graphics_object_name (); }

  void mark_modified (void) { properties.mark_modified (); }

  void override_defaults (base_graphics_object& obj)
  {
    // Allow parent (figure) to override first (properties knows how
    // to find the parent object).
    properties.override_defaults (obj);
  }

  void set_from_list (property_list& plist)
  {
    properties.set_from_list (*this, plist);
  }

  void set (const property_name& name, const octave_value& val)
  {
    properties.set (name, val);
  }

  octave_value get (void) const
  {
    return properties.get ();
  }

  octave_value get (const property_name& name) const
  {
    return properties.get (name);
  }

  graphics_handle get_parent (void) const { return properties.get_parent (); }

  void remove_child (const graphics_handle& h) { properties.remove_child (h); }

  void adopt (const graphics_handle& h) { properties.adopt (h); }

  void reparent (const graphics_handle& h) { properties.reparent (h); }

  bool valid_object (void) const { return true; }
};

std::string text::text_properties::go_name ("text");

// ---------------------------------------------------------------------

class image : public base_graphics_object
{
public:
  class image_properties : public base_properties
  {
  public:
    image_properties (const graphics_handle& mh, const graphics_handle& p)
      : base_properties (go_name, mh, p),
	cdata (Matrix ()),
	xdata (Matrix ()),
	ydata (Matrix ())
    { }

    ~image_properties (void) { }

    void set (const property_name& name, const octave_value& val)
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
	cdata = val;
      else if (name.compare ("xdata"))
	xdata = val;
      else if (name.compare ("ydata"))
	ydata = val;
      else
	{
	  modified = false;
	  warning ("set: invalid property `%s'", name.c_str ());
	}

      if (modified)
	mark_modified ();
    }

    octave_value get (void) const
    {
      Octave_map m;

      m.assign ("type", type);
      m.assign ("parent", parent);
      m.assign ("children", children);
      m.assign ("__modified__", __modified__);
      m.assign ("cdata", cdata);
      m.assign ("xdata", xdata);
      m.assign ("ydata", ydata);

      return m;
    }

    octave_value get (const property_name& name) const
    {
      octave_value retval;

      if (name.compare ("type"))
	retval = type;
      else if (name.compare ("parent"))
	retval = parent;
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

    std::string graphics_object_name (void) const { return go_name; }

    static property_list::pval_map_type factory_defaults (void)
    {
      property_list::pval_map_type m;

      m["cdata"] = Matrix ();
      m["xdata"] = Matrix ();
      m["ydata"] = Matrix ();

      return m;
    }

  private:
    octave_value cdata;
    octave_value xdata;
    octave_value ydata;

    static std::string go_name;
  };

  image_properties properties;

public:
  image (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), properties (mh, p)
  {
    properties.override_defaults (*this);
  }

  ~image (void) { properties.delete_children (); }

  std::string type (void) const { return properties.graphics_object_name (); }

  void mark_modified (void) { properties.mark_modified (); }

  void override_defaults (base_graphics_object& obj)
  {
    // Allow parent (figure) to override first (properties knows how
    // to find the parent object).
    properties.override_defaults (obj);
  }

  void set_from_list (property_list& plist)
  {
    properties.set_from_list (*this, plist);
  }

  void set (const property_name& name, const octave_value& val)
  {
    properties.set (name, val);
  }

  octave_value get (void) const
  {
    return properties.get ();
  }

  octave_value get (const property_name& name) const
  {
    return properties.get (name);
  }

  graphics_handle get_parent (void) const { return properties.get_parent (); }

  void remove_child (const graphics_handle& h) { properties.remove_child (h); }

  void adopt (const graphics_handle& h) { properties.adopt (h); }

  void reparent (const graphics_handle& h) { properties.reparent (h); }

  bool valid_object (void) const { return true; }
};

std::string image::image_properties::go_name ("image");

// ---------------------------------------------------------------------

class surface : public base_graphics_object
{
public:
  class surface_properties : public base_properties
  {
  public:
    surface_properties (const graphics_handle& mh, const graphics_handle& p)
      : base_properties (go_name, mh, p),
	xdata (Matrix ()),
	ydata (Matrix ()),
	zdata (Matrix ()),
	keylabel ("")
    { }

    ~surface_properties (void) { }

    void set (const property_name& name, const octave_value& val)
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
	xdata = val;
      else if (name.compare ("ydata"))
	ydata = val;
      else if (name.compare ("zdata"))
	zdata = val;
      else if (name.compare ("keylabel"))
	keylabel = val;
      else
	{
	  modified = false;
	  warning ("set: invalid property `%s'", name.c_str ());
	}

      if (modified)
	mark_modified ();
    }

    octave_value get (void) const
    {
      Octave_map m;

      m.assign ("type", type);
      m.assign ("parent", parent);
      m.assign ("children", children);
      m.assign ("__modified__", __modified__);
      m.assign ("xdata", xdata);
      m.assign ("ydata", ydata);
      m.assign ("zdata", zdata);
      m.assign ("keylabel", keylabel);

      return m;
    }

    octave_value get (const property_name& name) const
    {
      octave_value retval;

      if (name.compare ("type"))
	retval = type;
      else if (name.compare ("parent"))
	retval = parent;
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

    std::string graphics_object_name (void) const { return go_name; }

    static property_list::pval_map_type factory_defaults (void)
    {
      property_list::pval_map_type m;

      m["xdata"] = Matrix ();
      m["ydata"] = Matrix ();
      m["zdata"] = Matrix ();
      m["keylabel"] = "";

      return m;
    }

  private:
    octave_value xdata;
    octave_value ydata;
    octave_value zdata;
    octave_value keylabel;

    static std::string go_name;
  };

  surface_properties properties;

public:
  surface (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), properties (mh, p)
  {
    properties.override_defaults (*this);
  }

  ~surface (void) { properties.delete_children (); }

  std::string type (void) const { return properties.graphics_object_name (); }

  void mark_modified (void) { properties.mark_modified (); }

  void override_defaults (base_graphics_object& obj)
  {
    // Allow parent (figure) to override first (properties knows how
    // to find the parent object).
    properties.override_defaults (obj);
  }

  void set_from_list (property_list& plist)
  {
    properties.set_from_list (*this, plist);
  }

  void set (const property_name& name, const octave_value& val)
  {
    properties.set (name, val);
  }

  octave_value get (void) const
  {
    return properties.get ();
  }

  octave_value get (const property_name& name) const
  {
    return properties.get (name);
  }

  graphics_handle get_parent (void) const { return properties.get_parent (); }

  void remove_child (const graphics_handle& h) { properties.remove_child (h); }

  void adopt (const graphics_handle& h) { properties.adopt (h); }

  void reparent (const graphics_handle& h) { properties.reparent (h); }

  bool valid_object (void) const { return true; }
};

std::string surface::surface_properties::go_name ("surface");

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

  plist_map["figure"] = figure::figure_properties::factory_defaults ();
  plist_map["axes"] = axes::axes_properties::factory_defaults ();
  plist_map["line"] = line::line_properties::factory_defaults ();
  plist_map["text"] = text::text_properties::factory_defaults ();
  plist_map["image"] = image::image_properties::factory_defaults ();
  plist_map["surface"] = surface::surface_properties::factory_defaults ();

  return plist_map;
}

// ---------------------------------------------------------------------

DEFUN (ishandle, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Function File} {} ishandle (@var{h})\n\
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
@deftypefn {Function File} {} set (@var{h}, @var{p}, @var{v}, @dots{})\n\
Set the named property @var{p} to the value @var{v} in the graphics\n\
handle @var{h}.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin > 0)
    {
      double handle = args(0).double_value ();

      if (! error_state)
	{
	  graphics_object obj = gh_manager::get_object (handle);

	  if (obj)
	    {
	      obj.set (args.splice (0, 1));

	      feval ("__request_drawnow__");
	    }
	  else
	    error ("set: invalid handle (= %g)", handle);
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
@deftypefn {Function File} {} get (@var{h}, @var{p})\n\
Return the named property @var{p} from the graphics handle @var{h}.\n\
If @var{p} is omitted, return the complete property list for @var{h}.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      double handle = args(0).double_value ();

      if (! error_state)
	{
	  graphics_object obj = gh_manager::get_object (handle);

	  if (obj)
	    {
	      if (nargin == 1)
		retval = obj.get ();
	      else
		{
		  property_name property = args(1).string_value ();

		  if (! error_state)
		    retval = obj.get (property);
		  else
		    error ("get: expecting property name as second argument");
		}
	    }
	  else
	    error ("get: invalid handle (= %g)", handle);
	}
      else
	error ("get: expecting graphics handle as first argument");
    }
  else
    print_usage ();

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

      if (! xisnan (parent))
	{
	  graphics_handle h
	    = gh_manager::make_graphics_handle (go_name, parent);

	  if (! error_state)
	    {
	      adopt (parent, h);

	      xset (h, args.splice (0, 1));

	      retval = h;
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
Create a figure graphics object.\n\
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

	      retval = h;
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

	      if (! (error_state || xisnan (h)))
		{
		  adopt (0, h);

		  xset (h, args.splice (0, 1));

		  retval = h;
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
Create an axes graphics object.\n\
@end deftypefn")
{
  GO_BODY (axes);
}

DEFUN (__go_line__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_line__ (@var{parent})\n\
Create a line graphics object.\n\
@end deftypefn")
{
  GO_BODY (line);
}

DEFUN (__go_text__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_text__ (@var{parent})\n\
Create a text graphics object.\n\
@end deftypefn")
{
  GO_BODY (text);
}

DEFUN (__go_image__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_image__ (@var{parent})\n\
Create an image graphics object.\n\
@end deftypefn")
{
  GO_BODY (image);
}

DEFUN (__go_surface__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_surface__ (@var{parent})\n\
Create a surface graphics object.\n\
@end deftypefn")
{
  GO_BODY (surface);
}

DEFUN (__go_delete__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_delete__ (@var{h})\n\
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

	  if (! xisnan (h))
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
Initialize axes object.\n\
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

	  if (! xisnan (h))
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
Return current list of function handles.\n\
@end deftypefn")
{
  return octave_value (gh_manager::handle_list ());
}

DEFUN (__go_figure_handles__, , ,
   "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_figure_handles__ ()\n\
Return current list of function handles.\n\
@end deftypefn")
{
  return octave_value (gh_manager::figure_handle_list ());
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
