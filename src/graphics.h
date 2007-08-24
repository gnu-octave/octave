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

#if !defined (graphics_h)
#define graphics_h 1

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cctype>

#include <algorithm>
#include <list>
#include <map>
#include <set>
#include <string>

#include "oct-map.h"
#include "ov.h"

#define OCTAVE_GRAPHICS_PROPERTY_INTERNAL(TAG, TYPE, NAME) \
  private: TAG TYPE NAME; \
  public: const TYPE& get_ ## NAME () const { return NAME; } \
  private:

#define OCTAVE_GRAPHICS_PROPERTY(TYPE, NAME) \
  OCTAVE_GRAPHICS_PROPERTY_INTERNAL ( , TYPE, NAME)

#define OCTAVE_GRAPHICS_MUTABLE_PROPERTY(TYPE, NAME) \
  OCTAVE_GRAPHICS_PROPERTY_INTERNAL (mutable, TYPE, NAME)

class
radio_values
{
public:
  radio_values (const std::string& opt_string = std::string ());
  radio_values (const radio_values& a)
    : default_val (a.default_val), possible_vals (a.possible_vals) { }

  radio_values& operator = (const radio_values& a)
  {
    if (&a != this)
      {
	default_val = a.default_val;
	possible_vals = a.possible_vals;
      }

    return *this;
  }

  std::string default_value (void) const { return default_val; }

  std::set<std::string> possible_values (void) const { return possible_vals; }

  bool validate (const std::string& val)
  {
    bool retval = true;

    if (possible_vals.find (val) == possible_vals.end ())
      {
	error ("invalid value = %s", val.c_str ());
	retval = false;
      }

    return retval;
  }

private:
  // Might also want to cache
  std::string default_val;
  std::set<std::string> possible_vals;
};

class
radio_property
{
public:
  radio_property (const radio_values& v)
    : vals (v), current_val (v.default_value ()) { }

  radio_property (const radio_values& v, const std::string& initial_value)
    : vals (v), current_val (initial_value) { }

  radio_property (const radio_property& a)
    : vals (a.vals), current_val (a.current_val) { }

  radio_property& operator = (const radio_property& a)
  {
    if (&a != this)
      {
	vals = a.vals;
	current_val = a.current_val;
      }

    return *this;
  }

  radio_property& operator = (const std::string& newval)
  {
    if (vals.validate (newval))
      current_val = newval;

    return *this;
  }

  const std::string& current_value (void) const { return current_val; }

private:
  radio_values vals;
  std::string current_val;
};

class
color_values
{
public:
  color_values (double r = 0, double g = 0, double b = 1)
  {
    xrgb[0] = r;
    xrgb[1] = g;
    xrgb[2] = b;

    validate ();
  }

  color_values (std::string str)
  {
    if (! str2rgb (str))
      error ("invalid color specification");
  }

  color_values (const color_values& c)
  {
    xrgb[0] = c.xrgb[0];
    xrgb[1] = c.xrgb[1];
    xrgb[2] = c.xrgb[2];
  }

  color_values& operator = (const color_values& c)
  {
    if (&c != this)
      {
	xrgb[0] = c.xrgb[0];
	xrgb[1] = c.xrgb[1];
	xrgb[2] = c.xrgb[2];

      }

    return *this;
  }

  const double* rgb (void) const { return xrgb; }

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

private:
  double xrgb[3];

  bool str2rgb (std::string str);
};


class 
color_property
{
public:
  color_property (const color_values& c = color_values (),
		  const radio_values& v = radio_values ())
    : current_type (color_t), color_val (c), radio_val (v),
      current_val (v.default_value ())
  { }

  color_property (const radio_values& v)
    : current_type (radio_t), color_val (color_values ()), radio_val (v),
      current_val (v.default_value ())
  { }

  color_property (const radio_values& v, const std::string& initial_value)
    : current_type (radio_t), color_val (color_values ()), radio_val (v),
      current_val (initial_value)
  { }

  color_property (const octave_value& val);

  operator octave_value (void) const
  {
    if (current_type == color_t)
      {
	Matrix retval (1, 3);
	const double *xrgb = color_val.rgb ();

	for (int i = 0; i < 3 ; i++)
	  retval(i) = xrgb[i];

	return retval;
      }

    return current_val;
  }

  color_property& operator = (const color_property& a)
  {
    if (&a != this)
      {
	current_type = a.current_type;
	color_val = a.color_val;
	radio_val = a.radio_val;
	current_val = a.current_val;
      }

    return *this;
  }

  color_property& operator = (const std::string& newval)
  {
    if (radio_val.validate (newval))
      {
	current_val = newval;
	current_type = radio_t;
      }

    return *this;
  }

  color_property& operator = (const color_values& newval)
  {
    color_val = newval;
    current_type = color_t;

    return *this;
  }

  color_property& operator = (const octave_value& newval);

  bool is_rgb (void) const { return (current_type == color_t); }

  bool is_radio (void) const { return (current_type == radio_t); }

  const double* rgb (void) const
  {
    if (current_type != color_t)
      error ("color has no rgb value");

    return color_val.rgb ();
  }

  const std::string& current_value (void) const
  {
    if (current_type != radio_t)
      error ("color has no radio value");

    return current_val;
  }

private:
  enum current_enum { color_t, radio_t } current_type;
  color_values color_val;
  radio_values radio_val;
  std::string current_val;
};

class 
colormap_property
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

  void set (const property_name& name, const octave_value& val);

  octave_value lookup (const property_name& name) const;

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

  Octave_map as_struct (const std::string& prefix_arg) const;

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

  void set (const octave_value_list& args);

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

  graphics_handle get_handle (const std::string& go_name);

  void do_free (const graphics_handle& h);

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


// This function is NOT equivalent to the scripting language function gcf.
graphics_handle gcf (void);

// This function is NOT equivalent to the scripting language function gca.
graphics_handle gca (void);

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

  void set_from_list (base_graphics_object& obj, property_list& defaults);

  virtual void set (const property_name& name, const octave_value& val) = 0;

  graphics_handle get_parent (void) const { return parent; }

  void remove_child (const graphics_handle& h);

  void adopt (const graphics_handle& h)
  {
    octave_idx_type n = children.numel ();
    children.resize (1, n+1);
    children(n) = h;
  }

  void set_parent (const octave_value& val);

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

    void set (const property_name& name, const octave_value& val);

    octave_value get (void) const;

    octave_value get (const property_name& name) const;

    std::string graphics_object_name (void) const { return go_name; }

  private:
    OCTAVE_GRAPHICS_PROPERTY (graphics_handle, currentfigure);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, visible);

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

// ---------------------------------------------------------------------

class figure : public base_graphics_object
{
public:
  class figure_properties : public base_properties
  {
  public:
    figure_properties (const graphics_handle& mh, const graphics_handle& p);

    ~figure_properties (void) { }

    void set (const property_name& name, const octave_value& val);

    octave_value get (void) const;

    octave_value get (const property_name& name) const;

    void close (void);

    std::string graphics_object_name (void) const { return go_name; }

    static property_list::pval_map_type factory_defaults (void);

  private:
    OCTAVE_GRAPHICS_PROPERTY (octave_value, __plot_stream__);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, nextplot);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, closerequestfcn);
    OCTAVE_GRAPHICS_PROPERTY (graphics_handle, currentaxes);
    OCTAVE_GRAPHICS_PROPERTY (colormap_property, colormap);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, visible);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, paperorientation);

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

// ---------------------------------------------------------------------

class axes : public base_graphics_object
{
public:
  class axes_properties : public base_properties
  {
  public:
    axes_properties (const graphics_handle& mh, const graphics_handle& p);

    ~axes_properties (void) { }

    void set (const property_name& name, const octave_value& val);

    void set_defaults (base_graphics_object& obj, const std::string& mode);

    octave_value get (void) const;

    octave_value get (const property_name& name) const;

    void remove_child (const graphics_handle& h);

    void delete_children (void);

    std::string graphics_object_name (void) const { return go_name; }

    static property_list::pval_map_type factory_defaults (void);

  private:
    OCTAVE_GRAPHICS_PROPERTY (octave_value, position);
    OCTAVE_GRAPHICS_MUTABLE_PROPERTY (graphics_handle, title);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, box);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, key);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, keybox);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, keypos);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, dataaspectratio);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, dataaspectratiomode);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, xlim);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, ylim);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, zlim);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, clim);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, xlimmode);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, ylimmode);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, zlimmode);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, climmode);
    OCTAVE_GRAPHICS_MUTABLE_PROPERTY (graphics_handle, xlabel);
    OCTAVE_GRAPHICS_MUTABLE_PROPERTY (graphics_handle, ylabel);
    OCTAVE_GRAPHICS_MUTABLE_PROPERTY (graphics_handle, zlabel);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, xgrid);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, ygrid);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, zgrid);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, xminorgrid);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, yminorgrid);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, zminorgrid);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, xtick);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, ytick);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, ztick);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, xtickmode);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, ytickmode);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, ztickmode);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, xticklabel);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, yticklabel);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, zticklabel);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, xticklabelmode);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, yticklabelmode);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, zticklabelmode);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, xscale);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, yscale);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, zscale);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, xdir);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, ydir);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, zdir);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, xaxislocation);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, yaxislocation);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, view);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, visible);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, nextplot);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, outerposition);

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

// ---------------------------------------------------------------------

class line : public base_graphics_object
{
public:
  class line_properties : public base_properties
  {
  public:
    line_properties (const graphics_handle& mh, const graphics_handle& p);

    ~line_properties (void) { }

    void set (const property_name& name, const octave_value& val);

    octave_value get (void) const;

    octave_value get (const property_name& name) const;

    std::string graphics_object_name (void) const { return go_name; }

    static property_list::pval_map_type factory_defaults (void);

  private:
    OCTAVE_GRAPHICS_PROPERTY (octave_value, xdata);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, ydata);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, zdata);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, ldata);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, udata);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, xldata);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, xudata);
    OCTAVE_GRAPHICS_PROPERTY (color_property, color);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, linestyle);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, linewidth);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, marker);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, markeredgecolor);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, markerfacecolor);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, markersize);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, keylabel);

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

// ---------------------------------------------------------------------

class text : public base_graphics_object
{
public:
  class text_properties : public base_properties
  {
  public:
    text_properties (const graphics_handle& mh, const graphics_handle& p);

    ~text_properties (void) { }

    void set (const property_name& name, const octave_value& val);

    octave_value get (void) const;

    octave_value get (const property_name& name) const;

    std::string graphics_object_name (void) const { return go_name; }

    static property_list::pval_map_type factory_defaults (void);

  private:
    octave_value string;
    octave_value units;
    octave_value position;
    octave_value rotation;
    octave_value horizontalalignment;
    octave_value color;

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

// ---------------------------------------------------------------------

class image : public base_graphics_object
{
public:
  class image_properties : public base_properties
  {
  public:
    image_properties (const graphics_handle& mh, const graphics_handle& p);

    ~image_properties (void) { }

    void set (const property_name& name, const octave_value& val);

    octave_value get (void) const;

    octave_value get (const property_name& name) const;

    std::string graphics_object_name (void) const { return go_name; }

    static property_list::pval_map_type factory_defaults (void);

  private:
    OCTAVE_GRAPHICS_PROPERTY (octave_value, cdata);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, xdata);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, ydata);

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

// ---------------------------------------------------------------------

class patch : public base_graphics_object
{
public:
  class patch_properties : public base_properties
  {
  public:
    patch_properties (const graphics_handle& mh, const graphics_handle& p);

    ~patch_properties (void) { }

    void set (const property_name& name, const octave_value& val);

    octave_value get (void) const;

    octave_value get (const property_name& name) const;

    std::string graphics_object_name (void) const { return go_name; }

    static property_list::pval_map_type factory_defaults (void);

  private:
    OCTAVE_GRAPHICS_PROPERTY (octave_value, cdata);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, xdata);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, ydata);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, zdata);
    OCTAVE_GRAPHICS_PROPERTY (color_property, facecolor);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, facealpha);
    OCTAVE_GRAPHICS_PROPERTY (color_property, edgecolor);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, linestyle);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, linewidth);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, marker);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, markeredgecolor);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, markerfacecolor);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, markersize);

    static std::string go_name;
  };

  patch_properties properties;

public:
  patch (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), properties (mh, p)
  {
    properties.override_defaults (*this);
  }

  ~patch (void) { properties.delete_children (); }

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

// ---------------------------------------------------------------------

class surface : public base_graphics_object
{
public:
  class surface_properties : public base_properties
  {
  public:
    surface_properties (const graphics_handle& mh, const graphics_handle& p);

    ~surface_properties (void) { }

    void set (const property_name& name, const octave_value& val);

    octave_value get (void) const;

    octave_value get (const property_name& name) const;

    std::string graphics_object_name (void) const { return go_name; }

    static property_list::pval_map_type factory_defaults (void);

  private:
    OCTAVE_GRAPHICS_PROPERTY (octave_value, xdata);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, ydata);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, zdata);
    OCTAVE_GRAPHICS_PROPERTY (octave_value, keylabel);

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

octave_value
get_property_from_handle (double handle, const std::string &property,
			  const std::string &func);
bool
set_property_in_handle (double handle, const std::string &property,
			const octave_value &arg, const std::string &func);


#undef OCTAVE_GRAPHICS_PROPERTY_INTERNAL
#undef OCTAVE_GRAPHICS_PROPERTY
#undef OCTAVE_GRAPHICS_MUTABLE_PROPERTY

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
