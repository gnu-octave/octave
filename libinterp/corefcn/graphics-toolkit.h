/*

Copyright (C) 2007-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_graphics_toolkit_h)
#define octave_graphics_toolkit_h 1

#include "octave-config.h"

#include <map>
#include <string>

#include "dMatrix.h"

#include "Cell.h"
#include "error.h"
#include "graphics-handle.h"

class graphics_toolkit;
class graphics_object;

class base_graphics_toolkit
{
public:
  friend class graphics_toolkit;

public:
  base_graphics_toolkit (const std::string& nm)
    : name (nm), count (0) { }

  virtual ~base_graphics_toolkit (void) = default;

  std::string get_name (void) const { return name; }

  virtual bool is_valid (void) const { return false; }

  virtual void redraw_figure (const graphics_object&) const
  { gripe_if_tkit_invalid ("redraw_figure"); }

  virtual void show_figure (const graphics_object&) const
  { gripe_if_tkit_invalid ("show_figure"); }

  virtual void print_figure (const graphics_object&, const std::string&,
                             const std::string&,
                             const std::string& = "") const
  { gripe_if_tkit_invalid ("print_figure"); }

  virtual uint8NDArray get_pixels (const graphics_object&) const
  {
    gripe_if_tkit_invalid ("get_pixels");
    return uint8NDArray ();
  }

  virtual Matrix get_canvas_size (const graphics_handle&) const
  {
    gripe_if_tkit_invalid ("get_canvas_size");
    return Matrix (1, 2, 0.0);
  }

  virtual double get_screen_resolution (void) const
  {
    gripe_if_tkit_invalid ("get_screen_resolution");
    return 72.0;
  }

  virtual Matrix get_screen_size (void) const
  {
    gripe_if_tkit_invalid ("get_screen_size");
    return Matrix (1, 2, 0.0);
  }

  // Callback function executed when the given graphics object
  // changes.  This allows the graphics toolkit to act on property
  // changes if needed.
  virtual void update (const graphics_object&, int)
  { gripe_if_tkit_invalid ("base_graphics_toolkit::update"); }

  void update (const graphics_handle&, int);

  // Callback function executed when the given graphics object is
  // created.  This allows the graphics toolkit to do toolkit-specific
  // initializations for a newly created object.
  virtual bool initialize (const graphics_object&)
  {
    gripe_if_tkit_invalid ("base_graphics_toolkit::initialize");
    return false;
  }

  bool initialize (const graphics_handle&);

  // Callback function executed just prior to deleting the given
  // graphics object.  This allows the graphics toolkit to perform
  // toolkit-specific cleanup operations before an object is deleted.
  virtual void finalize (const graphics_object&)
  { gripe_if_tkit_invalid ("base_graphics_toolkit::finalize"); }

  void finalize (const graphics_handle&);

  // Close the graphics toolkit.
  virtual void close (void)
  { gripe_if_tkit_invalid ("base_graphics_toolkit::close"); }

private:
  std::string name;
  octave::refcount<int> count;

private:
  void gripe_if_tkit_invalid (const std::string& fname) const
  {
    if (! is_valid ())
      error ("%s: invalid graphics toolkit", fname.c_str ());
  }
};

class graphics_toolkit
{
public:
  graphics_toolkit (void)
    : rep (new base_graphics_toolkit ("unknown"))
  {
    rep->count++;
  }

  graphics_toolkit (base_graphics_toolkit *b)
    : rep (b)
  {
    rep->count++;
  }

  graphics_toolkit (const graphics_toolkit& b)
    : rep (b.rep)
  {
    rep->count++;
  }

  ~graphics_toolkit (void)
  {
    if (--rep->count == 0)
      delete rep;
  }

  graphics_toolkit& operator = (const graphics_toolkit& b)
  {
    if (rep != b.rep)
      {
        if (--rep->count == 0)
          delete rep;

        rep = b.rep;
        rep->count++;
      }

    return *this;
  }

  operator bool (void) const { return rep->is_valid (); }

  std::string get_name (void) const { return rep->get_name (); }

  void redraw_figure (const graphics_object& go) const
  { rep->redraw_figure (go); }

  void show_figure (const graphics_object& go) const
  { rep->show_figure (go); }

  void print_figure (const graphics_object& go, const std::string& term,
                     const std::string& file,
                     const std::string& debug_file = "") const
  { rep->print_figure (go, term, file, debug_file); }

  uint8NDArray get_pixels (const graphics_object& go) const
  { return rep->get_pixels (go); }

  Matrix get_canvas_size (const graphics_handle& fh) const
  { return rep->get_canvas_size (fh); }

  double get_screen_resolution (void) const
  { return rep->get_screen_resolution (); }

  Matrix get_screen_size (void) const
  { return rep->get_screen_size (); }

  // Notifies graphics toolkit that object't property has changed.
  void update (const graphics_object& go, int id)
  { rep->update (go, id); }

  void update (const graphics_handle& h, int id)
  { rep->update (h, id); }

  // Notifies graphics toolkit that new object was created.
  bool initialize (const graphics_object& go)
  { return rep->initialize (go); }

  bool initialize (const graphics_handle& h)
  { return rep->initialize (h); }

  // Notifies graphics toolkit that object was destroyed.
  // This is called only for explicitly deleted object.
  // Children are deleted implicitly and graphics toolkit isn't notified.
  void finalize (const graphics_object& go)
  { rep->finalize (go); }

  void finalize (const graphics_handle& h)
  { rep->finalize (h); }

  // Close the graphics toolkit.
  void close (void) { rep->close (); }

private:

  base_graphics_toolkit *rep;
};

#endif
