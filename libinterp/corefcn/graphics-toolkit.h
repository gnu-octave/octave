////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2007-2023 The Octave Project Developers
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

#if ! defined (octave_graphics_toolkit_h)
#define octave_graphics_toolkit_h 1

#include "octave-config.h"

#include <map>
#include <memory>
#include <string>

#include "dMatrix.h"

#include "Cell.h"
#include "error.h"
#include "graphics-handle.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class graphics_object;
class graphics_toolkit;

class base_graphics_toolkit
{
public:

  friend class graphics_toolkit;

public:

  base_graphics_toolkit (const std::string& nm)
    : m_name (nm)
  { }

  virtual ~base_graphics_toolkit (void) = default;

  std::string get_name (void) const
  {
    return m_name;
  }

  virtual bool is_valid (void) const
  {
    return false;
  }

  virtual void redraw_figure (const graphics_object&) const
  {
    gripe_if_tkit_invalid ("redraw_figure");
  }

  virtual void show_figure (const graphics_object&) const
  {
    gripe_if_tkit_invalid ("show_figure");
  }

  virtual void print_figure (const graphics_object&, const std::string&,
                             const std::string&,
                             const std::string& = "") const
  {
    gripe_if_tkit_invalid ("print_figure");
  }

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

  virtual Matrix get_text_extent (const graphics_object&) const
  {
    gripe_if_tkit_invalid ("get_text_extent");
    return Matrix ();
  }

  // Callback function executed when the given graphics object
  // changes.  This allows the graphics toolkit to act on property
  // changes if needed.
  virtual void update (const graphics_object&, int)
  {
    gripe_if_tkit_invalid ("base_graphics_toolkit::update");
  }

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
  {
    gripe_if_tkit_invalid ("base_graphics_toolkit::finalize");
  }

  void finalize (const graphics_handle&);

  // Close the graphics toolkit.
  virtual void close (void)
  {
    gripe_if_tkit_invalid ("base_graphics_toolkit::close");
  }

private:

  std::string m_name;

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
  graphics_toolkit (const std::string& name = "unknown")
    : m_rep (new base_graphics_toolkit (name))
  { }

  // NEW_REP must be dynamically allocated.
  graphics_toolkit (base_graphics_toolkit *new_rep)
    : m_rep (std::shared_ptr<base_graphics_toolkit> (new_rep))
  {
    if (! m_rep)
      error ("invalid graphics_toolkit!");
  }

  graphics_toolkit (const graphics_toolkit& b) = default;

  graphics_toolkit& operator = (const graphics_toolkit& b) = default;

  ~graphics_toolkit (void) = default;

  operator bool (void) const
  {
    return m_rep->is_valid ();
  }

  std::string get_name (void) const
  {
    return m_rep->get_name ();
  }

  void redraw_figure (const graphics_object& go) const
  {
    m_rep->redraw_figure (go);
  }

  void show_figure (const graphics_object& go) const
  {
    m_rep->show_figure (go);
  }

  void print_figure (const graphics_object& go, const std::string& term,
                     const std::string& file,
                     const std::string& debug_file = "") const
  {
    m_rep->print_figure (go, term, file, debug_file);
  }

  uint8NDArray get_pixels (const graphics_object& go) const
  {
    return m_rep->get_pixels (go);
  }

  Matrix get_canvas_size (const graphics_handle& fh) const
  {
    return m_rep->get_canvas_size (fh);
  }

  double get_screen_resolution (void) const
  {
    return m_rep->get_screen_resolution ();
  }

  Matrix get_screen_size (void) const
  {
    return m_rep->get_screen_size ();
  }

  Matrix get_text_extent (const graphics_object& go) const
  {
    return m_rep->get_text_extent (go);
  }

  // Notifies graphics toolkit that object't property has changed.
  void update (const graphics_object& go, int id)
  {
    m_rep->update (go, id);
  }

  void update (const graphics_handle& h, int id)
  {
    m_rep->update (h, id);
  }

  // Notifies graphics toolkit that new object was created.
  bool initialize (const graphics_object& go)
  {
    return m_rep->initialize (go);
  }

  bool initialize (const graphics_handle& h)
  {
    return m_rep->initialize (h);
  }

  // Notifies graphics toolkit that object was destroyed.
  // This is called only for explicitly deleted object.
  // Children are deleted implicitly and graphics toolkit isn't notified.
  void finalize (const graphics_object& go)
  {
    m_rep->finalize (go);
  }

  void finalize (const graphics_handle& h)
  {
    m_rep->finalize (h);
  }

  // Close the graphics toolkit.
  void close (void)
  {
    m_rep->close ();
  }

private:

  std::shared_ptr<base_graphics_toolkit> m_rep;
};

OCTAVE_END_NAMESPACE(octave)

#endif
