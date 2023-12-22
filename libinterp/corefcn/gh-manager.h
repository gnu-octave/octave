////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2007-2024 The Octave Project Developers
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

#if ! defined (octave_gh_manager_h)
#define octave_gh_manager_h 1

#include "octave-config.h"

#include "graphics.h"
#include "gtk-manager.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class OCTINTERP_API gh_manager
{
public:

  typedef std::pair<uint8NDArray /*pixels*/, std::string /*svg*/> latex_data;

  OCTINTERP_API gh_manager (octave::interpreter& interp);

  // FIXME: eventually eliminate these static functions and access
  // gh_manager object through the interpreter.

  OCTINTERP_API graphics_handle get_handle (bool integer_figure_handle);

  OCTINTERP_API void free (const graphics_handle& h, bool from_root = false);

  OCTINTERP_API void renumber_figure (const graphics_handle& old_gh,
                                      const graphics_handle& new_gh);

  graphics_handle lookup (double val) const
  {
    const_iterator p = (octave::math::isnan (val)
                        ? m_handle_map.end () : m_handle_map.find (val));

    return (p != m_handle_map.end ()) ? p->first : graphics_handle ();
  }

  graphics_handle lookup (const octave_value& val) const
  {
    return (val.is_real_scalar ()
            ? lookup (val.double_value ()) : graphics_handle ());
  }

  graphics_object get_object (double val) const
  {
    return get_object (lookup (val));
  }

  graphics_object get_object (const graphics_handle& h) const
  {
    const_iterator p = (h.ok () ? m_handle_map.find (h) : m_handle_map.end ());

    return (p != m_handle_map.end ()) ? p->second : graphics_object ();
  }

  OCTINTERP_API graphics_handle
  make_graphics_handle (const std::string& go_name,
                        const graphics_handle& p,
                        bool integer_figure_handle = false,
                        bool call_createfcn = true,
                        bool notify_toolkit = true);

  OCTINTERP_API graphics_handle
  make_figure_handle (double val, bool notify_toolkit = true);

  OCTINTERP_API void push_figure (const graphics_handle& h);

  OCTINTERP_API void pop_figure (const graphics_handle& h);

  graphics_handle current_figure () const
  {
    graphics_handle retval;

    for (const auto& hfig : m_figure_list)
      {
        if (is_handle_visible (hfig))
          retval = hfig;
      }

    return retval;
  }

  Matrix handle_list (bool show_hidden = false)
  {
    Matrix retval (1, m_handle_map.size ());

    octave_idx_type i = 0;
    for (const auto& h_iter : m_handle_map)
      {
        graphics_handle h = h_iter.first;

        if (show_hidden || is_handle_visible (h))
          retval(i++) = h.value ();
      }

    retval.resize (1, i);

    return retval;
  }

  void lock () { m_graphics_lock.lock (); }

  bool try_lock () { return m_graphics_lock.try_lock (); }

  void unlock () { m_graphics_lock.unlock (); }

  Matrix figure_handle_list (bool show_hidden = false)
  {
    Matrix retval (1, m_figure_list.size ());

    octave_idx_type i = 0;
    for (const auto& hfig : m_figure_list)
      {
        if (show_hidden || is_handle_visible (hfig))
          retval(i++) = hfig.value ();
      }

    retval.resize (1, i);

    return retval;
  }

  OCTINTERP_API void
  execute_listener (const graphics_handle& h, const octave_value& l);

  void execute_callback (const graphics_handle& h,
                         const std::string& name,
                         const octave_value& data = Matrix ())
  {
    octave_value cb;

    if (true)
      {
        octave::autolock guard (graphics_lock ());

        graphics_object go = get_object (h);

        if (go.valid_object ())
          cb = go.get (name);
      }

    execute_callback (h, cb, data);
  }

  OCTINTERP_API void
  execute_callback (const graphics_handle& h, const octave_value& cb,
                    const octave_value& data = Matrix ());

  OCTINTERP_API void
  post_callback (const graphics_handle& h, const std::string& name,
                 const octave_value& data = Matrix ());

  OCTINTERP_API void
  post_function (graphics_event::event_fcn fcn, void *fcn_data = nullptr);

  OCTINTERP_API void
  post_set (const graphics_handle& h, const std::string& name,
            const octave_value& value, bool notify_toolkit = true,
            bool redraw_figure = false);

  OCTINTERP_API int process_events (bool force = false);

  OCTINTERP_API void enable_event_processing (bool enable = true);

  bool is_handle_visible (const graphics_handle& h) const
  {
    bool retval = false;

    graphics_object go = get_object (h);

    if (go.valid_object ())
      retval = go.is_handle_visible ();

    return retval;
  }

  OCTINTERP_API void close_all_figures ();

  OCTINTERP_API void restore_gcbo ();

  OCTINTERP_API void post_event (const graphics_event& e);

  octave::mutex graphics_lock ()
  {
    return m_graphics_lock;
  }

  latex_data get_latex_data (const std::string& key) const
  {
    latex_data retval;

    const auto it = m_latex_cache.find (key);

    if (it != m_latex_cache.end ())
      retval = it->second;

    return retval;
  }

  void set_latex_data (const std::string& key, latex_data val)
  {
    // Limit the number of cache entries to 500
    if (m_latex_keys.size () >= 500)
      {
        auto it = m_latex_cache.find (m_latex_keys.front ());

        if (it != m_latex_cache.end ())
          m_latex_cache.erase (it);

        m_latex_keys.pop_front ();
      }

    m_latex_cache[key] = val;
    m_latex_keys.push_back (key);
  }

private:

  typedef std::map<graphics_handle, graphics_object>::iterator iterator;
  typedef std::map<graphics_handle, graphics_object>::const_iterator
    const_iterator;

  typedef std::set<graphics_handle>::iterator free_list_iterator;
  typedef std::set<graphics_handle>::const_iterator const_free_list_iterator;

  typedef std::list<graphics_handle>::iterator figure_list_iterator;
  typedef std::list<graphics_handle>::const_iterator const_figure_list_iterator;

  octave::interpreter& m_interpreter;

  // A map of handles to graphics objects.
  std::map<graphics_handle, graphics_object> m_handle_map;

  // The available graphics handles.
  std::set<graphics_handle> m_handle_free_list;

  // The next handle available if m_handle_free_list is empty.
  double m_next_handle;

  // The allocated figure handles.  Top of the stack is most recently
  // created.
  std::list<graphics_handle> m_figure_list;

  // The lock for accessing the graphics sytsem.
  octave::mutex m_graphics_lock;

  // The list of events queued by graphics toolkits.
  std::list<graphics_event> m_event_queue;

  // The stack of callback objects.
  std::list<graphics_object> m_callback_objects;

  // A flag telling whether event processing must be constantly on.
  int m_event_processing;

  // Cache of already parsed latex strings. Store a separate list of keys
  // to allow for erasing oldest entries if cache size becomes too large.
  std::unordered_map<std::string, latex_data> m_latex_cache;
  std::list<std::string> m_latex_keys;
};

OCTAVE_END_NAMESPACE(octave)

#endif
