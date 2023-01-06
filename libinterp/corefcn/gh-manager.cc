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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "cmd-edit.h"

#include "builtin-defun-decls.h"
#include "gh-manager.h"
#include "graphics-utils.h"
#include "input.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "parse.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static double
make_handle_fraction (void)
{
  static double maxrand = RAND_MAX + 2.0;

  return (rand () + 1.0) / maxrand;
}

graphics_handle
gh_manager::get_handle (bool integer_figure_handle)
{
  graphics_handle retval;

  if (integer_figure_handle)
    {
      // Figure handles are positive integers corresponding
      // to the figure number.

      // We always want the lowest unused figure number.

      retval = 1;

      while (m_handle_map.find (retval) != m_handle_map.end ())
        retval++;
    }
  else
    {
      // Other graphics handles are negative integers plus some random
      // fractional part.  To avoid running out of integers, we recycle the
      // integer part but tack on a new random part each time.

      auto p = m_handle_free_list.begin ();

      if (p != m_handle_free_list.end ())
        {
          retval = *p;
          m_handle_free_list.erase (p);
        }
      else
        {
          retval = graphics_handle (m_next_handle);

          m_next_handle = std::ceil (m_next_handle) - 1.0 - make_handle_fraction ();
        }
    }

  return retval;
}

void
gh_manager::free (const graphics_handle& h, bool from_root)
{
  if (h.ok ())
    {
      if (h.value () == 0)
        error ("graphics_handle::free: can't delete root object");

      auto p = m_handle_map.find (h);

      if (p == m_handle_map.end ())
        error ("graphics_handle::free: invalid object %g", h.value ());

      base_properties& bp = p->second.get_properties ();

      if (! p->second.valid_object () || bp.is_beingdeleted ())
        return;

      graphics_handle parent_h = p->second.get_parent ();
      graphics_object parent_go = nullptr;
      if (! from_root || isfigure (h.value ()))
        parent_go = get_object (parent_h);

      bp.set_beingdeleted (true);

      // delete listeners before invalidating object
      p->second.remove_all_listeners ();

      bp.delete_children (true, from_root);

      // NOTE: Call the delete function while the object's state is still valid.
      octave_value val = bp.get_deletefcn ();

      bp.execute_deletefcn ();

      // Notify graphics toolkit.
      p->second.finalize ();


      // NOTE: Call remove_child before erasing the go from the map if not
      // removing from groot.
      // A callback function might have already deleted the parent
      if ((! from_root || isfigure (h.value ())) && parent_go.valid_object ()
          && h.ok ())
        parent_go.remove_child (h);

      // Note: this will be valid only for first explicitly deleted
      // object.  All its children will then have an
      // unknown graphics toolkit.

      // Graphics handles for non-figure objects are negative
      // integers plus some random fractional part.  To avoid
      // running out of integers, we recycle the integer part
      // but tack on a new random part each time.

      m_handle_map.erase (p);

      if (h.value () < 0)
        m_handle_free_list.insert
        (std::ceil (h.value ()) - make_handle_fraction ());
    }
}

void
gh_manager::renumber_figure (const graphics_handle& old_gh,
                             const graphics_handle& new_gh)
{
  auto p = m_handle_map.find (old_gh);

  if (p == m_handle_map.end ())
    error ("graphics_handle::free: invalid object %g", old_gh.value ());

  graphics_object go = p->second;

  m_handle_map.erase (p);

  m_handle_map[new_gh] = go;

  if (old_gh.value () < 0)
    m_handle_free_list.insert (std::ceil (old_gh.value ())
                               - make_handle_fraction ());

  for (auto& hfig : m_figure_list)
    {
      if (hfig == old_gh)
        {
          hfig = new_gh;
          break;
        }
    }
}

void
gh_manager::close_all_figures (void)
{
  // FIXME: should we process or discard pending events?

  m_event_queue.clear ();

  // Don't use m_figure_list_iterator because we'll be removing elements
  // from the list elsewhere.

  Matrix hlist = figure_handle_list (true);

  for (octave_idx_type i = 0; i < hlist.numel (); i++)
    {
      graphics_handle h = lookup (hlist(i));

      if (h.ok ())
        close_figure (h);
    }

  // They should all be closed now.  If not, force them to close.

  hlist = figure_handle_list (true);

  for (octave_idx_type i = 0; i < hlist.numel (); i++)
    {
      graphics_handle h = lookup (hlist(i));

      if (h.ok ())
        force_close_figure (h);
    }

  // None left now, right?

  hlist = figure_handle_list (true);

  if (hlist.numel () != 0)
    warning ("gh_manager::close_all_figures: some graphics elements failed to close");

  // Clear all callback objects from our list.

  m_callback_objects.clear ();
}

// We use a random value for the handle to avoid issues with plots and
// scalar values for the first argument.
gh_manager::gh_manager (octave::interpreter& interp)
  : m_interpreter (interp), m_handle_map (), m_handle_free_list (),
    m_next_handle (-1.0 - (rand () + 1.0) / (RAND_MAX + 2.0)),
    m_figure_list (), m_graphics_lock (),  m_event_queue (),
    m_callback_objects (), m_event_processing (0)
{
  m_handle_map[0] = graphics_object (new root_figure ());

  octave::gtk_manager& gtk_mgr = octave::__get_gtk_manager__ ();

  // Make sure the default graphics toolkit is registered.
  gtk_mgr.default_toolkit ();
}

graphics_handle
gh_manager::make_graphics_handle (const std::string& go_name,
                                  const graphics_handle& p,
                                  bool integer_figure_handle,
                                  bool call_createfcn, bool notify_toolkit)
{
  graphics_handle h = get_handle (integer_figure_handle);

  base_graphics_object *bgo = make_graphics_object_from_type (go_name, h, p);

  if (! bgo)
    error ("gh_manager::make_graphics_handle: invalid object type '%s'",
           go_name.c_str ());

  graphics_object go (bgo);

  m_handle_map[h] = go;

  if (go_name == "axes")
    {
      // Handle defaults for labels since overriding defaults for
      // them can't work before the axes object is fully
      // constructed.

      axes::properties& props
        = dynamic_cast<axes::properties&> (go.get_properties ());

      graphics_object tgo;

      tgo = get_object (props.get_xlabel ());
      tgo.override_defaults ();

      tgo = get_object (props.get_ylabel ());
      tgo.override_defaults ();

      tgo = get_object (props.get_zlabel ());
      tgo.override_defaults ();

      tgo = get_object (props.get_title ());
      tgo.override_defaults ();
    }

  // Overriding defaults will work now because the handle is valid
  // and we can find parent objects (not just handles).
  go.override_defaults ();

  if (call_createfcn)
    bgo->get_properties ().execute_createfcn ();

  // Notify graphics toolkit.
  if (notify_toolkit)
    go.initialize ();

  return h;
}

graphics_handle
gh_manager::make_figure_handle (double val, bool notify_toolkit)
{
  graphics_handle h = val;

  base_graphics_object *bgo = new figure (h, 0);
  graphics_object go (bgo);

  m_handle_map[h] = go;

  // Notify graphics toolkit.
  if (notify_toolkit)
    go.initialize ();

  go.override_defaults ();

  return h;
}

void
gh_manager::push_figure (const graphics_handle& h)
{
  pop_figure (h);

  m_figure_list.push_front (h);
}

void
gh_manager::pop_figure (const graphics_handle& h)
{
  for (auto it = m_figure_list.begin (); it != m_figure_list.end (); it++)
    {
      if (*it == h)
        {
          m_figure_list.erase (it);
          break;
        }
    }
}

static void
xset_gcbo (const graphics_handle& h)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (0);

  root_figure::properties& props
    = dynamic_cast<root_figure::properties&> (go.get_properties ());

  props.set_callbackobject (h.as_octave_value ());
}

void
gh_manager::restore_gcbo (void)
{
  octave::autolock guard (m_graphics_lock);

  m_callback_objects.pop_front ();

  xset_gcbo (m_callback_objects.empty ()
             ? graphics_handle () : m_callback_objects.front ().get_handle ());
}

void
gh_manager::execute_listener (const graphics_handle& h, const octave_value& l)
{
  if (octave::thread::is_thread ())
    execute_callback (h, l, octave_value ());
  else
    {
      octave::autolock guard (m_graphics_lock);

      post_event (graphics_event::create_callback_event (h, l));
    }
}

void
gh_manager::execute_callback (const graphics_handle& h,
                              const octave_value& cb_arg,
                              const octave_value& data)
{
  if (cb_arg.is_defined () && ! cb_arg.isempty ())
    {
      octave_value_list args;
      octave_value ov_fcn;
      octave_function *fcn = nullptr;

      args(0) = h.as_octave_value ();
      if (data.is_defined ())
        args(1) = data;
      else
        args(1) = Matrix ();

      octave::unwind_action_safe restore_gcbo_action
      (&gh_manager::restore_gcbo, this);

      graphics_object go (get_object (h));
      if (go)
        {
          // FIXME: Is the lock necessary when we're only calling a
          //        const "get" method?
          octave::autolock guard (m_graphics_lock);
          m_callback_objects.push_front (go);
          xset_gcbo (h);
        }

      // Copy CB because "function_value" method is non-const.
      octave_value cb = cb_arg;

      if (cb.is_function ())
        fcn = cb.function_value ();
      else if (cb.is_function_handle ())
        ov_fcn = cb;
      else if (cb.is_string ())
        {
          int status;
          std::string s = cb.string_value ();

          try
            {
              m_interpreter.eval_string (s, false, status, 0);
            }
          catch (const octave::execution_exception& ee)
            {
              m_interpreter.handle_exception (ee);
            }
        }
      else if (cb.iscell () && cb.length () > 0
               && (cb.rows () == 1 || cb.columns () == 1)
               && (cb.cell_value ()(0).is_function ()
                   || cb.cell_value ()(0).is_function_handle ()))
        {
          Cell c = cb.cell_value ();

          ov_fcn = c(0);

          for (int i = 1; i < c.numel () ; i++)
            args(1+i) = c(i);
        }
      else
        {
          std::string nm = cb.class_name ();
          error ("trying to execute non-executable object (class = %s)",
                 nm.c_str ());
        }

      if (fcn || ov_fcn.is_defined ())
        try
          {
            if (ov_fcn.is_defined ())
              octave::feval (ov_fcn, args);
            else
              octave::feval (fcn, args);
          }
        catch (const octave::execution_exception& ee)
          {
            m_interpreter.handle_exception (ee);
          }

      // Redraw after interacting with a user-interface (ui*) object.
      if (Vdrawnow_requested)
        {
          if (go)
            {
              std::string go_name
                = go.get_properties ().graphics_object_name ();

              if (go_name.length () > 1
                  && go_name[0] == 'u' && go_name[1] == 'i')
                {
                  Fdrawnow (m_interpreter);
                  Vdrawnow_requested = false;
                }
            }
        }
    }
}

static int
process_graphics_events (void)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  return gh_mgr.process_events ();
}

void
gh_manager::post_event (const graphics_event& e)
{
  m_event_queue.push_back (e);

  octave::command_editor::add_event_hook (process_graphics_events);
}

void
gh_manager::post_callback (const graphics_handle& h, const std::string& name,
                           const octave_value& data)
{
  octave::autolock guard (m_graphics_lock);

  graphics_object go = get_object (h);

  if (go.valid_object ())
    {
      caseless_str cname (name);
      int busyaction = base_graphics_event::QUEUE;

      if (cname == "deletefcn" || cname == "createfcn"
          || cname == "closerequestfcn"
          || ((go.isa ("figure") || go.isa ("uipanel")
               || go.isa ("uibuttongroup"))
              && (cname == "resizefcn" || cname == "sizechangedfcn")))
        busyaction = base_graphics_event::INTERRUPT;
      else if (go.get_properties ().get_busyaction () == "cancel")
        busyaction = base_graphics_event::CANCEL;

      // The "closerequestfcn" callback must be executed once the figure has
      // been made current.  Let "close" do the job.
      if (cname == "closerequestfcn")
        {
          std::string cmd ("close (gcbf ());");
          post_event (graphics_event::create_mcode_event (h, cmd, busyaction));
        }
      else
        post_event (graphics_event::create_callback_event (h, name, data,
                    busyaction));
    }
}

void
gh_manager::post_function (graphics_event::event_fcn fcn, void *fcn_data)
{
  octave::autolock guard (m_graphics_lock);

  post_event (graphics_event::create_function_event (fcn, fcn_data));
}

void
gh_manager::post_set (const graphics_handle& h, const std::string& name,
                      const octave_value& value, bool notify_toolkit,
                      bool redraw_figure)
{
  octave::autolock guard (m_graphics_lock);

  post_event (graphics_event::create_set_event (h, name, value, notify_toolkit,
              redraw_figure));
}

int
gh_manager::process_events (bool force)
{
  graphics_event e;
  bool old_Vdrawnow_requested = Vdrawnow_requested;
  bool events_executed = false;

  do
    {
      e = graphics_event ();

      {
        octave::autolock guard (m_graphics_lock);

        if (! m_event_queue.empty ())
          {
            if (m_callback_objects.empty () || force)
              {
                e = m_event_queue.front ();

                m_event_queue.pop_front ();
              }
            else
              {
                const graphics_object& go = m_callback_objects.front ();

                if (go.get_properties ().is_interruptible ())
                  {
                    e = m_event_queue.front ();

                    m_event_queue.pop_front ();
                  }
                else
                  {
                    std::list<graphics_event>::iterator p = m_event_queue.begin ();

                    while (p != m_event_queue.end ())
                      if (p->get_busyaction () == base_graphics_event::CANCEL)
                        {
                          p = m_event_queue.erase (p);
                        }
                      else if (p->get_busyaction ()
                               == base_graphics_event::INTERRUPT)
                        {
                          e = (*p);
                          m_event_queue.erase (p);
                          break;
                        }
                      else
                        p++;
                  }
              }
          }
      }

      if (e.ok ())
        {
          e.execute ();
          events_executed = true;
        }
    }
  while (e.ok ());

  {
    octave::autolock guard (m_graphics_lock);

    if (m_event_queue.empty () && m_event_processing == 0)
      octave::command_editor::remove_event_hook (process_graphics_events);
  }

  if (events_executed)
    octave::flush_stdout ();

  if (Vdrawnow_requested && ! old_Vdrawnow_requested)
    {
      Fdrawnow (m_interpreter);

      Vdrawnow_requested = false;
    }

  return 0;
}


/*
## Test interruptible/busyaction properties
%!function cb (h, ~)
%! setappdata (gcbf (), "cb_exec", [getappdata(gcbf (), "cb_exec") h]);
%! drawnow ();
%! setappdata (gcbf (), "cb_exec", [getappdata(gcbf (), "cb_exec") h]);
%!endfunction
%!
%!testif HAVE_OPENGL, HAVE_QT; have_window_system () && any (strcmp ("qt", available_graphics_toolkits ()))
%! hf = figure ("visible", "off", "resizefcn", @cb);
%! graphics_toolkit (hf, "qt");
%! unwind_protect
%!   ## Default
%!   hui1 = uicontrol ("parent", hf, "interruptible", "on", "callback", @cb);
%!   hui2 = uicontrol ("parent", hf, "busyaction", "queue", "callback", @cb);
%!   hui3 = uicontrol ("parent", hf, "busyaction", "queue", "callback", @cb);
%!   __go_post_callback__ (hui1, "callback");
%!   __go_post_callback__ (hui2, "callback");
%!   __go_post_callback__ (hui3, "callback");
%!
%!   assert (getappdata (hf, "cb_exec"), []);
%!   drawnow ();
%!   assert (getappdata (hf, "cb_exec"), [hui1 hui2 hui3 hui3 hui2 hui1]);
%!
%!   ## Interruptible off
%!   setappdata (hf, "cb_exec", []);
%!   set (hui1, "interruptible", "off");
%!   __go_post_callback__ (hui1, "callback");
%!   __go_post_callback__ (hui2, "callback");
%!   __go_post_callback__ (hui3, "callback");
%!   drawnow ();
%!   assert (getappdata (hf, "cb_exec"), [hui1 hui1 hui2 hui3 hui3 hui2]);
%!
%!   ## "resizefcn" callback interrupts regardless of interruptible property
%!   setappdata (hf, "cb_exec", []);
%!   __go_post_callback__ (hui1, "callback");
%!   __go_post_callback__ (hf, "resizefcn");
%!   drawnow ();
%!   assert (getappdata (hf, "cb_exec"), [hui1 hf hf hui1]);
%!
%!   ## test "busyaction" "cancel"
%!   setappdata (hf, "cb_exec", []);
%!   set (hui2, "busyaction", "cancel");
%!   __go_post_callback__ (hui1, "callback");
%!   __go_post_callback__ (hui2, "callback");
%!   __go_post_callback__ (hui3, "callback");
%!   __go_post_callback__ (hf, "resizefcn");
%!   drawnow ();
%!   assert (getappdata (hf, "cb_exec"), [hui1 hf hui3 hui3 hf hui1]);
%! unwind_protect_cleanup
%!   close (hf)
%! end_unwind_protect
*/

void
gh_manager::enable_event_processing (bool enable)
{
  octave::autolock guard (m_graphics_lock);

  if (enable)
    {
      m_event_processing++;

      octave::command_editor::add_event_hook (process_graphics_events);
    }
  else
    {
      m_event_processing--;

      if (m_event_queue.empty () && m_event_processing == 0)
        octave::command_editor::remove_event_hook (process_graphics_events);
    }
}

OCTAVE_END_NAMESPACE(octave)
