////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2023 The Octave Project Developers
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

#include <iostream>

#include "builtin-defun-decls.h"
#include "cmd-edit.h"
#include "cmd-hist.h"
#include "defun.h"
#include "event-manager.h"
#include "interpreter.h"
#include "interpreter-private.h"
#include "oct-env.h"
#include "oct-mutex.h"
#include "ovl.h"
#include "pager.h"
#include "syminfo.h"

#include "quit.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static int readline_event_hook (void)
{
  event_manager& evmgr = __get_event_manager__ ();

  evmgr.process_events ();

  return 0;
}

void interpreter_events::display_exception (const execution_exception& ee,
    bool beep)
{
  if (beep)
    std::cerr << "\a";

  ee.display (std::cerr);
}

event_manager::event_manager (interpreter& interp)
  : m_event_queue_mutex (new mutex ()), m_gui_event_queue (),
    m_debugging (false), m_link_enabled (true),
    m_interpreter (interp), m_instance (new interpreter_events ()),
    m_qt_event_handlers ()
{
  push_event_queue ();
  command_editor::add_event_hook (readline_event_hook);
}

event_manager::~event_manager (void)
{
  delete m_event_queue_mutex;
}

// Programming Note: It is possible to disable the link without deleting
// the connection.  This allows it to be temporarily disabled.  But if
// the link is removed, we also set the link_enabled flag to false
// because if there is no link, it can't be enabled.  Also, access to
// instance is only protected by a check on the link_enabled flag.

void
event_manager::connect_link (const std::shared_ptr<interpreter_events>& obj)
{
  if (! obj)
    disable ();

  m_instance = obj;
}

bool event_manager::enable (void)
{
  bool retval = m_link_enabled;

  if (m_instance)
    m_link_enabled = true;
  else
    warning ("event_manager: must have connected link to enable");

  return retval;
}

void event_manager::process_events (bool disable_flag)
{
  if (enabled ())
    {
      if (disable_flag)
        disable ();

      m_event_queue_mutex->lock ();
      std::shared_ptr<event_queue> evq = m_gui_event_queue.top ();
      m_event_queue_mutex->unlock ();

      evq->run ();
    }
}

void event_manager::discard_events (void)
{
  if (enabled ())
    {
      m_event_queue_mutex->lock ();
      std::shared_ptr<event_queue> evq = m_gui_event_queue.top ();
      m_event_queue_mutex->unlock ();

      evq->discard ();
    }
}

void event_manager::push_event_queue (void)
{
  std::shared_ptr<event_queue> evq (new event_queue ());
  m_gui_event_queue.push (evq);
}

void event_manager::pop_event_queue (void)
{
  // FIXME: Should we worry about the possibility of events remaining
  // in the queue when we pop back to the previous queue?  If so, then
  // we will probably want to push them on to the front of the
  // previous queue so they will be executed before any other events
  // that were in the previous queue.  This case could happen if
  // graphics callback functions were added to the event queue during a
  // debug session just after a dbcont command was added but before it
  // executed and brought us here, for example.

  std::shared_ptr<event_queue> evq = m_gui_event_queue.top ();
  m_gui_event_queue.pop ();
}

void event_manager::post_event (const fcn_callback& fcn)
{
  if (enabled ())
    {
      std::shared_ptr<event_queue> evq = m_gui_event_queue.top ();
      evq->add (fcn);
    }
}

void event_manager::post_event (const meth_callback& meth)
{
  if (enabled ())
    {
      std::shared_ptr<event_queue> evq = m_gui_event_queue.top ();
      evq->add (std::bind (meth, std::ref (m_interpreter)));
    }
}

void event_manager::set_workspace (void)
{
  if (enabled ())
    {
      tree_evaluator& tw = m_interpreter.get_evaluator ();

      m_instance->set_workspace (tw.at_top_level (), m_debugging,
                                 tw.get_symbol_info (), true);
    }
}

void event_manager::set_history (void)
{
  if (enabled ())
    m_instance->set_history (command_history::list ());
}

// FIXME: Should the following function be __event_manager_desktop__
// with the desktop function implemented in a .m file, similar to the
// way the UI* functions work?

DEFMETHOD (desktop, interp, , ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} desktop ()
If running in command-line mode, start the GUI desktop.
@end deftypefn */)
{
  if (interp.experimental_terminal_widget ())
    {
      if (! application::is_gui_running ())
        {
          // FIXME: Currently, the following action is queued and
          // executed in a Qt event loop and we return immediately to
          // the command prompt where additional commands may be
          // executed.  Is that what should happen?  Or should we be
          // waiting until the GUI exits to return to the command
          // prompt, similar to the way the UI* functions work?

          event_manager& evmgr = interp.get_event_manager ();

          evmgr.start_gui ();
        }
      else
        warning ("GUI desktop is already running");
    }
  else
    error ("desktop function requires new experimental terminal widget");

  return ovl ();
}

DEFMETHOD (__event_manager_enabled__, interp, , ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} __event_manager_enabled__ ()
Undocumented internal function.
@end deftypefn */)
{
  event_manager& evmgr = interp.get_event_manager ();

  return ovl (evmgr.enabled ());
}

DEFMETHOD (__event_manager_have_dialogs__, interp, , ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} __event_manager_have_dialogs__ ()
Undocumented internal function.
@end deftypefn */)
{
  event_manager& evmgr = interp.get_event_manager ();

  return ovl (evmgr.have_dialogs ());
}

DEFMETHOD (__event_manager_edit_file__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{status} =} __event_manager_edit_file__ (@var{file})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

  event_manager& evmgr = interp.get_event_manager ();

  if (args.length () == 1)
    {
      std::string file
        = args(0).xstring_value ("first argument must be filename");

      flush_stdout ();

      retval = evmgr.edit_file (file);
    }
  else if (args.length () == 2)
    {
      std::string file
        = args(0).xstring_value ("first argument must be filename");

      flush_stdout ();

      retval = evmgr.prompt_new_edit_file (file);
    }

  return retval;
}

DEFMETHOD (__event_manager_question_dialog__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{btn_val} =} __event_manager_question_dialog__ (@var{msg}, @var{title}, @var{btn1}, @var{btn2}, @var{btn3}, @var{default})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

  if (args.length () == 6)
    {
      std::string msg = args(0).xstring_value ("invalid arguments");
      std::string title = args(1).xstring_value ("invalid arguments");
      std::string btn1 = args(2).xstring_value ("invalid arguments");
      std::string btn2 = args(3).xstring_value ("invalid arguments");
      std::string btn3 = args(4).xstring_value ("invalid arguments");
      std::string btndef = args(5).xstring_value ("invalid arguments");

      flush_stdout ();

      event_manager& evmgr = interp.get_event_manager ();

      retval = evmgr.question_dialog (msg, title, btn1, btn2, btn3, btndef);
    }

  return retval;
}

DEFMETHOD (__event_manager_file_dialog__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {[@var{fname}, @var{fpath}, @var{fltidx}] =} __event_manager_file_dialog__ (@var{filterlist}, @var{title}, @var{filename}, @var{multiselect}, @var{pathname})
Undocumented internal function.
@end deftypefn */)
{
  if (args.length () != 5)
    return ovl ();

  octave_value_list retval (3);

  const Array<std::string> flist = args(0).cellstr_value ();
  std::string title = args(1).string_value ();
  std::string filename = args(2).string_value ();
  std::string multi_on = args(3).string_value (); // on, off, create
  std::string pathname = args(4).string_value ();

  octave_idx_type nel;

  event_manager::filter_list filter_lst;

  for (octave_idx_type i = 0; i < flist.rows (); i++)
    filter_lst.push_back (std::make_pair (flist(i, 0),
                                          (flist.columns () > 1
                                           ? flist(i, 1) : "")));

  flush_stdout ();

  event_manager& evmgr = interp.get_event_manager ();

  std::list<std::string> items_lst
    = evmgr.file_dialog (filter_lst, title, filename, pathname, multi_on);

  nel = items_lst.size ();

  // If 3, then retval is filename, directory, and selected index.
  if (nel <= 3)
    {
      if (items_lst.front ().empty ())
        retval = ovl (octave_value (0.), octave_value (0.), octave_value (0.));
      else
        {
          int idx = 0;
          for (auto& str : items_lst)
            {
              if (idx != 2)
                retval(idx++) = str;
              else
                retval(idx++) = atoi (str.c_str ());
            }
        }
    }
  else
    {
      // Multiple files.
      nel -= 2;
      Cell items (dim_vector (1, nel));

      auto it = items_lst.begin ();

      for (int idx = 0; idx < nel; idx++, it++)
        items.xelem (idx) = *it;

      retval = ovl (items, *it++, atoi (it->c_str ()));
    }

  return retval;
}

DEFMETHOD (__event_manager_list_dialog__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {[@var{sel}, @var{ok}] =} __event_manager_list_dialog__ (@var{list}, @var{mode}, @var{size}, @var{initial}, @var{name}, @var{prompt}, @var{ok_string}, @var{cancel_string})
Undocumented internal function.
@end deftypefn */)
{
  if (args.length () != 8)
    return ovl ();

  Cell list = args(0).cell_value ();
  const Array<std::string> tlist = list.cellstr_value ();
  octave_idx_type nel = tlist.numel ();
  std::list<std::string> list_lst;
  for (octave_idx_type i = 0; i < nel; i++)
    list_lst.push_back (tlist(i));

  std::string mode = args(1).string_value ();

  Matrix size_matrix = args(2).matrix_value ();
  int width = size_matrix(0);
  int height = size_matrix(1);

  Matrix initial_matrix = args(3).matrix_value ();
  nel = initial_matrix.numel ();
  std::list<int> initial_lst;
  for (octave_idx_type i = 0; i < nel; i++)
    initial_lst.push_back (initial_matrix(i));

  std::string name = args(4).string_value ();
  list = args(5).cell_value ();
  const Array<std::string> plist = list.cellstr_value ();
  nel = plist.numel ();
  std::list<std::string> prompt_lst;
  for (octave_idx_type i = 0; i < nel; i++)
    prompt_lst.push_back (plist(i));
  std::string ok_string = args(6).string_value ();
  std::string cancel_string = args(7).string_value ();

  flush_stdout ();

  event_manager& evmgr = interp.get_event_manager ();

  std::pair<std::list<int>, int> result
    = evmgr.list_dialog (list_lst, mode, width, height, initial_lst,
                         name, prompt_lst, ok_string, cancel_string);

  std::list<int> items_lst = result.first;
  nel = items_lst.size ();
  Matrix items (dim_vector (1, nel));
  octave_idx_type i = 0;
  for (const auto& int_el : items_lst)
    items.xelem(i++) = int_el;

  return ovl (items, result.second);
}

DEFMETHOD (__event_manager_input_dialog__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{cstr} =} __event_manager_input_dialog__ (@var{prompt}, @var{title}, @var{rowscols}, @var{defaults})
Undocumented internal function.
@end deftypefn */)
{
  if (args.length () != 4)
    return ovl ();

  Cell prompt = args(0).cell_value ();
  Array<std::string> tmp = prompt.cellstr_value ();
  octave_idx_type nel = tmp.numel ();
  std::list<std::string> prompt_lst;
  for (octave_idx_type i = 0; i < nel; i++)
    prompt_lst.push_back (tmp(i));

  std::string title = args(1).string_value ();

  Matrix rc = args(2).matrix_value ();
  nel = rc.rows ();
  std::list<float> nr;
  std::list<float> nc;
  for (octave_idx_type i = 0; i < nel; i++)
    {
      nr.push_back (rc(i, 0));
      nc.push_back (rc(i, 1));
    }

  Cell defaults = args(3).cell_value ();
  tmp = defaults.cellstr_value ();
  nel = tmp.numel ();
  std::list<std::string> defaults_lst;
  for (octave_idx_type i = 0; i < nel; i++)
    defaults_lst.push_back (tmp(i));

  flush_stdout ();

  event_manager& evmgr = interp.get_event_manager ();

  std::list<std::string> items_lst
    = evmgr.input_dialog (prompt_lst, title, nr, nc, defaults_lst);

  nel = items_lst.size ();
  Cell items (dim_vector (nel, 1));
  octave_idx_type i = 0;
  for (const auto& str_el : items_lst)
    items.xelem(i++) = str_el;

  return ovl (items);
}


DEFMETHOD (__event_manager_named_icon__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{icon} =} __event_manager_dialog_icons__ (@var{icon_name})
Undocumented internal function.
@end deftypefn */)
{
  uint8NDArray retval;

  if (args.length () > 0)
    {
      std::string icon_name = args(0).xstring_value ("invalid arguments");

      event_manager& evmgr = interp.get_event_manager ();

      retval = evmgr.get_named_icon (icon_name);
    }

  return ovl (retval);
}

// FIXME: Why does this function return any value at all?
DEFMETHOD (__event_manager_show_preferences__, interp, , ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{status} =} __event_manager_show_preferences__ ()
Undocumented internal function.
@end deftypefn */)
{
  event_manager& evmgr = interp.get_event_manager ();

  return ovl (evmgr.show_preferences ());
}

DEFMETHOD (__event_manager_apply_preferences__, interp, , ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{status} =} __event_manager_apply_preferences__ ()
Undocumented internal function.
@end deftypefn */)
{
  event_manager& evmgr = interp.get_event_manager ();

  return ovl (evmgr.apply_preferences ());
}

DEFMETHOD (__event_manager_gui_preference__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{prefval} =} __event_manager_gui_preference__ (@var{key})
@deftypefnx {} {@var{prefval} =} __event_manager_gui_preference__ (@var{key}, @var{value})
Undocumented internal function.
@end deftypefn */)
{
  std::string key;
  std::string value = "";

  if (args.length () >= 1)
    key = args(0).string_value();
  else
    error ("__event_manager_gui_preference__: "
           "first argument must be the preference key");

  if (args.length () >= 2)
    value = args(1).string_value();

  if (application::is_gui_running ())
    {
      event_manager& evmgr = interp.get_event_manager ();

      return ovl (evmgr.gui_preference (key, value));
    }
  else
    return ovl (value);
}

DEFMETHOD (__event_manager_file_remove__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __event_manager_file_remove__ ()
Undocumented internal function.
@end deftypefn */)
{
  std::string old_name, new_name;

  if (args.length () == 2)
    {
      old_name = args(0).string_value();
      new_name = args(1).string_value();
    }
  else
    error ("__event_manager_file_remove__: "
           "old and new name expected as arguments");

  event_manager& evmgr = interp.get_event_manager ();

  evmgr.file_remove (old_name, new_name);

  return ovl ();
}

DEFMETHOD (__event_manager_file_renamed__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __event_manager_file_renamed__ ()
Undocumented internal function.
@end deftypefn */)
{
  bool load_new;

  if (args.length () == 1)
    load_new = args(0).bool_value();
  else
    error ("__event_manager_file_renamed__: "
           "first argument must be boolean for reload new named file");

  event_manager& evmgr = interp.get_event_manager ();

  evmgr.file_renamed (load_new);

  return ovl ();
}

DEFMETHOD (openvar, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} openvar (@var{name})
Open the variable @var{name} in the graphical Variable Editor.
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  if (! args(0).is_string ())
    error ("openvar: NAME must be a string");

  std::string name = args(0).string_value ();

  octave_value val = interp.varval (name);

  if (val.is_undefined ())
    error ("openvar: '%s' is not a variable", name.c_str ());

  event_manager& evmgr = interp.get_event_manager ();

  evmgr.edit_variable (name, val);

  return ovl ();
}

/*
%!error openvar ()
%!error openvar ("a", "b")
%!error <NAME must be a string> openvar (1:10)
*/

DEFMETHOD (__event_manager_show_terminal_window__, interp, , ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __event_manager_show_terminal_window__ ()
Undocumented internal function.
@end deftypefn */)
{
  std::string file;

  event_manager& evmgr = interp.get_event_manager ();

  evmgr.show_terminal_window ();

  return ovl ();
}

DEFMETHOD (__event_manager_show_documentation__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{status} =} __event_manager_show_documentation__ (@var{filename})
Undocumented internal function.
@end deftypefn */)
{
  std::string file;

  if (args.length () >= 1)
    file = args(0).string_value();

  event_manager& evmgr = interp.get_event_manager ();

  return ovl (evmgr.show_documentation (file));
}

DEFMETHOD (__event_manager_register_documentation__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{status} =} __event_manager_register_documentation__ (@var{filename})
Undocumented internal function.
@end deftypefn */)
{
  std::string file;

  if (args.length () >= 1)
    file = args(0).string_value();

  event_manager& evmgr = interp.get_event_manager ();

  return ovl (evmgr.register_documentation (file));
}

DEFMETHOD (__event_manager_unregister_documentation__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{status} =} __event_manager_unregister_documentation__ (@var{filename})
Undocumented internal function.
@end deftypefn */)
{
  std::string file;

  if (args.length () >= 1)
    file = args(0).string_value();

  event_manager& evmgr = interp.get_event_manager ();

  return ovl (evmgr.unregister_documentation (file));
}

DEFMETHOD (__event_manager_show_file_browser__, interp, , ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __event_manager_show_file_browser__ ()
Undocumented internal function.
@end deftypefn */)
{
  event_manager& evmgr = interp.get_event_manager ();

  evmgr.show_file_browser ();

  return ovl ();
}

DEFMETHOD (__event_manager_show_command_history__, interp, , ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __event_manager_show_command_history__ ()
Undocumented internal function.
@end deftypefn */)
{
  event_manager& evmgr = interp.get_event_manager ();

  evmgr.show_command_history ();

  return ovl ();
}

DEFMETHOD (__event_manager_show_workspace__, interp, , ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __event_manager_show_workspace__ ()
Undocumented internal function.
@end deftypefn */)
{
  event_manager& evmgr = interp.get_event_manager ();

  evmgr.show_workspace ();

  return ovl ();
}

DEFMETHOD (__event_manager_show_community_news__, interp, , ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __event_manager_show_community_news__ ()
Undocumented internal function.
@end deftypefn */)
{
  event_manager& evmgr = interp.get_event_manager ();

  evmgr.show_community_news ();

  return ovl ();
}

DEFMETHOD (__event_manager_show_release_notes__, interp, , ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __event_manager_show_release_notes__ ()
Undocumented internal function.
@end deftypefn */)
{
  event_manager& evmgr = interp.get_event_manager ();

  evmgr.show_release_notes ();

  return ovl ();
}

DEFMETHOD (__event_manager_gui_status_update__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{status} =} __event_manager_gui_status_update__ (@var{feature}, @var{status})
Internal function for updating the status of some features in the GUI.
@end deftypefn */)
{
  // This is currently a stub and should only be activated some
  // interpreter action only implemented in m-files requires to update
  // a status indicator in the gui. BUT: This internal function can
  // be activated by the user leading to gui indicators not reflecting
  // the real state of the related feature.
  return ovl ();

  std::string feature;
  std::string status;

  if (! (Fisguirunning ())(0).is_true ())
    return ovl ();

  if (args.length () < 2)
    error ("__event_manager_gui_status_update__: two parameters required");
  if (! (args(0).is_string ()))
    error ("__event_manager_gui_status_update__: FEATURE must be a string");
  if (! (args(1).is_string ()))
    error ("__event_manager_gui_status_update__: STATUS must be a string");

  feature = args(0).string_value ();
  status = args(1).string_value ();

  event_manager& evmgr = interp.get_event_manager ();

  return ovl (evmgr.gui_status_update (feature, status));
}

DEFMETHOD (__event_manager_update_gui_lexer__, interp, , ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{status} =} __event_manager_update_gui_lexer__ ()
Undocumented internal function.
@end deftypefn */)
{
  event_manager& evmgr = interp.get_event_manager ();

  return ovl (evmgr.update_gui_lexer ());
}

DEFMETHOD (__event_manager_copy_image_to_clipboard__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __event_manager_copy_image_to_clipboard__ (@var{filename})
Undocumented internal function.
@end deftypefn */)
{
  std::string file;

  if (args.length () >= 1)
    file = args(0).string_value();

  event_manager& evmgr = interp.get_event_manager ();
  evmgr.copy_image_to_clipboard (file);
  return ovl ();
}

DEFMETHOD (commandhistory, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} commandhistory ()
Show the GUI command history window and give it the keyboard focus.
@seealso{commandwindow, filebrowser, workspace}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  event_manager& evmgr = interp.get_event_manager ();
  evmgr.focus_window ("history");
  return ovl ();
}

DEFMETHOD (commandwindow, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} commandwindow ()
Show the GUI command window and give it the keyboard focus.
@seealso{commandhistory, filebrowser, workspace}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  event_manager& evmgr = interp.get_event_manager ();
  evmgr.focus_window ("command");
  return ovl ();
}

DEFMETHOD (filebrowser, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} filebrowser ()
Show the GUI file browser window and give it the keyboard focus.
@seealso{commandwindow, commandhistory, workspace}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  event_manager& evmgr = interp.get_event_manager ();
  evmgr.focus_window ("filebrowser");
  return ovl ();
}

DEFMETHOD (workspace, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} workspace ()
Show the GUI workspace window and give it the keyboard focus.
@seealso{commandwindow, commandhistory, filebrowser}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  event_manager& evmgr = interp.get_event_manager ();
  evmgr.focus_window ("workspace");
  return ovl ();
}

OCTAVE_END_NAMESPACE(octave)
