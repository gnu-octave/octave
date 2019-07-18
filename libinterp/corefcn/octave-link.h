/*

Copyright (C) 2013-2019 John W. Eaton
Copyright (C) 2011-2019 Jacob Dawid
Copyright (C) 2011-2019 John P. Swensen

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

#if ! defined (octave_octave_link_h)
#define octave_octave_link_h 1

#include "octave-config.h"

#include <list>
#include <string>

#include "oct-mutex.h"
#include "octave.h"
#include "event-queue.h"
#include "uint8NDArray.h"

class octave_value;
class string_vector;

namespace octave
{
  class symbol_info_list;
}

class
octave_link_events
{
public:

  octave_link_events (void) = default;

  octave_link_events (const octave_link_events&) = default;

  octave_link_events& operator = (const octave_link_events&) = default;

  virtual ~octave_link_events (void) = default;

  virtual bool do_confirm_shutdown (void) { return false; }

  virtual bool do_copy_image_to_clipboard (const std::string& /*file*/)
  {
    return false;
  }

  virtual bool do_edit_file (const std::string& /*file*/) { return false; }

  virtual bool do_prompt_new_edit_file (const std::string& /*file*/)
  {
    return false;
  }

  virtual std::string
  do_question_dialog (const std::string& /*msg*/,
                      const std::string& /*title*/,
                      const std::string& /*btn1*/,
                      const std::string& /*btn2*/,
                      const std::string& /*btn3*/,
                      const std::string& /*btndef*/)
  {
    return "";
  }

  virtual std::pair<std::list<int>, int>
  do_list_dialog (const std::list<std::string>& /*list*/,
                  const std::string& /*mode*/,
                  int /*width*/, int /*height*/,
                  const std::list<int>& /*initial_value*/,
                  const std::string& /*name*/,
                  const std::list<std::string>& /*prompt*/,
                  const std::string& /*ok_string*/,
                  const std::string& /*cancel_string*/)
  {
    return std::pair<std::list<int>, int> ();
  }

  virtual std::list<std::string>
  do_input_dialog (const std::list<std::string>& /*prompt*/,
                   const std::string& /*title*/,
                   const std::list<float>& /*nr*/,
                   const std::list<float>& /*nc*/,
                   const std::list<std::string>& /*defaults*/)
  {
    return std::list<std::string> ();
  }

  typedef std::list<std::pair<std::string, std::string>> filter_list;

  virtual std::list<std::string>
  do_file_dialog (const filter_list& /*filter*/,
                  const std::string& /*title*/,
                  const std::string& /*filename*/,
                  const std::string& /*dirname*/,
                  const std::string& /*multimode*/)
  {
    return std::list<std::string> ();
  }

  virtual int
  do_debug_cd_or_addpath_error (const std::string& /*file*/,
                                const std::string& /*dir*/,
                                bool /*addpath_option*/)
  {
    return -1;
  }

  virtual void do_change_directory (const std::string& /*dir*/) { }

  virtual void do_file_remove (const std::string& /*old_name*/,
                               const std::string& /*new_name*/)
  { }

  virtual void do_file_renamed (bool) { }

  virtual void
  do_execute_command_in_terminal (const std::string& /*command*/) { }

  virtual uint8NDArray do_get_named_icon (const std::string& /*icon_name*/)
  {
    return uint8NDArray ();
  }

  virtual void do_set_workspace (bool /*top_level*/, bool /*debug*/,
                                 const octave::symbol_info_list& /*syminfo*/,
                                 bool /*update_variable_editor*/)
  { }

  virtual void do_clear_workspace (void) { }

  virtual void do_set_history (const string_vector& /*hist*/) { }

  virtual void do_append_history (const std::string& /*hist_entry*/) { }

  virtual void do_clear_history (void) { }

  virtual void do_pre_input_event (void) { }

  virtual void do_post_input_event (void) { }

  virtual void
  do_enter_debugger_event (const std::string& /*file*/, int /*line*/) { }

  virtual void
  do_execute_in_debugger_event (const std::string& /*file*/, int /*line*/) { }

  virtual void do_exit_debugger_event (void) { }

  virtual void do_update_breakpoint (bool /*insert*/,
                                     const std::string& /*file*/,
                                     int /*line*/, const std::string& /*cond*/)
  { }

  virtual void do_show_preferences (void) { }

  virtual std::string do_gui_preference (const std::string& /*key*/,
                                         const std::string& /*value*/)
  {
    return "";
  }

  virtual void do_show_doc (const std::string& /*file*/) { }

  virtual void do_register_doc (const std::string& /*file*/) { }

  virtual void do_unregister_doc (const std::string& /*file*/) { }

  virtual void do_edit_variable (const std::string& /*name*/,
                                 const octave_value& /*val*/)
  { }
};

//! Provides threadsafe access to octave.
//! @author Jacob Dawid
//!
//! This class is a wrapper around octave and provides thread safety by
//! buffering access operations to octave and executing them in the
//! readline event hook, which lives in the octave thread.

class
OCTINTERP_API
octave_link
{
protected:

  octave_link (void);

public:

  // No copying!

  octave_link (const octave_link&) = delete;

  octave_link& operator = (const octave_link&) = delete;

  virtual ~octave_link (void);

  static void connect_link (octave_link_events *obj);

  static octave_link_events * disconnect_link (bool delete_instance = true);

  static bool enable (void)
  {
    bool retval = link_enabled;
    link_enabled = true;
    return retval;
  }

  static bool disable (void)
  {
    bool retval = link_enabled;
    link_enabled = false;
    return retval;
  }

  static bool enabled (void)
  {
    return link_enabled;
  }

  // If disable is TRUE, then no additional events will be processed
  // other than exit.

  static void process_events (bool disable_arg = false);

  static void discard_events (void);

  static bool confirm_shutdown (void)
  {
    bool retval = true;

    if (enabled ())
      retval = instance->do_confirm_shutdown ();

    return retval;
  }

  template <typename F, typename... Args>
  static void
  post_event (F&& fcn, Args&&... args)
  {
    if (enabled ())
      gui_event_queue.add (fcn, std::forward<Args> (args)...);
  }

  template <typename T, typename... Params, typename... Args>
  static void
  post_event (T *obj, void (T::*method) (Params...), Args&&... args)
  {
    if (enabled ())
      gui_event_queue.add_method (obj, method, std::forward<Args> (args)...);
  }

  static void
  post_exception (const std::exception_ptr &p)
  {
    if (enabled ())
      post_event (&octave_link::rethrow_exception_callback, p);
  }

  static bool
  copy_image_to_clipboard (const std::string& file)
  {
    return enabled () ? instance->do_copy_image_to_clipboard (file) : false;
  }

  static bool
  edit_file (const std::string& file)
  {
    return enabled () ? instance->do_edit_file (file) : false;
  }

  static bool
  prompt_new_edit_file (const std::string& file)
  {
    return enabled () ? instance->do_prompt_new_edit_file (file) : false;
  }

  static std::string
  question_dialog (const std::string& msg, const std::string& title,
                   const std::string& btn1, const std::string& btn2,
                   const std::string& btn3, const std::string& btndef)
  {
    return enabled () ? instance->do_question_dialog (msg, title, btn1,
                                                      btn2, btn3, btndef) : "";
  }

  static std::pair<std::list<int>, int>
  list_dialog (const std::list<std::string>& list,
               const std::string& mode,
               int width, int height,
               const std::list<int>& initial_value,
               const std::string& name,
               const std::list<std::string>& prompt,
               const std::string& ok_string,
               const std::string& cancel_string)
  {
    return enabled ()
           ? instance->do_list_dialog (list, mode, width, height,
                                       initial_value, name, prompt,
                                       ok_string, cancel_string)
           : std::pair<std::list<int>, int> ();
  }

  static std::list<std::string>
  input_dialog (const std::list<std::string>& prompt,
                const std::string& title,
                const std::list<float>& nr,
                const std::list<float>& nc,
                const std::list<std::string>& defaults)
  {
    return enabled ()
           ? instance->do_input_dialog (prompt, title, nr, nc, defaults)
           : std::list<std::string> ();
  }

  typedef std::list<std::pair<std::string, std::string>> filter_list;

  static std::list<std::string>
  file_dialog (const filter_list& filter, const std::string& title,
               const std::string& filename, const std::string& dirname,
               const std::string& multimode)
  {
    return enabled ()
           ? instance->do_file_dialog (filter, title, filename, dirname,
                                       multimode)
           : std::list<std::string> ();
  }

  static int debug_cd_or_addpath_error (const std::string& file,
                                        const std::string& dir,
                                        bool addpath_option)
  {
    return enabled ()
           ? instance->do_debug_cd_or_addpath_error (file, dir, addpath_option)
           : 0;
  }

  static void change_directory (const std::string& dir)
  {
    if (enabled ())
      instance->do_change_directory (dir);
  }

  // Methods for removing/renaming files which might be open in editor
  static void file_remove (const std::string& old_name,
                           const std::string& new_name)
  {
    if (octave::application::is_gui_running () && enabled ())
      instance->do_file_remove (old_name, new_name);
  }

  static void file_renamed (bool load_new)
  {
    if (octave::application::is_gui_running () && enabled ())
      instance->do_file_renamed (load_new);
  }

  // Preserves pending input.
  static void execute_command_in_terminal (const std::string& command)
  {
    if (enabled ())
      instance->do_execute_command_in_terminal (command);
  }

  static uint8NDArray
  get_named_icon (const std::string& icon_name)
  {
    return (enabled () ?
            instance->do_get_named_icon (icon_name) : uint8NDArray ());
  }

  static void set_workspace (void);

  static void set_workspace (bool top_level,
                             const octave::symbol_info_list& syminfo,
                             bool update_variable_editor = true)
  {
    if (enabled ())
      instance->do_set_workspace (top_level, debugging, syminfo,
                                  update_variable_editor);
  }

  static void clear_workspace (void)
  {
    if (enabled ())
      instance->do_clear_workspace ();
  }

  static void set_history (const string_vector& hist)
  {
    if (enabled ())
      instance->do_set_history (hist);
  }

  static void append_history (const std::string& hist_entry)
  {
    if (enabled ())
      instance->do_append_history (hist_entry);
  }

  static void clear_history (void)
  {
    if (enabled ())
      instance->do_clear_history ();
  }

  static void pre_input_event (void)
  {
    if (enabled ())
      instance->do_pre_input_event ();
  }

  static void post_input_event (void)
  {
    if (enabled ())
      instance->do_post_input_event ();
  }

  static void enter_debugger_event (const std::string& file, int line)
  {
    if (enabled ())
      {
        debugging = true;

        instance->do_enter_debugger_event (file, line);
      }
  }

  static void execute_in_debugger_event (const std::string& file, int line)
  {
    if (enabled ())
      instance->do_execute_in_debugger_event (file, line);
  }

  static void exit_debugger_event (void)
  {
    if (enabled () && debugging)
      {
        debugging = false;

        instance->do_exit_debugger_event ();
      }
  }

  static void
  update_breakpoint (bool insert, const std::string& file, int line,
                     const std::string& cond = "")
  {
    if (enabled ())
      instance->do_update_breakpoint (insert, file, line, cond);
  }

  static bool
  show_preferences ()
  {
    if (enabled ())
      {
        instance->do_show_preferences ();
        return true;
      }
    else
      return false;
  }

  static std::string
  gui_preference (const std::string& key,
                  const std::string& value)
  {
    if (enabled ())
      {
        return instance->do_gui_preference (key, value);
      }
    else
      return "";
  }

  static bool
  show_doc (const std::string & file)
  {
    if (enabled ())
      {
        instance->do_show_doc (file);
        return true;
      }
    else
      return false;
  }

  static bool
  register_doc (const std::string & file)
  {
    if (enabled ())
      {
        instance->do_register_doc (file);
        return true;
      }
    else
      return false;
  }

  static bool
  unregister_doc (const std::string & file)
  {
    if (enabled ())
      {
        instance->do_unregister_doc (file);
        return true;
      }
    else
      return false;

  }

  static bool
  edit_variable (const std::string &name, const octave_value& val)
  {
    if (enabled ())
      {
        instance->do_edit_variable (name, val);
        return true;
      }
    else
      return false;
  }

private:

  static octave_link_events *instance;

  static bool instance_ok (void) { return instance != nullptr; }

protected:

  // Semaphore to lock access to the event queue.
  static octave::mutex *event_queue_mutex;

  // Event Queue.
  static octave::event_queue gui_event_queue;

  static bool debugging;
  static bool link_enabled;

  static void rethrow_exception_callback (const std::exception_ptr &p)
  {
    std::rethrow_exception (p);
  }
};

#endif
