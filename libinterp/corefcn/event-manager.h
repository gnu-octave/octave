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

#if ! defined (octave_event_manager_h)
#define octave_event_manager_h 1

#include "octave-config.h"

#include <functional>
#include <list>
#include <memory>
#include <stack>
#include <string>

#include "oct-mutex.h"
#include "octave.h"
#include "event-queue.h"
#include "uint8NDArray.h"

class octave_value;
class string_vector;

OCTAVE_BEGIN_NAMESPACE(octave)

typedef std::function<void (void)> fcn_callback;
typedef std::function<void (interpreter&)> meth_callback;

class execution_exception;
class symbol_info_list;

// The methods in this class provide a way to pass signals to the GUI
// thread.  A GUI that wishes to act on these events should derive
// from this class and perform actions in a thread-safe way.  In
// Octave's Qt-based GUI, for example, these functions are all
// implemented as wrappers around Qt signals that trigger actions in
// the GUI.  The Qt signal/slot mechanism ensures that the actions are
// properly queued for execution when the objects corresponding to the
// signal and slot belong to different threads.
//
// These functions should not be called directly.  Instead all
// requests from the interpreter for GUI actions should be done
// through the event_manager class.  That class checks to ensure that
// the GUI is connected and enabled before calling these virtual
// functions.

// FIXME: it would be nice if instead of requiring the GUI to derive
// from this class, it could subscribe to individual events, possibly
// multiple times.  In that way, it would be more flexible and
// decentralized, similar to the Qt signal/slot connection mechanism
// and would allow the GUI to connect multiple signals to a single
// action or multiple actions to a single signal.

// FIXME: audit this list of functions and determine whether they are
// all necessary and whether there might be better names for them.

class OCTINTERP_API interpreter_events
{
public:

  interpreter_events (void) = default;

  interpreter_events (const interpreter_events&) = default;

  interpreter_events& operator = (const interpreter_events&) = default;

  virtual ~interpreter_events (void) = default;

  // Note: START_GUI and CLOSE_GUI currently only work with the new
  // experimental terminal widget.

  // Set GUI_APP to true when starting Octave as a gui application
  // (invoked with the --gui option) and false when starting the GUI
  // from the Octave prompt when Octave is already running as a
  // command line application.

  virtual void start_gui (bool /*gui_app*/ = false) { }
  virtual void close_gui (void) { }

  // Dialogs.

  virtual bool have_dialogs (void) const { return false; }

  typedef std::list<std::pair<std::string, std::string>> filter_list;

  virtual std::list<std::string>
  file_dialog (const filter_list& /*filter*/,
               const std::string& /*title*/,
               const std::string& /*filename*/,
               const std::string& /*dirname*/,
               const std::string& /*multimode*/)
  {
    return std::list<std::string> ();
  }

  virtual std::list<std::string>
  input_dialog (const std::list<std::string>& /*prompt*/,
                const std::string& /*title*/,
                const std::list<float>& /*nr*/,
                const std::list<float>& /*nc*/,
                const std::list<std::string>& /*defaults*/)
  {
    return std::list<std::string> ();
  }

  virtual std::pair<std::list<int>, int>
  list_dialog (const std::list<std::string>& /*list*/,
               const std::string& /*mode*/, int /*width*/, int /*height*/,
               const std::list<int>& /*initial_value*/,
               const std::string& /*name*/,
               const std::list<std::string>& /*prompt*/,
               const std::string& /*ok_string*/,
               const std::string& /*cancel_string*/)
  {
    return std::pair<std::list<int>, int> ();
  }

  virtual std::string
  question_dialog (const std::string& /*msg*/, const std::string& /*title*/,
                   const std::string& /*btn1*/, const std::string& /*btn2*/,
                   const std::string& /*btn3*/, const std::string& /*btndef*/)
  {
    return "";
  }

  virtual void update_path_dialog (void) {  }

  virtual void show_preferences (void) { }

  virtual void apply_preferences (void) { }

  virtual void show_terminal_window (void) { }

  virtual bool show_documentation (const std::string& /*file*/)
  {
    return false;
  }

  virtual void show_file_browser (void) { }

  virtual void show_command_history (void) { }

  virtual void show_workspace (void) { }

  virtual void show_community_news (int /*serial*/) { }
  virtual void show_release_notes (void) { }

  virtual bool edit_file (const std::string& /*file*/) { return false; }

  virtual void
  edit_variable (const std::string& /*name*/, const octave_value& /*val*/)
  { }

  // Other requests for user interaction, usually some kind of
  // confirmation before another action.  Could these be reformulated
  // using the question_dialog action?

  virtual bool confirm_shutdown (void) { return true; }

  virtual bool prompt_new_edit_file (const std::string& /*file*/)
  {
    return false;
  }

  virtual int
  debug_cd_or_addpath_error (const std::string& /*file*/,
                             const std::string& /*dir*/,
                             bool /*addpath_option*/)
  {
    return -1;
  }

  // Requests for information normally stored in the GUI.

  virtual uint8NDArray get_named_icon (const std::string& /*icon_name*/)
  {
    return uint8NDArray ();
  }

  virtual std::string gui_preference (const std::string& /*key*/,
                                      const std::string& /*value*/)
  {
    return "";
  }

  // Requests for GUI action that do not require user interaction.
  // These are different from other notifications in that they are not
  // associated with changes in the interpreter state (like a change
  // in the current working directory or command history).

  virtual bool copy_image_to_clipboard (const std::string& /*file*/)
  {
    return false;
  }

  virtual void focus_window (const std::string /*win_name*/)
  { }

  virtual void
  execute_command_in_terminal (const std::string& /*command*/) { }

  virtual void register_documentation (const std::string& /*file*/) { }

  virtual void unregister_documentation (const std::string& /*file*/) { }

  virtual void interpreter_output (const std::string& /*msg*/) { }

  virtual void display_exception (const execution_exception& ee, bool beep);

  virtual void gui_status_update (const std::string& /*feature*/,
                                  const std::string& /*status*/) { }

  virtual void update_gui_lexer (void) { }

  // Notifications of events in the interpreter that a GUI will
  // normally wish to respond to.

  virtual void directory_changed (const std::string& /*dir*/) { }

  virtual void
  file_remove (const std::string& /*old_nm*/, const std::string& /*new_nm*/)
  { }

  virtual void file_renamed (bool) { }

  virtual void
  set_workspace (bool /*top_level*/, bool /*debug*/,
                 const symbol_info_list& /*syminfo*/,
                 bool /*update_variable_editor*/)
  { }

  virtual void clear_workspace (void) { }

  virtual void update_prompt (const std::string& /*prompt*/) { }

  virtual void set_history (const string_vector& /*hist*/) { }

  virtual void append_history (const std::string& /*hist_entry*/) { }

  virtual void clear_history (void) { }

  virtual void pre_input_event (void) { }

  virtual void post_input_event (void) { }

  virtual void
  enter_debugger_event (const std::string& /*fcn_name*/,
                        const std::string& /*fcn_file_name*/,
                        int /*line*/)
  { }

  virtual void
  execute_in_debugger_event (const std::string& /*file*/, int /*line*/) { }

  virtual void exit_debugger_event (void) { }

  virtual void
  update_breakpoint (bool /*insert*/, const std::string& /*file*/,
                     int /*line*/, const std::string& /*cond*/)
  { }

  virtual void interpreter_interrupted (void) { }
};

//! Provides threadsafe access to octave.
//!
//! This class provides thread-safe communication between the
//! interpreter and a GUI.

class
OCTINTERP_API
event_manager
{
public:

  OCTINTERP_API event_manager (interpreter& interp);

  // No copying!

  event_manager (const event_manager&) = delete;

  event_manager&
  operator = (const event_manager&) = delete;

  virtual ~event_manager (void);

  // OBJ should be an object of a class that is derived from the base
  // class interpreter_events, or nullptr to disconnect and delete the
  // previous link.

  OCTINTERP_API void
  connect_link (const std::shared_ptr<interpreter_events>& obj);

  OCTINTERP_API bool enable (void);

  bool disable (void)
  {
    bool retval = m_link_enabled;
    m_link_enabled = false;
    return retval;
  }

  bool enabled (void) const
  {
    return m_link_enabled;
  }

  // Make the Qt actions available for others.  This is a temporary
  // solution to allow Qt actions like opening the documentation
  // browser when the primary interpreter_events object is not the one
  // defined for the Qt GUI.
  void
  install_qt_event_handlers (const std::shared_ptr<interpreter_events>& obj)
  {
    m_qt_event_handlers = obj;
  }

  std::shared_ptr<interpreter_events>
  qt_event_handlers (void) const { return m_qt_event_handlers; }

  // If disable is TRUE, then no additional events will be processed
  // other than exit.

  OCTINTERP_API void process_events (bool disable = false);

  OCTINTERP_API void discard_events (void);

  // The post_event and post_exception functions provide a thread-safe
  // way for the GUI to queue interpreter functions for execution.
  // The queued functions are executed when the interpreter is
  // otherwise idle.

  void push_event_queue (void);
  void pop_event_queue (void);

  OCTINTERP_API void post_event (const fcn_callback& fcn);
  OCTINTERP_API void post_event (const meth_callback& meth);

  // The following functions correspond to the virtual fuunctions in
  // the interpreter_events class.  They provide a way for the
  // interpreter to notify the GUI that some event has occurred
  // (directory or workspace changed, for example) or to request the
  // GUI to perform some action (display a dialog, for example).

  // Please keep this list of declarations in the same order as the
  // ones above in the interpreter_events class.


  // Note: START_GUI and CLOSE_GUI currently only work with the new
  // experimental terminal object.

  void start_gui (bool gui_app = false)
  {
    if (enabled ())
      m_instance->start_gui (gui_app);
  }

  void close_gui (void)
  {
    if (enabled ())
      m_instance->close_gui ();
  }

  // Dialogs

  bool have_dialogs (void) const
  {
    return m_qt_event_handlers && m_qt_event_handlers->have_dialogs ();
  }

  typedef std::list<std::pair<std::string, std::string>> filter_list;

  std::list<std::string>
  file_dialog (const filter_list& filter, const std::string& title,
               const std::string& filename, const std::string& dirname,
               const std::string& multimode)
  {
    return (enabled () && have_dialogs ()
            ? m_instance->file_dialog (filter, title, filename, dirname,
                                       multimode)
            : std::list<std::string> ());
  }

  std::list<std::string>
  input_dialog (const std::list<std::string>& prompt,
                const std::string& title,
                const std::list<float>& nr,
                const std::list<float>& nc,
                const std::list<std::string>& defaults)
  {
    return (enabled () && have_dialogs ()
            ? m_instance->input_dialog (prompt, title, nr, nc, defaults)
            : std::list<std::string> ());
  }

  std::pair<std::list<int>, int>
  list_dialog (const std::list<std::string>& list,
               const std::string& mode,
               int width, int height,
               const std::list<int>& initial_value,
               const std::string& name,
               const std::list<std::string>& prompt,
               const std::string& ok_string,
               const std::string& cancel_string)
  {
    return (enabled () && have_dialogs ()
            ? m_instance->list_dialog (list, mode, width, height,
                                       initial_value, name, prompt,
                                       ok_string, cancel_string)
            : std::pair<std::list<int>, int> ());
  }

  std::string
  question_dialog (const std::string& msg, const std::string& title,
                   const std::string& btn1, const std::string& btn2,
                   const std::string& btn3, const std::string& btndef)
  {
    return (enabled () && have_dialogs ()
            ? m_instance->question_dialog (msg, title, btn1,
                                           btn2, btn3, btndef)
            : "");
  }

  void update_path_dialog (void)
  {
    if (application::is_gui_running () && enabled ())
      m_instance->update_path_dialog ();
  }

  bool show_preferences (void)
  {
    if (enabled ())
      {
        m_instance->show_preferences ();
        return true;
      }
    else
      return false;
  }

  bool apply_preferences (void)
  {
    if (enabled ())
      {
        m_instance->apply_preferences ();
        return true;
      }
    else
      return false;
  }

  void show_terminal_window (void)
  {
    if (enabled ())
      m_instance->show_terminal_window ();
  }

  bool show_documentation (const std::string& file)
  {
    return enabled () ? m_instance->show_documentation (file) : false;
  }

  void show_file_browser (void)
  {
    if (enabled ())
      m_instance->show_file_browser ();
  }

  void show_command_history (void)
  {
    if (enabled ())
      m_instance->show_command_history ();
  }

  void show_workspace (void)
  {
    if (enabled ())
      m_instance->show_workspace ();
  }

  void show_community_news (int serial = -1)
  {
    if (enabled ())
      m_instance->show_community_news (serial);
  }

  void show_release_notes (void)
  {
    if (enabled ())
      m_instance->show_release_notes ();
  }

  bool edit_file (const std::string& file)
  {
    return enabled () ? m_instance->edit_file (file) : false;
  }

  bool edit_variable (const std::string& name, const octave_value& val)
  {
    if (enabled ())
      {
        m_instance->edit_variable (name, val);
        return true;
      }
    else
      return false;
  }

  bool confirm_shutdown (void)
  {
    bool retval = true;

    if (enabled ())
      retval = m_instance->confirm_shutdown ();

    return retval;
  }

  bool prompt_new_edit_file (const std::string& file)
  {
    return enabled () ? m_instance->prompt_new_edit_file (file) : false;
  }

  int debug_cd_or_addpath_error (const std::string& file,
                                 const std::string& dir, bool addpath_option)
  {
    return (enabled ()
            ? m_instance->debug_cd_or_addpath_error (file, dir,
                addpath_option)
            : 0);
  }

  uint8NDArray get_named_icon (const std::string& icon_name)
  {
    return (enabled ()
            ? m_instance->get_named_icon (icon_name) : uint8NDArray ());
  }

  std::string gui_preference (const std::string& key,
                              const std::string& value)
  {
    return enabled () ? m_instance->gui_preference (key, value) : "";
  }

  bool copy_image_to_clipboard (const std::string& file)
  {
    return enabled () ? m_instance->copy_image_to_clipboard (file) : false;
  }

  virtual void focus_window (const std::string win_name)
  {
    if (enabled ())
      m_instance->focus_window (win_name);
  }

  // Preserves pending input.
  void execute_command_in_terminal (const std::string& command)
  {
    if (enabled ())
      m_instance->execute_command_in_terminal (command);
  }

  bool register_documentation (const std::string& file)
  {
    if (enabled ())
      {
        m_instance->register_documentation (file);
        return true;
      }
    else
      return false;
  }

  bool unregister_documentation (const std::string& file)
  {
    if (enabled ())
      {
        m_instance->unregister_documentation (file);
        return true;
      }
    else
      return false;
  }

  bool interpreter_output (const std::string& msg)
  {
    if (enabled ())
      {
        m_instance->interpreter_output (msg);
        return true;
      }
    else
      return false;
  }

  bool display_exception (const execution_exception& ee, bool beep = false)
  {
    if (enabled ())
      {
        m_instance->display_exception (ee, beep);
        return true;
      }
    else
      return false;
  }

  bool gui_status_update (const std::string& feature,
                          const std::string& status)
  {
    if (enabled ())
      {
        m_instance->gui_status_update (feature, status);
        return true;
      }
    else
      return false;
  }

  bool update_gui_lexer (void)
  {
    if (enabled ())
      {
        m_instance->update_gui_lexer ();
        return true;
      }
    else
      return false;
  }

  void directory_changed (const std::string& dir)
  {
    if (enabled ())
      m_instance->directory_changed (dir);
  }

  // Methods for removing/renaming files which might be open in editor
  void file_remove (const std::string& old_name, const std::string& new_name)
  {
    if (application::is_gui_running () && enabled ())
      m_instance->file_remove (old_name, new_name);
  }

  void file_renamed (bool load_new)
  {
    if (application::is_gui_running () && enabled ())
      m_instance->file_renamed (load_new);
  }

  OCTINTERP_API void set_workspace (void);

  void set_workspace (bool top_level, const symbol_info_list& syminfo,
                      bool update_variable_editor = true)
  {
    if (enabled ())
      m_instance->set_workspace (top_level, m_debugging, syminfo,
                                 update_variable_editor);
  }

  void clear_workspace (void)
  {
    if (enabled ())
      m_instance->clear_workspace ();
  }

  void update_prompt (const std::string& prompt)
  {
    if (enabled ())
      m_instance->update_prompt (prompt);
  }

  OCTINTERP_API void set_history (void);

  void set_history (const string_vector& hist)
  {
    if (enabled ())
      m_instance->set_history (hist);
  }

  void append_history (const std::string& hist_entry)
  {
    if (enabled ())
      m_instance->append_history (hist_entry);
  }

  void clear_history (void)
  {
    if (enabled ())
      m_instance->clear_history ();
  }

  void pre_input_event (void)
  {
    if (enabled ())
      m_instance->pre_input_event ();
  }

  void post_input_event (void)
  {
    if (enabled ())
      m_instance->post_input_event ();
  }

  void enter_debugger_event (const std::string& fcn_name,
                             const std::string& fcn_file_name, int line)
  {
    if (enabled ())
      {
        m_debugging = true;

        m_instance->enter_debugger_event (fcn_name, fcn_file_name, line);
      }
  }

  void execute_in_debugger_event (const std::string& file, int line)
  {
    if (enabled ())
      m_instance->execute_in_debugger_event (file, line);
  }

  void exit_debugger_event (void)
  {
    if (enabled () && m_debugging)
      {
        m_debugging = false;

        m_instance->exit_debugger_event ();
      }
  }

  void update_breakpoint (bool insert, const std::string& file,
                          int line, const std::string& cond = "")
  {
    if (enabled ())
      m_instance->update_breakpoint (insert, file, line, cond);
  }

  void interpreter_interrupted (void)
  {
    if (enabled ())
      m_instance->interpreter_interrupted ();
  }

protected:

  // Semaphore to lock access to the event queue.
  mutex *m_event_queue_mutex;

  // Event Queue.  We use a stack so that we can handle evaluation in
  // the debugger when we are executing in server mode.  In server
  // mode, code is evaluated from inside the event queue.  So when the
  // evaluator reaches a breakpoint, the queue is already locked and
  // executing an event function.  We can't just add a new command to the
  // existing queue, so we need another one that can process new
  // events generated at the debug prompt.  When execution continues
  // (dbcont or dbstep, for example) we pop the queue and return to
  // the previous point of execution.

  std::stack<std::shared_ptr <event_queue>> m_gui_event_queue;

  bool m_debugging;
  bool m_link_enabled;

private:

  interpreter& m_interpreter;

  // Using a shared_ptr to manage the link_events object ensures that it
  // will be valid until it is no longer needed.

  std::shared_ptr<interpreter_events> m_instance;

  std::shared_ptr<interpreter_events> m_qt_event_handlers;

};

OCTAVE_END_NAMESPACE(octave)

#endif
