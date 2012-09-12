/*

Copyright (C) 2011-2012 Jacob Dawid
Copyright (C) 2011-2012 John P. Swensen

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

#ifndef OCTAVEEVENT_H
#define OCTAVEEVENT_H

#include <string>

class octave_value_list;

#include "octave-event-observer.h"

/**
  * \class octave_event
  * \brief Base class for an octave event.
  * In order to make communication with octave threadsafe, comunication is
  * implemented via events. An application may create events and post them,
  * however there is no guarantee events will be processed in a given time.
  *
  * In order to create an event, there must be an event observer. The event
  * observer will be given the opportunity to react on the event as soon as
  * it has been processed in the octave thread. Accepting and ignoring takes
  * places in the octave thread.
  */
class octave_event
{
  public:
    octave_event (octave_event_observer& o)
      : _octave_event_observer (o)
    { }

    virtual ~octave_event ()
    { }

    /** Performs what is necessary to make this event happen. This
      * code is thread-safe since it will be executed in the octave
      * thread. However, you should take care to keep this code as
      * short as possible. */
    virtual bool perform () = 0;

    /**
      * Accepts this event. This allows the event observer to react properly
      * onto the event.
      */
    void accept ()
    { _octave_event_observer.event_accepted (this); }

    /**
      * Rejects this event. This allows the event observer to react properly
      * onto the event.
      */
    void reject ()
    { _octave_event_observer.event_reject (this); }

  protected:
    void call_octave_function (const std::string& name);

    void call_octave_function (const std::string& name,
                               const octave_value_list& args,
                               int nargout = 0);

    void finish_readline_event () const;

  private:
    octave_event_observer& _octave_event_observer;
};

class octave_update_history_event : public octave_event
{
  public:
    /** Creates a new octave_exit_event. */
    octave_update_history_event (octave_event_observer& o)
      : octave_event (o)
    { }

    bool perform ()
    { return true; /* Always grant. */ }
};

class octave_update_workspace_event : public octave_event
{
  public:
    /** Creates a new octave_exit_event. */
    octave_update_workspace_event (octave_event_observer& o)
      : octave_event (o)
    { }

    bool perform ()
    { return true; /* Always grant. */ }
};

/** Implements an octave exit event. */
class octave_exit_event : public octave_event
{
  public:
    /** Creates a new octave_exit_event. */
    octave_exit_event (octave_event_observer& o)
      : octave_event (o)
    { }

    bool perform ();
};

/** Implements an octave run file event. */
class octave_run_file_event : public octave_event
{
  public:
    /** Creates a new octave_run_file_event. */
    octave_run_file_event (octave_event_observer& o,
                           const std::string& file)
      : octave_event (o)
    { _file = file; }

    bool perform ();

  private:
    std::string _file;
};

/** Implements a change directory event. */
class octave_change_directory_event : public octave_event
{
  public:
    /** Creates a new octave_change_directory_event. */
    octave_change_directory_event (octave_event_observer& o,
                                   const std::string& directory)
      : octave_event (o)
    { _directory = directory; }

    bool perform ();

  private:
    std::string _directory;
};

/** Implements a clear workspace event. */
class octave_clear_workspace_event : public octave_event
{
  public:
    /** Creates a new octave_run_file_event. */
    octave_clear_workspace_event (octave_event_observer& o)
      : octave_event (o)
    { }

    bool perform ()
    {
      call_octave_function ("clear");
      return true;
    }
};

/** Implements a load workspace event. */
class octave_load_workspace_event : public octave_event
{
  public:
    /** Creates a new octave_change_directory_event. */
    octave_load_workspace_event (octave_event_observer& o,
                                 const std::string& file)
      : octave_event (o)
    { _file = file; }

    bool perform ();

  private:
    std::string _file;
};

/** Implements a save workspace event. */
class octave_save_workspace_event : public octave_event
{
  public:
    /** Creates a new octave_change_directory_event. */
    octave_save_workspace_event (octave_event_observer& o,
                                 const std::string& file)
      : octave_event (o)
    { _file = file; }

    bool perform ();

  private:
    std::string _file;
};

class octave_clear_history_event : public octave_event
{
  public:
    /** Creates a new octave_clear_history_event. */
    octave_clear_history_event (octave_event_observer& o)
      : octave_event (o)
    { }

  bool perform ();
};

class octave_add_breakpoint_event : public octave_event
{
  public:
    octave_add_breakpoint_event (octave_event_observer& o,
                                 const std::string& path,
                                 const std::string& function_name,
                                 int line)
      : octave_event (o)
    {
      _path = path;
      _function_name = function_name;
      _line = line;
    }

    bool perform ();

    std::string get_path ()
    {
      return _path;
    }

    std::string get_function_name ()
    {
      return _function_name;
    }

    int get_line ()
    {
      return _line;
    }

  private:
    std::string _path;
    std::string _function_name;
    int _line;
};

class octave_remove_breakpoint_event : public octave_event
{
  public:
    octave_remove_breakpoint_event (octave_event_observer& o,
                                    const std::string& path,
                                    const std::string& function_name,
                                    int line)
      : octave_event (o)
    {
      _path = path;
      _function_name = function_name;
      _line = line;
    }

    bool perform ();

    std::string get_path ()
    {
      return _path;
    }

    std::string get_function_name ()
    {
      return _function_name;
    }

    int get_line ()
    {
      return _line;
    }

  private:
    std::string _path;
    std::string _function_name;
    int _line;
};

class octave_remove_all_breakpoints_event : public octave_event
{
  public:
    octave_remove_all_breakpoints_event (octave_event_observer& o,
                                         const std::string& path,
                                         const std::string& function_name)
      : octave_event (o)
    {
      _path = path;
      _function_name = function_name;
    }

    bool perform ();

    std::string get_path ()
    {
      return _path;
    }

    std::string get_function_name ()
    {
      return _function_name;
    }

  private:
    std::string _path;
    std::string _function_name;
};

class octave_debug_step_into_event : public octave_event
{
  public:
    /** Creates a new octave_debug_step_into_event. */
    octave_debug_step_into_event (octave_event_observer& o)
      : octave_event (o) { }

    bool perform ();
};

class octave_debug_step_over_event : public octave_event
{
  public:
    /** Creates a new octave_debug_step_over_event. */
    octave_debug_step_over_event (octave_event_observer& o)
      : octave_event (o) { }

    bool perform ()
    {
      call_octave_function ("dbnext");
      finish_readline_event ();
      return true;
    }
};

class octave_debug_step_out_event : public octave_event
{
  public:
    /** Creates a new octave_debug_step_out_event. */
    octave_debug_step_out_event (octave_event_observer& o)
      : octave_event (o) { }

    bool perform ();
};

class octave_debug_continue_event : public octave_event
{
  public:
    /** Creates a new octave_debug_step_out_event. */
    octave_debug_continue_event (octave_event_observer& o)
      : octave_event (o) { }

    bool perform ()
    {
      call_octave_function ("dbcont");
      finish_readline_event ();
      return true;
    }
};

class octave_debug_quit_event : public octave_event
{
  public:
    /** Creates a new octave_debug_step_out_event. */
    octave_debug_quit_event (octave_event_observer& o)
      : octave_event (o) { }

    bool perform ()
    {
      call_octave_function ("dbquit");
      finish_readline_event ();
      return true;
    }
};

#endif // OCTAVEEVENT_H
