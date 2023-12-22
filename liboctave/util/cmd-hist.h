////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2024 The Octave Project Developers
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

#if ! defined (octave_cmd_hist_h)
#define octave_cmd_hist_h 1

#include "octave-config.h"

#include <string>

#include "str-vec.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class OCTAVE_API command_history
{
protected:

  command_history ()
    : m_initialized (false), m_ignoring_additions (false),
      m_history_control (0), m_lines_in_file (0),
      m_lines_this_session (0), m_file (), m_size (-1)
  { }

public:

  OCTAVE_DISABLE_COPY_MOVE (command_history)

  virtual ~command_history () = default;

  static void initialize (bool, const std::string&, int, const std::string&);

  static bool is_initialized ();

  static void set_file (const std::string&);

  static std::string file ();

  static void process_histcontrol (const std::string&);

  static std::string histcontrol ();

  static void set_size (int);

  static int size ();

  static void ignore_entries (bool = true);

  static bool ignoring_entries ();

  static bool add (const std::string&);

  static void remove (int);

  static void clear ();

  static int where ();

  static int length ();

  static int max_input_history ();

  static int base ();

  static int current_number ();

  static void stifle (int);

  static int unstifle ();

  static int is_stifled ();

  static void set_mark (int n);

  // Gag.  This declaration has to match the Function typedef in
  // readline.h.

  static int goto_mark ();

  static void read (bool = true);

  static void read (const std::string&, bool = true);

  static void read_range (int = -1, int = -1, bool = true);

  static void read_range (const std::string&, int = -1, int = -1,
                          bool = true);

  static void write (const std::string& = "");

  static void append (const std::string& = "");

  static void truncate_file (const std::string& = "", int = -1);

  static string_vector list (int = -1, bool = false);

  static std::string get_entry (int);

  static void replace_entry (int, const std::string&);

  static void clean_up_and_save (const std::string& = "", int = -1);

private:

  static bool instance_ok ();

  static void make_command_history ();

  // The real thing.
  static command_history *s_instance;

  static void cleanup_instance ()
  {
    delete s_instance;
    s_instance = nullptr;
  }

protected:

  // To use something other than the GNU history library, derive a new
  // class from command_history, overload these functions as
  // necessary, and make instance point to the new class.

  virtual void do_set_file (const std::string&);

  virtual std::string do_file ();

  virtual void do_process_histcontrol (const std::string&);

  virtual std::string do_histcontrol () const { return ""; }

  virtual void do_initialize (bool, const std::string&, int,
                              const std::string&);

  virtual bool do_is_initialized () const;

  virtual void do_set_size (int);

  virtual int do_size () const;

  virtual void do_ignore_entries (bool);

  virtual bool do_ignoring_entries () const;

  virtual bool do_add (const std::string&);

  virtual void do_remove (int);

  virtual void do_clear ();

  virtual int do_where () const;

  virtual int do_length () const;

  virtual int do_max_input_history () const;

  virtual int do_base () const;

  virtual int do_current_number () const;

  virtual void do_stifle (int);

  virtual int do_unstifle ();

  virtual int do_is_stifled () const;

  virtual void do_set_mark (int);

  virtual int do_goto_mark ();

  virtual void do_read (const std::string&, bool);

  virtual void do_read_range (const std::string&, int, int, bool);

  virtual void do_write (const std::string&) const;

  virtual void do_append (const std::string&);

  virtual void do_truncate_file (const std::string&, int) const;

  virtual string_vector do_list (int, bool) const;

  virtual std::string do_get_entry (int) const;

  virtual void do_replace_entry (int, const std::string&);

  virtual void do_clean_up_and_save (const std::string&, int);

  void error (int, const std::string& msg = "") const;

  void error (const std::string&) const;

  // TRUE means we have initialized the history filename and number of
  // lines to save.
  bool m_initialized;

  // TRUE means we are ignoring new additions.
  bool m_ignoring_additions;

  // Bitmask for history control options.  See oct-rl-hist.h.
  int m_history_control;

  // The number of history lines we read from the history file.
  int m_lines_in_file;

  // The number of history lines we've saved so far.
  int m_lines_this_session;

  // The default history file.
  std::string m_file;

  // The number of lines of history to save.
  int m_size;
};

OCTAVE_END_NAMESPACE(octave)

#endif
