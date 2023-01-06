////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

class
OCTAVE_API
command_history
{
protected:

  command_history (void)
    : m_initialized (false), m_ignoring_additions (false),
      m_history_control (0), m_lines_in_file (0),
      m_lines_this_session (0), m_file (), m_size (-1)
  { }

public:

  // No copying!

  command_history (const command_history&) = delete;

  command_history& operator = (const command_history&) = delete;

  virtual ~command_history (void) = default;

  static void initialize (bool, const std::string&, int, const std::string&);

  static bool is_initialized (void);

  static void set_file (const std::string&);

  static std::string file (void);

  static void process_histcontrol (const std::string&);

  static std::string histcontrol (void);

  static void set_size (int);

  static int size (void);

  static void ignore_entries (bool = true);

  static bool ignoring_entries (void);

  static bool add (const std::string&);

  static void remove (int);

  static void clear (void);

  static int where (void);

  static int length (void);

  static int max_input_history (void);

  static int base (void);

  static int current_number (void);

  static void stifle (int);

  static int unstifle (void);

  static int is_stifled (void);

  static void set_mark (int n);

  // Gag.  This declaration has to match the Function typedef in
  // readline.h.

  static int goto_mark (void);

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

  static bool instance_ok (void);

  static void make_command_history (void);

  // The real thing.
  static command_history *s_instance;

  static void cleanup_instance (void)
  {
    delete s_instance;
    s_instance = nullptr;
  }

protected:

  // To use something other than the GNU history library, derive a new
  // class from command_history, overload these functions as
  // necessary, and make instance point to the new class.

  virtual void do_set_file (const std::string&);

  virtual std::string do_file (void);

  virtual void do_process_histcontrol (const std::string&);

  virtual std::string do_histcontrol (void) const { return ""; }

  virtual void do_initialize (bool, const std::string&, int,
                              const std::string&);

  virtual bool do_is_initialized (void) const;

  virtual void do_set_size (int);

  virtual int do_size (void) const;

  virtual void do_ignore_entries (bool);

  virtual bool do_ignoring_entries (void) const;

  virtual bool do_add (const std::string&);

  virtual void do_remove (int);

  virtual void do_clear (void);

  virtual int do_where (void) const;

  virtual int do_length (void) const;

  virtual int do_max_input_history (void) const;

  virtual int do_base (void) const;

  virtual int do_current_number (void) const;

  virtual void do_stifle (int);

  virtual int do_unstifle (void);

  virtual int do_is_stifled (void) const;

  virtual void do_set_mark (int);

  virtual int do_goto_mark (void);

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
