/*

Copyright (C) 1996 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_cmd_hist_h)
#define octave_cmd_hist_h 1

#include <string>

#include "str-vec.h"

class
command_history
{
public:

  command_history (const string& = string (), int = -1, bool = false)

  command_history (const string& = string (), int = -1)

  ~command_history (void) { initialized = false; }

  void set_file (const string&);

  string file (void);

  void set_size (int);

  int size (void);

  void ignore_entries (bool = true);

  bool ignoring_entries (void);

  void add (const string&);

  void remove (int);

  int where (void);

  int base (void);

  int current_number (void);

  void stifle (int);

  int unstifle (void);

  int is_stifled (void);

  void read (bool = true);

  void read (const string& = string (), bool = true)

  void read_range (int = -1, int = -1, bool = true)

  void read_range (const string& = string (), int = -1, int = -1,
		   bool = true)

  void write (const string& = string ());

  void append (const string& = string ());

  void truncate_file (const string& = string (), int = -1);

  string_vector list (int = -1, int = 0);

  string get_entry (int);

  void replace_entry (int, const string&);

  void clean_up_and_save (const string& = string (), int = -1);

private:

  // We can only have one history object in any given program.
  static bool initialized;

  // TRUE means we are ignoring new additions.
  bool ignoring_additions;

  // The number of hisory lines we read from the history file.
  int lines_in_file;

  // The number of history lines we've saved so far.
  int lines_this_session;

  // The default history file.
  string xfile;

  // The number of lines of history to save.
  int xsize;

  void error (int);

  void error (const string&);

  command_history (const command_history&);

  command_history& operator = (const command_history&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
