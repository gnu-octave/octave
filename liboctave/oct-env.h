/*

Copyright (C) 1996, 1997 John W. Eaton

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

#if !defined (octave_env_h)
#define octave_env_h 1

#include <string>

class
octave_env
{
protected:

  octave_env (void);

public:

  static string polite_directory_format (const string& name);

  static bool absolute_pathname (const string& s);

  static string base_pathname (const string& s);

  static string make_absolute (const string& s, const string& dot_path);

  static string getcwd (void);

  static string get_home_directory (void);

  static string get_program_name (void);

  static string get_program_invocation_name (void);

  static string get_user_name (void);

  static string get_host_name (void);

  static string getenv (const string& name);

  static void putenv (const string& name, const string& value);

  static bool chdir (const string& newdir);

  static void set_program_name (const string& s);

private:

  static bool instance_ok (void);

  string do_polite_directory_format (const string& name) const;

  bool do_absolute_pathname (const string& s) const;

  string do_base_pathname (const string& s) const;

  string do_make_absolute (const string& s, const string& dot_path) const;

  string do_getcwd (void);

  string do_get_home_directory (void) const;

  string do_get_user_name (void) const;

  string do_get_host_name (void) const;

  string do_getenv (const string& name) const;

  void do_putenv (const string& name, const string& value) const;

  bool do_chdir (const string& newdir);

  void do_set_program_name (const string& s) const;

  void pathname_backup (string& path, int n) const;

  void error (int) const;

  void error (const string&) const;

  // No copying!

  octave_env (const octave_env&);

  octave_env& operator = (const octave_env&);

  // The real thing.
  static octave_env *instance;

  // TRUE means follow symbolic links that point to directories just
  // as if they are real directories.
  bool follow_symbolic_links;

  // TRUE means that pwd always give verbatim directory, regardless
  // of symbolic link following.
  bool verbatim_pwd;

  // Where are we?
  string current_directory;

  // Etc.
  mutable string program_name;

  mutable string program_invocation_name;

  mutable string user_name;

  mutable string host_name;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
