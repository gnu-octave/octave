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

#if ! defined (octave_oct_env_h)
#define octave_oct_env_h 1

#include "octave-config.h"

#include <string>

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(sys)

class
OCTAVE_API
env
{
protected:

  env ();

public:

  // No copying!

  env (const env&) = delete;

  env& operator = (const env&) = delete;

  static std::string polite_directory_format (const std::string& name);

  static bool absolute_pathname (const std::string& s);

  static bool rooted_relative_pathname (const std::string& s);

  static std::string base_pathname (const std::string& s);

  static std::string
  make_absolute (const std::string& s,
                 const std::string& dot_path = get_current_directory ());

  static std::string get_current_directory ();

  static std::string get_home_directory ();

  static std::string get_temp_directory ();

  static std::string get_user_config_directory ();

  static std::string get_user_data_directory ();

  static std::string get_program_name ();

  static std::string get_program_invocation_name ();

  static std::string get_user_name ();

  static std::string get_host_name ();

  static std::string getenv (const std::string& name);

  static void putenv (const std::string& name, const std::string& value);

  static bool have_x11_display ();

  static bool chdir (const std::string& newdir);

  static void set_program_name (const std::string& s);

private:

  static bool instance_ok ();

  std::string do_polite_directory_format (const std::string& name);

  bool do_absolute_pathname (const std::string& s) const;

  bool do_rooted_relative_pathname (const std::string& s) const;

  std::string do_base_pathname (const std::string& s) const;

  std::string do_make_absolute (const std::string& s,
                                const std::string& dot_path) const;

  std::string do_getcwd ();

  std::string do_get_home_directory ();

  std::string do_get_temp_directory () const;

  std::string do_get_user_config_directory ();

  std::string do_get_user_data_directory ();

  std::string do_get_user_name ();

  std::string do_get_host_name ();

  std::string do_getenv (const std::string& name) const;

  void do_putenv (const std::string& name, const std::string& value) const;

  bool do_chdir (const std::string& newdir);

  void do_set_program_name (const std::string& s);

  void pathname_backup (std::string& path, int n) const;

  void error (int) const;

  void error (const std::string&) const;

  // The real thing.
  static env *m_instance;

  static void cleanup_instance ()
  { delete m_instance; m_instance = nullptr; }

  // TRUE means follow symbolic links that point to directories just
  // as if they are real directories.
  bool m_follow_symbolic_links;

  // TRUE means that pwd always give verbatim directory, regardless
  // of symbolic link following.
  bool m_verbatim_pwd;

  // Where are we?
  std::string m_current_directory;

  // Etc.
  std::string m_prog_name;

  std::string m_prog_invocation_name;

  std::string m_user_name;

  std::string m_host_name;
};

OCTAVE_END_NAMESPACE(sys)
OCTAVE_END_NAMESPACE(octave)

#endif
