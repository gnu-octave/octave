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

/*

The function builtin_pwd adapted from a similar function from GNU
Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991 Free
Software Foundation, Inc.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream.h>

#include <cstdlib>

#include <string>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "defaults.h"
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "ov.h"
#include "toplev.h"
#include "variables.h"
#include "version.h"

string Voctave_home;

string Vbin_dir;
string Vlib_dir;
string Vinfo_dir;
string Varch_lib_dir;
string Vlocal_arch_lib_dir;
string Vfcn_file_dir;

// The path that will be searched for programs that we execute.
// (--exec-path path)
string Vexec_path;

// Load path specified on command line.
// (--path path; -p path)
string Vload_path;

// Name of the editor to be invoked by the edit_history command.
string Veditor;

string Vimagepath;

string Vlocal_site_defaults_file;
string Vsite_defaults_file;

static string
subst_octave_home (const string& s)
{
  string retval;

  string prefix = OCTAVE_PREFIX;

  retval = s;

  if (Voctave_home != prefix)
    {
      int len = prefix.length ();
      size_t start = 0;
      while ((start = s.find (prefix)) != NPOS)
	{
	  retval.replace (start, len, Voctave_home);
	  start++;
	}
    }

  return retval;
}

static void
set_octave_home (void)
{
  char *oh = getenv ("OCTAVE_HOME");

  Voctave_home = oh ? string (oh) : string (OCTAVE_PREFIX);
}

static void
set_default_info_dir (void)
{
  Vinfo_dir = subst_octave_home (OCTAVE_INFODIR);
}

static void
set_default_arch_lib_dir (void)
{
  Varch_lib_dir = subst_octave_home (OCTAVE_ARCHLIBDIR);
}

static void
set_default_local_arch_lib_dir (void)
{
  Vlocal_arch_lib_dir = subst_octave_home (OCTAVE_LOCALARCHLIBDIR);
}

static void
set_default_fcn_file_dir (void)
{
  Vfcn_file_dir = subst_octave_home (OCTAVE_FCNFILEDIR);
}

static void
set_default_bin_dir (void)
{
  Vbin_dir = subst_octave_home (OCTAVE_BINDIR);
}

static void
set_default_lib_dir (void)
{
  Vlib_dir = subst_octave_home (OCTAVE_LIBDIR);
}

static void
set_default_exec_path (void)
{
  char *octave_exec_path = getenv ("OCTAVE_EXEC_PATH");

  if (octave_exec_path)
    Vexec_path = string (octave_exec_path);
  else
    {
      char *shell_path = getenv ("PATH");

      if (shell_path)
	{
	  Vexec_path = string (":");
	  Vexec_path.append (shell_path);
	}
    }
}

// Handle OCTAVE_PATH from the environment like TeX handles TEXINPUTS.
// If the path starts with `:', prepend the standard path.  If it ends
// with `:' append the standard path.  If it begins and ends with
// `:', do both (which is useless, but the luser asked for it...).

static void
set_default_path (void)
{
  string std_path = subst_octave_home (OCTAVE_FCNFILEPATH);

  char *oct_path = getenv ("OCTAVE_PATH");

  Vload_path = oct_path ? string (oct_path) : std_path;
}

static void
set_default_info_file (void)
{
  char *oct_info_file = getenv ("OCTAVE_INFO_FILE");

  if (oct_info_file)
    Vinfo_file = string (oct_info_file);
  else
    {
      Vinfo_file = Vinfo_dir;
      Vinfo_file.append ("/octave.info");
    }
}

static void
set_default_info_prog (void)
{
  char *oct_info_prog = getenv ("OCTAVE_INFO_PROGRAM");

  if (oct_info_prog)
    Vinfo_prog = string (oct_info_prog);
  else
    {
      Vinfo_prog = Varch_lib_dir;
      Vinfo_prog.append ("/info");
    }
}

static void
set_default_editor (void)
{
  Veditor = "vi";

  char *env_editor = getenv ("EDITOR");

  if (env_editor && *env_editor)
    Veditor = string (env_editor);
}

static void
set_local_site_defaults_file (void)
{
  Vlocal_site_defaults_file = subst_octave_home (OCTAVE_LOCALSTARTUPFILEDIR);
  Vlocal_site_defaults_file.append ("/octaverc");
}

static void
set_site_defaults_file (void)
{
  Vsite_defaults_file = subst_octave_home (OCTAVE_STARTUPFILEDIR);
  Vsite_defaults_file.append ("/octaverc");
}

string
maybe_add_default_load_path (const string& pathstring)
{
  string std_path = subst_octave_home (OCTAVE_FCNFILEPATH);

  string retval;

  if (! pathstring.empty ())
    {
      if (pathstring[0] == SEPCHAR)
	{
	  retval = std_path;
	  retval.append (pathstring);
	}
      else
	retval = pathstring;

      if (pathstring[pathstring.length () - 1] == SEPCHAR)
	retval.append (std_path);
    }

  return retval;
}

void
install_defaults (void)
{
  // OCTAVE_HOME must be set first!

  set_octave_home ();

  set_default_info_dir ();

  set_default_arch_lib_dir ();

  set_default_local_arch_lib_dir ();

  set_default_fcn_file_dir ();

  set_default_bin_dir ();

  set_default_lib_dir ();

  set_default_exec_path ();

  set_default_path ();

  set_default_info_file ();

  set_default_info_prog ();

  set_default_editor ();

  set_local_site_defaults_file ();

  set_site_defaults_file ();
}

static int
editor (void)
{
  int status = 0;

  string s = builtin_string_variable ("EDITOR");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("EDITOR");
      status = -1;
    }
  else
    Veditor = s;

  return status;
}

static int
exec_path (void)
{
  int status = 0;

  string s = builtin_string_variable ("EXEC_PATH");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("EXEC_PATH");
      status = -1;
    }
  else
    {
      Vexec_path = s;

      string std_path = Vlocal_arch_lib_dir;
      std_path.append (SEPCHAR_STR);
      std_path.append (Varch_lib_dir);
      std_path.append (SEPCHAR_STR);
      std_path.append (Vbin_dir);

      int std_len = std_path.length ();

      static char *putenv_cmd = 0;

      delete [] putenv_cmd;

      putenv_cmd = 0;

      int eplen = Vexec_path.length ();

      if (eplen > 0)
	{
	  int prepend = (Vexec_path[0] == ':');
	  int append = (eplen > 1 && Vexec_path[eplen-1] == ':');

	  if (prepend)
	    {
	      if (append)
		{
		  putenv_cmd = new char [2 * std_len + eplen + 6];
		  sprintf (putenv_cmd, "PATH=%s%s%s",
			   std_path.c_str (), Vexec_path.c_str (),
			   std_path.c_str ());
		}
	      else
		{
		  putenv_cmd = new char [std_len + eplen + 6];
		  sprintf (putenv_cmd, "PATH=%s%s",
			   std_path.c_str (), Vexec_path.c_str ());
		}
	    }
	  else
	    {
	      if (append)
		{
		  putenv_cmd = new char [std_len + eplen + 6];
		  sprintf (putenv_cmd, "PATH=%s%s",
			   Vexec_path.c_str (), std_path.c_str ());
		}
	      else
		{
		  putenv_cmd = new char [eplen + 6];
		  sprintf (putenv_cmd, "PATH=%s", Vexec_path.c_str ());
		}
	    }
	}
      else
	{
	  putenv_cmd = new char [std_len+6];
	  sprintf (putenv_cmd, "PATH=%s", std_path.c_str ());
	}

      putenv (putenv_cmd);
    }

  return status;
}

static int
imagepath (void)
{
  int status = 0;

  string s = builtin_string_variable ("IMAGEPATH");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("IMAGEPATH");
      status = -1;
    }
  else
    Vimagepath = s;

  return status;
}

static int
loadpath (void)
{
  int status = 0;

  string s = builtin_string_variable ("LOADPATH");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("LOADPATH");
      status = -1;
    }
  else
    Vload_path = maybe_add_default_load_path (s);

  return status;
}

void
symbols_of_defaults (void)
{
  DEFVAR (EDITOR, Veditor, 0, editor,
    "name of the editor to be invoked by the edit_history command");

  DEFVAR (EXEC_PATH, Vexec_path, 0, exec_path,
    "colon separated list of directories to search for programs to run");

  DEFVAR (LOADPATH, Vload_path, 0, loadpath,
    "colon separated list of directories to search for scripts");

  DEFVAR (IMAGEPATH, OCTAVE_IMAGEPATH, 0, imagepath,
    "colon separated list of directories to search for image files");

  DEFCONSTX ("OCTAVE_VERSION", SBV_OCTAVE_VERSION, OCTAVE_VERSION, 0, 0,
    "Octave version");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
