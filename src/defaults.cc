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

#include "oct-env.h"
#include "pathsearch.h"

#include <defaults.h>
#include "defun.h"
#include "error.h"
#include "file-ops.h"
#include "gripes.h"
#include "help.h"
#include "oct-obj.h"
#include "ov.h"
#include "toplev.h"
#include "variables.h"
#include <version.h>

string Voctave_home;

string Vbin_dir;
string Vinfo_dir;
string Vdata_dir;
string Vlibexec_dir;
string Varch_lib_dir;
string Vlocal_arch_lib_dir;
string Vfcn_file_dir;

// The path that will be searched for programs that we execute.
// (--exec-path path)
string Vexec_path;

// Load path specified on command line.
// (--path path; -p path)
string Vload_path;

// And the cached directory path corresponding to Vload_path.
dir_path Vload_path_dir_path;

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
      while ((start = retval.find (prefix, start)) != NPOS)
	{
	  retval.replace (start, len, Voctave_home);
	  start += len;
	}
    }

  return retval;
}

static void
set_octave_home (void)
{
  string oh = octave_env::getenv ("OCTAVE_HOME");

  Voctave_home = oh.empty () ? string (OCTAVE_PREFIX) : oh;
}

static void
set_default_info_dir (void)
{
  Vinfo_dir = subst_octave_home (OCTAVE_INFODIR);
}

static void
set_default_data_dir (void)
{
  Vdata_dir = subst_octave_home (OCTAVE_DATADIR);
}

static void
set_default_libexec_dir (void)
{
  Vlibexec_dir = subst_octave_home (OCTAVE_LIBEXECDIR);
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
set_default_exec_path (void)
{
  string octave_exec_path = octave_env::getenv ("OCTAVE_EXEC_PATH");

  if (octave_exec_path.empty ())
    {
      string shell_path = octave_env::getenv ("PATH");

      if (! shell_path.empty ())
	{
	  Vexec_path = string (":");
	  Vexec_path.append (shell_path);
	}
    }
  else
    Vexec_path = string (octave_exec_path);
}

// Handle OCTAVE_PATH from the environment like TeX handles TEXINPUTS.
// If the path starts with `:', prepend the standard path.  If it ends
// with `:' append the standard path.  If it begins and ends with
// `:', do both (which is useless, but the luser asked for it...).

static void
set_default_path (void)
{
  string std_path = subst_octave_home (OCTAVE_FCNFILEPATH);

  string oct_path = octave_env::getenv ("OCTAVE_PATH");

  Vload_path = oct_path.empty () ? std_path : oct_path;

  Vload_path_dir_path = dir_path (Vload_path);
}

static void
set_default_info_file (void)
{
  string std_info_file = subst_octave_home (OCTAVE_INFOFILE);

  string oct_info_file = octave_env::getenv ("OCTAVE_INFO_FILE");

  Vinfo_file = oct_info_file.empty () ? std_info_file : oct_info_file;
}

static void
set_default_info_prog (void)
{
  string oct_info_prog = octave_env::getenv ("OCTAVE_INFO_PROGRAM");

  if (oct_info_prog.empty ())
    Vinfo_prog = "info";
  else
    Vinfo_prog = string (oct_info_prog);
}

static void
set_default_editor (void)
{
  Veditor = "emacs";

  string env_editor = octave_env::getenv ("EDITOR");

  if (! env_editor.empty ())
    Veditor = env_editor;
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

  set_default_data_dir ();

  set_default_libexec_dir ();

  set_default_arch_lib_dir ();

  set_default_local_arch_lib_dir ();

  set_default_fcn_file_dir ();

  set_default_bin_dir ();

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

      string std_path = Vlocal_arch_lib_dir + string (SEPCHAR_STR)
	+ Varch_lib_dir + string (SEPCHAR_STR) + Vbin_dir;

      string path;

      int eplen = Vexec_path.length ();

      if (eplen > 0)
	{
	  bool prepend = (Vexec_path[0] == ':');
	  bool append = (eplen > 1 && Vexec_path[eplen-1] == ':');

	  if (prepend)
	    {
	      path = std_path + Vexec_path;

	      if (append)
		path.append (std_path);
	    }
	  else
	    {
	      path = Vexec_path;

	      if (append)
		path.append (std_path);
	    }
	}
      else
	path = std_path;

      octave_env::putenv ("PATH", path);
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
    {
      Vload_path = maybe_add_default_load_path (s);

      Vload_path_dir_path = dir_path (Vload_path);
    }

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

  DEFCONST (OCTAVE_HOME, Voctave_home,
    "top-level Octave installation directory");

  DEFCONSTX ("OCTAVE_VERSION", SBV_OCTAVE_VERSION, OCTAVE_VERSION,
    "Octave version");
}

DEFUN (rehash, , ,
  "rehash (): reinitialize LOADPATH directory cache")
{
  octave_value_list retval;

  Vload_path_dir_path.rehash ();

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
