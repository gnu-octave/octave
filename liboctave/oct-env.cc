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

/*

The functions listed below were adapted from a similar functions
from GNU Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991
Free Software Foundation, Inc.

  octave_env::do_absolute_pathname
  octave_env::do_base_pathname
  octave_env::do_chdir
  octave_env::do_getcwd
  octave_env::do_make_absolute
  octave_env::do_polite_directory_format
  octave_env::pathname_backup

*/ 

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "lo-error.h"
#include "lo-sysdep.h"
#include "lo-utils.h"
#include "oct-env.h"
#include "oct-passwd.h"
#include "oct-syscalls.h"

octave_env::octave_env (void)
  : follow_symbolic_links (true), verbatim_pwd (true),
    current_directory (), program_name (), program_invocation_name (),
    user_name (), host_name ()
{
  // Get a real value for the current directory.
  do_getcwd ();

  // Etc.
  do_get_user_name ();

  do_get_host_name ();
}

octave_env *octave_env::instance = 0;

bool
octave_env::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    instance = new octave_env ();

  if (! instance)
    {
      (*current_liboctave_error_handler)
	("unable to create current working directoy object!");

      retval = false;
    }

  return retval;
}

string
octave_env::polite_directory_format (const string& name)
{
  return (instance_ok ())
    ? instance->do_polite_directory_format (name) : string ();
}

bool
octave_env::absolute_pathname (const string& s)
{
  return (instance_ok ())
    ? instance->do_absolute_pathname (s) : false;
}

string
octave_env::base_pathname (const string& s)
{
  return (instance_ok ())
    ? instance->do_base_pathname (s) : string ();
}

string
octave_env::make_absolute (const string& s, const string& dot_path)
{
  return (instance_ok ())
    ? instance->do_make_absolute (s, dot_path) : string ();
}

string
octave_env::getcwd ()
{
  return (instance_ok ())
    ? instance->do_getcwd () : string ();
}

string
octave_env::get_home_directory ()
{
  return (instance_ok ())
    ? instance->do_get_home_directory () : string ();
}

string
octave_env::get_program_name (void)
{
  return (instance_ok ())
    ? instance->program_name : string ();
}

string
octave_env::get_program_invocation_name (void)
{
  return (instance_ok ())
    ? instance->program_invocation_name : string ();
}

void
octave_env::set_program_name (const string& s)
{
  if (instance_ok ())
    instance->do_set_program_name (s);
}

string
octave_env::get_user_name (void)
{
  return (instance_ok ())
    ? instance->do_get_user_name () : string ();
}

string
octave_env::get_host_name (void)
{
  return (instance_ok ())
    ? instance->do_get_host_name () : string ();
}

// XXX FIXME XXX -- this leaves no way to distinguish between a
// variable that is not set and one that is set to the empty string.
// Is this a problem?

string
octave_env::getenv (const string& name)
{
  return (instance_ok ())
    ? instance->do_getenv (name) : string ();
}

void
octave_env::putenv (const string& name, const string& value)
{
  octave_putenv (name, value);
}

bool
octave_env::chdir (const string& newdir)
{
  return (instance_ok ())
    ? instance->do_chdir (newdir) : false;
}

void
octave_env::do_set_program_name (const string& s) const
{
  program_invocation_name = s;

  size_t pos = program_invocation_name.rfind ('/');

  program_name = (pos == NPOS)
    ? program_invocation_name : program_invocation_name.substr (pos+1);
}

// Return a pretty pathname.  If the first part of the pathname is the
// same as $HOME, then replace that with `~'.

string
octave_env::do_polite_directory_format (const string& name) const
{
  string retval;

  string home_dir = do_get_home_directory ();

  size_t len = home_dir.length ();

  if (len > 1 && home_dir.compare (name, 0, len) == 0
      && (name.length () == len || name[len] == '/'))
    {
      retval = "~";
      retval.append (name.substr (len));
    }
  else
    retval = name;

  return retval;
}

// Return 1 if STRING contains an absolute pathname, else 0.

bool
octave_env::do_absolute_pathname (const string& s) const
{
  if (s.empty ())
    return 0;

  if (s[0] == '/')
    return true;

  if (s[0] == '.')
    {
      if (s[1] == '\0' || s[1] == '/')
	return true;

      if (s[1] == '.')
	if (s[2] == '\0' || s[2] == '/')
	  return true;
    }

  return false;
}

// Return the `basename' of the pathname in STRING (the stuff after
// the last '/').  If STRING is not a full pathname, simply return it.

string
octave_env::do_base_pathname (const string& s) const
{
  if (! do_absolute_pathname (s))
    return s;

  size_t pos = s.rfind ('/');

  if (pos == NPOS)
    return s;
  else
    return s.substr (pos+1);
}

// Turn STRING (a pathname) into an absolute pathname, assuming that
// DOT_PATH contains the symbolic location of '.'.

string
octave_env::do_make_absolute (const string& s, const string& dot_path) const
{
#if defined (__EMX__)
  if (s.length () > 1 && s[1] == ':')
    return s;
#endif

  if (dot_path.empty () || s[0] == '/' || s.empty ())
    return s;

  string current_path = dot_path;

  if (current_path.empty ())
    current_path = "./";

  size_t pos = current_path.length () - 1;

  if (current_path[pos] != '/')
    current_path.append ("/");

  size_t i = 0;
  size_t slen = s.length ();

  while (i < slen)
    {
      if (s[i] == '.')
	{
	  if (i + 1 == slen)
	    return current_path;

	  if (s[i+1] == '/')
	    {
	      i += 2;
	      continue;
	    }

	  if (s[i+1] == '.' && (i + 2 == slen || s[i+2] == '/'))
	    {
	      i += 2;

	      if (i != slen)
		i++;

	      pathname_backup (current_path, 1);

	      continue;
	    }
	}

      size_t tmp = s.find ('/', i);

      if (tmp == NPOS)
	{
	  current_path.append (s, i, tmp-i);
	  break;
	}
      else
	{
	  current_path.append (s, i, tmp-i+1);
	  i = tmp + 1;
	}
    }

  return current_path;
}

// Return a consed string which is the current working directory.

string
octave_env::do_getcwd ()
{
  if (! follow_symbolic_links)
    current_directory = "";

  if (verbatim_pwd || current_directory.empty ())
    current_directory = ::octave_getcwd ();

  return current_directory;
}

// This value is not cached because it can change while Octave is
// running.

string
octave_env::do_get_home_directory (void) const
{
  string hd = do_getenv ("HOME");

  if (hd.empty ())
    {
      octave_passwd pw = octave_passwd::getpwuid (octave_syscalls::getuid ());

      hd = pw ? pw.dir () : string ("/");
    }

  return hd;
}

string
octave_env::do_get_user_name (void) const
{
  // XXX FIXME XXX -- is it possible for this to change while Octave
  // is running?

  if (user_name.empty ())
    {
      octave_passwd pw = octave_passwd::getpwuid (octave_syscalls::getuid ());

      user_name = pw ? pw.name () : string ("unknown");
    }

  return user_name;
}

string
octave_env::do_get_host_name (void) const
{
  // XXX FIXME XXX -- is it possible for this to change while Octave
  // is running?

  if (host_name.empty ())
    {
      char hostname[256];

      int status = gethostname (hostname, 255);

      host_name = (status < 0) ? "unknown" : hostname;
    }

  return host_name;
}

string
octave_env::do_getenv (const string& name) const
{
  char *value = ::getenv (name.c_str ());

  return value ? value : "";
}

// Do the work of changing to the directory NEWDIR.  Handle symbolic
// link following, etc.

bool
octave_env::do_chdir (const string& newdir)
{
  bool retval = false;

  string tmp;

  if (follow_symbolic_links)
    {
      if (current_directory.empty ())
	do_getcwd ();

      if (current_directory.empty ())
	tmp = newdir;
      else
	tmp = do_make_absolute (newdir, current_directory);

      // Get rid of trailing `/'.

      size_t len = tmp.length ();

      if (len > 1)
	{
	  if (tmp[--len] == '/')
	    tmp.resize (len);
	}

      if (! ::octave_chdir (tmp))
	{
	  current_directory = tmp;
	  retval = true;
	}
    }
  else
    retval = (! ::octave_chdir (newdir));

  return retval;
}

// Remove the last N directories from PATH.

void
octave_env::pathname_backup (string& path, int n) const
{
  if (path.empty ())
    return;

  size_t i = path.length () - 1;

  while (n--)
    {
      while (path[i] == '/' && i > 0)
	i--;

      while (path[i] != '/' && i > 0)
	i--;

      i++;
    }

  path.resize (i);
}

void
octave_env::error (int err_num) const
{
  (*current_liboctave_error_handler) ("%s", strerror (err_num));
}

void
octave_env::error (const string& s) const
{
  (*current_liboctave_error_handler) ("%s", s.c_str ());
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
