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

#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <iostream.h>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "file-ops.h"
#include "lo-error.h"
#include "oct-env.h"
#include "oct-passwd.h"
#include "statdefs.h"
#include "str-vec.h"

// We provide a replacement for mkdir().

int
file_ops::mkdir (const string& name, mode_t mode)
{
  return ::mkdir (name.c_str (), mode);
}

int
file_ops::mkdir (const string& name, mode_t mode, string& msg)
{
  msg = string ();

  int status = ::mkdir (name.c_str (), mode);

  if (status < 0)
    msg = ::strerror (errno);

  return status;
}

// I don't know how to emulate this on systems that don't provide it.

int
file_ops::mkfifo (const string& name, mode_t mode)
{
#if defined (HAVE_MKFIFO)
  return ::mkfifo (name.c_str (), mode);
#else
  (*current_liboctave_error_handler)
    ("mkfifo: not implemented on this system");
  return -1;
#endif
}

int
file_ops::mkfifo (const string& name, mode_t mode, string& msg)
{
  msg = string ();

#if defined (HAVE_MKFIFO)
  int status = ::mkfifo (name.c_str (), mode);

  if (status < 0)
    msg = ::strerror (errno);

  return status;
#else
  (*current_liboctave_error_handler)
    ("mkfifo: not implemented on this system");
  return -1;
#endif
}

// We provide a replacement for rename().

int
file_ops::rename (const string& from, const string& to)
{
  return ::rename (from.c_str (), to.c_str ());
}

int
file_ops::rename (const string& from, const string& to, string& msg)
{
  msg = string ();

  int status = ::rename (from.c_str (), to.c_str ());

  if (status < 0)
    msg = ::strerror (errno);

  return status;
}

// We provide a replacement for rmdir().

int
file_ops::rmdir (const string& name)
{
  return ::rmdir (name.c_str ());
}

int
file_ops::rmdir (const string& name, string& msg)
{
  msg = string ();

  int status = ::rmdir (name.c_str ());

  if (status < 0)
    msg = ::strerror (errno);

  return status;
}

// We provide a replacement for tempnam().

string
file_ops::tempnam (void)
{
  string retval;

  char *tmp = ::tempnam (0, "oct-");

  if (tmp)
    {
      retval = tmp;

      free (tmp);
    }
  else
    (*current_liboctave_error_handler) ("can't open temporary file!");

  return retval;
}

// If NAME has a leading ~ or ~user, Unix-style, expand it to the
// user's home directory.  If no ~, or no <pwd.h>, just return NAME.

// Mostly stolen from kpathsea.  Readline also has a more complicated
// tilde-expand function, but we can probalby get by with something a
// bit simpler.

// XXX FIXME XXX
#define DIR_SEP_CHAR '/'

string
file_ops::tilde_expand (const string& name)
{
  string expansion = name;

  // If no leading tilde, do nothing.

  size_t beg = name.find_first_not_of (" \t");

  if (beg != NPOS && name[beg] == '~')
    {
      // If `~' or `~/', use $HOME if it exists, or `.' if it doesn't.

      // If `~user' or `~user/', look up user in the passwd database.

      size_t len = name.length ();

      if (beg == len-1 || name[beg+1] == DIR_SEP_CHAR)
	{
	  string home = octave_env::get_home_directory ();

	  if (home.empty ())
	    home = ".";
        
	  expansion = name.substr (0, beg) + home;

	  if (beg < len)
	    expansion.append (name.substr (beg+1));
	}
      else
	{
	  size_t end = name.find (DIR_SEP_CHAR, beg);

	  size_t len = end;

	  if (len != NPOS)
	    len -= beg + 1;

	  string user = name.substr (beg+1, len);

	  octave_passwd pw = octave_passwd::getpwnam (user);

	  // If no such user, just use `.'.

	  string home = pw.empty () ? : string (".") : pw.dir ();
      
	  expansion = string (" ", beg) + home;

	  if (end != NPOS)
	    expansion.append (name.substr (end));
	}
    }

  return expansion;
}

// A vector version of the above.

string_vector
file_ops::tilde_expand (const string_vector& names)
{
  string_vector retval;

  int n = names.length ();

  retval.resize (n);

  for (int i = 0; i < n; i++)
    retval[i] = file_ops::tilde_expand (names[i]);

  return retval;
}

int
file_ops::umask (mode_t mode)
{
#if defined (HAVE_UMASK)
  return ::umask (mode);
#else
  return 0;
#endif
}

int
file_ops::unlink (const string& name)
{
  return ::unlink (name.c_str ());
}

int
file_ops::unlink (const string& name, string& errmsg)
{
  errmsg = string ();

  int status = ::unlink (name.c_str ());

  if (status < 0)
    errmsg = ::strerror (errno);

  return status;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
