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

#include <cstring>

#include <strstream.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include <readline/history.h>

#include "file-ops.h"
#include "lo-error.h"
#include "cmd-hist.h"

bool command_history::initialized = false;

command_history::command_history (const string& f, int n)
{
  if (initialized)
    error ("only one history object can be active at once");
  else
    {
      ignoring_additions = false;

      lines_in_file = 0;
      lines_this_session = 0;

      xsize = -1;

      if (! f.empty ())
	{
	  xfile = f;

	  ::read_history (f.c_str ());

	  lines_in_file = where ();

	  ::using_history ();
	}

      if (n > 0)
	xsize = n;

      initialized = true;
    }
}

void
command_history::set_file (const string& f)
{
  xfile = f;
}

string
command_history::file (void)
{
  return xfile;
}

void
command_history::set_size (int n)
{
  xsize = n;
}

int
command_history::size (void)
{
  return xsize;
}

void
command_history::ignore_entries (bool flag)
{
  ignoring_additions = flag;
}

bool
command_history::ignoring_entries (void)
{
  return ignoring_additions;
}

void
command_history::add (const string& s)
{
  if (! ignoring_entries ())
    {
      if (s.empty ()
	  || (s.length () == 1 && (s[0] == '\r' || s[0] == '\n')))
	return;

      ::add_history (s.c_str ());
      lines_this_session++;
    }
}

void
command_history::remove (int n)
{
  HIST_ENTRY *discard = ::remove_history (n);

  if (discard)
    {
      if (discard->line)
	::free (discard->line);

      ::free (discard);
    }
}

int
command_history::where (void)
{
  return ::where_history ();
}

int
command_history::base (void)
{
  return ::history_base;
}

int
command_history::current_number (void)
{
  return (xsize > 0) ? base () + where () : -1;
}

void
command_history::stifle (int n)
{
  ::stifle_history (n);
}

int
command_history::unstifle (void)
{
  return ::unstifle_history ();
}

int
command_history::is_stifled (void)
{
  return ::history_is_stifled ();
}

void
command_history::read (bool must_exist)
{
  read (xfile, must_exist);
}

void
command_history::read (const string& f, bool must_exist)
{
  if (! f.empty ())
    {
      int status = ::read_history (f.c_str ());

      if (status != 0 && must_exist)
	error (status);
      else
	{
	  lines_in_file = where ();

	  ::using_history ();
	}
    }
  else
    error ("command_history::read: missing file name");
}

void
command_history::read_range (int from, int to, bool must_exist)
{
  read_range (xfile, from, to, must_exist);
}

void
command_history::read_range (const string& f, int from, int to,
			     bool must_exist)
{
  if (from < 0)
    from = lines_in_file;

  if (! f.empty ())
    {
      int status = ::read_history_range (f.c_str (), from, to);

      if (status != 0 && must_exist)
	error (status);
      else
	{
	  lines_in_file = where ();

	  ::using_history ();
	}
    }
  else
    error ("command_history::read_range: missing file name");
}

void
command_history::write (const string& f_arg)
{
  string f = f_arg;

  if (f.empty ())
    f = xfile;

  if (! f.empty ())
    {
      int status = ::write_history (f.c_str ());

      if (status != 0)
	error (status);
    }
  else
    error ("command_history::write: missing file name");
}

void
command_history::append (const string& f_arg)
{
  if (lines_this_session)
    {
      if (lines_this_session < where ())
	{
	  // Create file if it doesn't already exist.

	  string f = f_arg;

	  if (f.empty ())
	    f = xfile;

	  if (! f.empty ())
	    {
	      file_stat fs (f);

	      if (! fs)
		{
		  int tem;

		  tem = open (f.c_str (), O_CREAT, 0666);
		  close (tem);
		}

	      int status = ::append_history (lines_this_session, f.c_str ());

	      if (status != 0)
		error (status);
	      else
		lines_in_file += lines_this_session;

	      lines_this_session = 0;
	    }
	  else
	    error ("comman_history::append: missing file name");
	}
    }
}

void
command_history::truncate_file (const string& f_arg, int n)
{
  string f = f_arg;

  if (f.empty ())
    f = xfile;

  if (! f.empty ())
    ::history_truncate_file (f.c_str (), n);
  else
    error ("command_history::truncate_file: missing file name");
}

string_vector
command_history::list (int limit, int number_lines)
{
  string_vector retval;

  if (limit)
    {
      HIST_ENTRY **hlist = ::history_list ();

      if (hlist)
	{
	  int end = 0;
	  while (hlist[end])
	    end++;

	  int beg = (limit < 0 || end < limit) ? 0 : (end - limit);

	  retval.resize (end - beg);

	  int k = 0;
	  for (int i = beg; i < end; i++)
	    {
	      ostrstream output_buf;

	      if (number_lines)
		output_buf.form ("%5d%c", i + ::history_base,
				 hlist[i]->data ? '*' : ' '); 

	      output_buf << hlist[i]->line << ends;

	      const char *tmp = output_buf.str ();

	      retval[k++] = tmp;

	      delete [] tmp;  
	    }
	}
    }

  return retval;
}

string
command_history::get_entry (int n)
{
  string retval;

  HIST_ENTRY *entry = ::history_get (::history_base + n);

  if (entry && entry->line)
    retval = entry->line;

  return retval;
}

void
command_history::replace_entry (int which, const string& line)
{
  HIST_ENTRY *discard = ::replace_history_entry (which, line.c_str (), 0);

  if (discard)
    {
      if (discard->line)
	::free (discard->line);

      ::free (discard);
    }
}

void
command_history::clean_up_and_save (const string& f_arg, int n)
{
  string f = f_arg;

  if (f.empty ())
    f = xfile;

  if (! f.empty ())
    {
      if (n < 0)
	n = xsize;

      stifle (n);

      ::write_history (f.c_str ());
    }
  else
    error ("command_history::clean_up_and_save: missing file name");
}

void
command_history::error (int err_num)
{
  (*current_liboctave_error_handler) ("%s", strerror (err_num));
}

void
command_history::error (const string& s)
{
  (*current_liboctave_error_handler) ("%s", s.c_str ());
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
