/*

Copyright (C) 1996, 1997, 2000, 2002, 2004, 2005, 2006, 2007
              John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstring>

#include <iostream>
#include <string>

#include "cmd-edit.h"
#include "cmd-hist.h"
#include "file-ops.h"
#include "lo-error.h"
#include "str-vec.h"

command_history *command_history::instance = 0;

#if defined (USE_READLINE)

#include <cstdlib>

#include <sys/types.h>
#include <unistd.h>

#include <fcntl.h>

#include "oct-rl-hist.h"

#include "file-stat.h"

class
gnu_history : public command_history
{
public:

  gnu_history (void)
    : command_history (), mark (0) { }

  ~gnu_history (void) { }

  void do_add (const std::string&);

  void do_remove (int);

  int do_where (void);

  int do_length (void);

  int do_max_input_history (void);

  int do_base (void);

  int do_current_number (void);

  void do_stifle (int);

  int do_unstifle (void);

  int do_is_stifled (void);

  void do_set_mark (int);

  int do_goto_mark (void);

  void do_read (const std::string&, bool);

  void do_read_range (const std::string&, int, int, bool);

  void do_write (const std::string&);

  void do_append (const std::string&);

  void do_truncate_file (const std::string&, int);

  string_vector do_list (int, bool);

  std::string do_get_entry (int);

  void do_replace_entry (int, const std::string&);

  void do_clean_up_and_save (const std::string&, int);

private:

  int mark;
};

void
gnu_history::do_add (const std::string& s)
{
  if (! do_ignoring_entries ())
    {
      if (s.empty ()
          || (s.length () == 1 && (s[0] == '\r' || s[0] == '\n')))
        return;

      ::octave_add_history (s.c_str ());

      lines_this_session++;
    }
}

void
gnu_history::do_remove (int n)
{
  ::octave_remove_history (n);
}

int
gnu_history::do_where (void)
{
  return ::octave_where_history ();
}

int
gnu_history::do_length (void)
{
  return ::octave_history_length ();
}

int
gnu_history::do_max_input_history (void)
{
  return ::octave_max_input_history ();
}

int
gnu_history::do_base (void)
{
  return ::octave_history_base ();
}

int
gnu_history::do_current_number (void)
{
  return (xsize > 0) ? do_base () + do_where () : -1;
}

void
gnu_history::do_stifle (int n)
{
  ::octave_stifle_history (n);
}

int
gnu_history::do_unstifle (void)
{
  return ::octave_unstifle_history ();
}

int
gnu_history::do_is_stifled (void)
{
  return ::octave_history_is_stifled ();
}

void
gnu_history::do_set_mark (int n)
{
  mark = n;
}

int
gnu_history::do_goto_mark (void)
{
  if (mark)
    {
      char *line = ::octave_history_goto_mark (mark);

      if (line)
        {
          command_editor::insert_text (line);

          command_editor::clear_undo_list ();
        }
    }

  mark = 0;

  // FIXME -- for operate_and_get_next.
  command_editor::remove_startup_hook (command_history::goto_mark);

  return 0;
}

void
gnu_history::do_read (const std::string& f, bool must_exist)
{
  if (! f.empty ())
    {
      int status = ::octave_read_history (f.c_str ());

      if (status != 0 && must_exist)
        error (status);
      else
        {
          lines_in_file = do_where ();

          ::octave_using_history ();
        }
    }
  else
    error ("gnu_history::read: missing file name");
}

void
gnu_history::do_read_range (const std::string& f, int from, int to,
                            bool must_exist)
{
  if (from < 0)
    from = lines_in_file;

  if (! f.empty ())
    {
      int status = ::octave_read_history_range (f.c_str (), from, to);

      if (status != 0 && must_exist)
        error (status);
      else
        {
          lines_in_file = do_where ();

          ::octave_using_history ();
        }
    }
  else
    error ("gnu_history::read_range: missing file name");
}

void
gnu_history::do_write (const std::string& f_arg)
{
  std::string f = f_arg;

  if (f.empty ())
    f = xfile;

  if (! f.empty ())
    {
      int status = ::octave_write_history (f.c_str ());

      if (status != 0)
        error (status);
    }
  else
    error ("gnu_history::write: missing file name");
}

void
gnu_history::do_append (const std::string& f_arg)
{
  if (lines_this_session)
    {
      if (lines_this_session < do_where ())
        {
          // Create file if it doesn't already exist.

          std::string f = f_arg;

          if (f.empty ())
            f = xfile;

          if (! f.empty ())
            {
              file_stat fs (f);

              if (! fs)
                {
                  int tem;

                  tem = gnulib::open (f.c_str (), O_CREAT, 0666);
                  gnulib::close (tem);
                }

              int status
                = ::octave_append_history (lines_this_session, f.c_str ());

              if (status != 0)
                error (status);
              else
                lines_in_file += lines_this_session;

              lines_this_session = 0;
            }
          else
            error ("gnu_history::append: missing file name");
        }
    }
}

void
gnu_history::do_truncate_file (const std::string& f_arg, int n)
{
  std::string f = f_arg;

  if (f.empty ())
    f = xfile;

  if (! f.empty ())
    ::octave_history_truncate_file (f.c_str (), n);
  else
    error ("gnu_history::truncate_file: missing file name");
}

string_vector
gnu_history::do_list (int limit, bool number_lines)
{
  string_vector retval;

  if (limit)
    retval = ::octave_history_list (limit, number_lines);

  return retval;
}

std::string
gnu_history::do_get_entry (int n)
{
  std::string retval;

  char *line = ::octave_history_get (do_base () + n);

  if (line)
    retval = line;

  return retval;
}

void
gnu_history::do_replace_entry (int which, const std::string& line)
{
  ::octave_replace_history_entry (which, line.c_str ());
}

void
gnu_history::do_clean_up_and_save (const std::string& f_arg, int n)
{
  std::string f = f_arg;

  if (f.empty ())
    f = xfile;

  if (! f.empty ())
    {
      if (n < 0)
        n = xsize;

      stifle (n);

      do_write (f.c_str ());
    }
  else
    error ("gnu_history::clean_up_and_save: missing file name");
}

#endif

bool
command_history::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    make_command_history ();

  if (! instance)
    {
      (*current_liboctave_error_handler)
        ("unable to create command history object!");

      retval = false;
    }

  return retval;
}

void
command_history::make_command_history (void)
{
#if defined (USE_READLINE)
  instance = new gnu_history ();
#else
  instance = new command_history ();
#endif
}

void
command_history::set_file (const std::string& f_arg)
{
  if (instance_ok ())
    {
      std::string f = file_ops::tilde_expand (f_arg);

      instance->do_set_file (f);
    }
}

std::string
command_history::file (void)
{
  return (instance_ok ())
    ? instance->do_file () : std::string ();
}

void
command_history::set_size (int n)
{
  if (instance_ok ())
    instance->do_set_size (n);
}

int
command_history::size (void)
{
  return (instance_ok ())
    ? instance->do_size () : 0;
}

void
command_history::ignore_entries (bool flag)
{
  if (instance_ok ())
    instance->do_ignore_entries (flag);
}

bool
command_history::ignoring_entries (void)
{
  return (instance_ok ())
    ? instance->do_ignoring_entries () : false;
}

void
command_history::add (const std::string& s)
{
  if (instance_ok ())
    instance->do_add (s);
}

void
command_history::remove (int n)
{
  if (instance_ok ())
    instance->do_remove (n);
}

int
command_history::where (void)
{
  return (instance_ok ())
    ? instance->do_where () : 0;
}

int
command_history::length (void)
{
  return (instance_ok ())
    ? instance->do_length () : 0;
}

int
command_history::max_input_history (void)
{
  return (instance_ok ())
    ? instance->do_max_input_history () : 0;
}

int
command_history::base (void)
{
  return (instance_ok ())
    ? instance->do_base () : 0;
}

int
command_history::current_number (void)
{
  return (instance_ok ())
    ? instance->do_current_number () : 0;
}

void
command_history::stifle (int n)
{
  if (instance_ok ())
    instance->do_stifle (n);
}

int
command_history::unstifle (void)
{
  return (instance_ok ())
    ? instance->do_unstifle () : 0;
}

int
command_history::is_stifled (void)
{
  return (instance_ok ())
    ? instance->do_is_stifled () : 0;
}

void
command_history::set_mark (int n)
{
  if (instance_ok ())
    instance->do_set_mark (n);
}

int
command_history::goto_mark (void)
{
  return (instance_ok ())
    ? instance->do_goto_mark () : 0;
}

void
command_history::read (bool must_exist)
{
  read (file (), must_exist);
}

void
command_history::read (const std::string& f, bool must_exist)
{
  if (instance_ok ())
    instance->do_read (f, must_exist);
}

void
command_history::read_range (int from, int to, bool must_exist)
{
  read_range (file (), from, to, must_exist);
}

void
command_history::read_range (const std::string& f, int from, int to,
                             bool must_exist) 
{
  if (instance_ok ())
    instance->do_read_range (f, from, to, must_exist);
}

void
command_history::write (const std::string& f)
{
  if (instance_ok ())
    instance->do_write (f);
}

void
command_history::append (const std::string& f)
{
  if (instance_ok ())
    instance->do_append (f);
}

void
command_history::truncate_file (const std::string& f, int n)
{
  if (instance_ok ())
    instance->do_truncate_file (f, n);
}

string_vector
command_history::list (int limit, bool number_lines)
{
  return (instance_ok ())
    ? instance->do_list (limit, number_lines) : string_vector ();
}

std::string
command_history::get_entry (int n)
{
  return (instance_ok ())
    ? instance->do_get_entry (n) : std::string ();
}

void
command_history::replace_entry (int which, const std::string& line)
{
  if (instance_ok ())
    instance->do_replace_entry (which, line);
}

void
command_history::clean_up_and_save (const std::string& f, int n)
{
  if (instance_ok ())
    instance->do_clean_up_and_save (f, n);
}

void
command_history::do_set_file (const std::string& f)
{
  xfile = f;
}

std::string
command_history::do_file (void)
{
  return xfile;
}

void
command_history::do_set_size (int n)
{
  xsize = n;
}

int
command_history::do_size (void)
{
  return xsize;
}

void
command_history::do_ignore_entries (bool flag)
{
  ignoring_additions = flag;
}

bool
command_history::do_ignoring_entries (void)
{
  return ignoring_additions;
}

void
command_history::do_add (const std::string&)
{
}

void
command_history::do_remove (int)
{
}

int
command_history::do_where (void)
{
  return 0;
}

int
command_history::do_length (void)
{
  return 0;
}

int
command_history::do_max_input_history (void)
{
  return 0;
}

int
command_history::do_base (void)
{
  return 0;
}

int
command_history::do_current_number (void)
{
  return (xsize > 0) ? do_base () + do_where () : -1;
}

void
command_history::do_stifle (int)
{
}

int
command_history::do_unstifle (void)
{
  return -1;
}

int
command_history::do_is_stifled (void)
{
  return 0;
}

void
command_history::do_set_mark (int)
{
}

int
command_history::do_goto_mark (void)
{
  return 0;
}

void
command_history::do_read (const std::string& f, bool)
{
  if (f.empty ())
    error ("command_history::read: missing file name");
}

void
command_history::do_read_range (const std::string& f, int, int, bool)
{
  if (f.empty ())
    error ("command_history::read_range: missing file name");
}

void
command_history::do_write (const std::string& f_arg)
{
  std::string f = f_arg;

  if (f.empty ())
    f = xfile;

  if (f.empty ())
    error ("command_history::write: missing file name");
}

void
command_history::do_append (const std::string& f_arg)
{
  if (lines_this_session)
    {
      if (lines_this_session < do_where ())
        {
          // Create file if it doesn't already exist.

          std::string f = f_arg;

          if (f.empty ())
            f = xfile;

          if (f.empty ())
            error ("command_history::append: missing file name");
        }
    }
}

void
command_history::do_truncate_file (const std::string& f_arg, int)
{
  std::string f = f_arg;

  if (f.empty ())
    f = xfile;

  if (f.empty ())
    error ("command_history::truncate_file: missing file name");
}

string_vector
command_history::do_list (int, bool)
{
  return string_vector ();
}

std::string
command_history::do_get_entry (int)
{
  return std::string ();
}

void
command_history::do_replace_entry (int, const std::string&)
{
}

void
command_history::do_clean_up_and_save (const std::string& f_arg, int)
{
  std::string f = f_arg;

  if (f.empty ())
    f = xfile;

  if (f.empty ())
    error ("command_history::clean_up_and_save: missing file name");
}

void
command_history::error (int err_num)
{
  (*current_liboctave_error_handler) ("%s", gnulib::strerror (err_num));
}

void
command_history::error (const std::string& s)
{
  (*current_liboctave_error_handler) ("%s", s.c_str ());
}
