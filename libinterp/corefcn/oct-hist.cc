/*

Copyright (C) 1993-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

/*

The functions listed below were adapted from similar functions from
GNU Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991 Free
Software Foundation, Inc.

  do_history         edit_history_readline
  do_edit_history    edit_history_add_hist

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstdlib>
#include <cstring>

#include <fstream>
#include <string>

#include "cmd-hist.h"
#include "file-ops.h"
#include "lo-mappers.h"
#include "octave-link.h"
#include "oct-env.h"
#include "oct-time.h"
#include "str-vec.h"
#include "unistd-wrappers.h"

#include "defaults.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "input.h"
#include "oct-hist.h"
#include "ovl.h"
#include "pager.h"
#include "parse.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "interpreter.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// TRUE means input is coming from temporary history file.
bool input_from_tmp_history_file = false;

static std::string
default_history_file (void)
{
  std::string file;

  std::string env_file = octave::sys::env::getenv ("OCTAVE_HISTFILE");

  if (! env_file.empty ())
    file = env_file;

  if (file.empty ())
    file = octave::sys::file_ops::concat (octave::sys::env::get_home_directory (),
                                          ".octave_hist");

  return file;
}

static int
default_history_size (void)
{
  int size = 1000;

  std::string env_size = octave::sys::env::getenv ("OCTAVE_HISTSIZE");

  if (! env_size.empty ())
    {
      int val;

      if (sscanf (env_size.c_str (), "%d", &val) == 1)
        size = (val > 0 ? val : 0);
    }

  return size;
}

static std::string
default_history_timestamp_format (void)
{
  return
    std::string ("# Octave " OCTAVE_VERSION ", %a %b %d %H:%M:%S %Y %Z <")
    + octave::sys::env::get_user_name ()
    + std::string ("@")
    + octave::sys::env::get_host_name ()
    + std::string (">");
}

// The format of the timestamp marker written to the history file when
// Octave exits.
static std::string Vhistory_timestamp_format_string
  = default_history_timestamp_format ();

// Display, save, or load history.  Stolen and modified from bash.
//
// Arg of -w FILENAME means write file, arg of -r FILENAME
// means read file, arg of -q means don't number lines.  Arg of N
// means only display that many items.

static string_vector
do_history (const octave_value_list& args, int nargout)
{
  bool numbered_output = nargout == 0;

  octave::unwind_protect frame;

  string_vector hlist;

  frame.add_fcn (octave::command_history::set_file,
                 octave::command_history::file ());

  int nargin = args.length ();

  // Number of history lines to show (-1 = all)
  int limit = -1;

  for (octave_idx_type i = 0; i < nargin; i++)
    {
      octave_value arg = args(i);

      std::string option;

      if (arg.is_string ())
        option = arg.string_value ();
      else if (arg.isnumeric ())
        {
          limit = arg.int_value ();
          if (limit < 0)
            limit = -limit;
          continue;
        }
      else
        err_wrong_type_arg ("history", arg);

      if (option == "-r" || option == "-w" || option == "-a"
          || option == "-n")
        {
          if (i < nargin - 1)
            {
              std::string fname
                = args(++i).xstring_value ("history: filename must be a string for %s option",
                                           option.c_str ());

              octave::command_history::set_file (fname);
            }
          else
            octave::command_history::set_file (default_history_file ());

          if (option == "-a")
            // Append 'new' lines to file.
            octave::command_history::append ();

          else if (option == "-w")
            // Write entire history.
            octave::command_history::write ();

          else if (option == "-r")
            {
              // Read entire file.
              octave::command_history::read ();
              octave_link::set_history (octave::command_history::list ());
            }

          else if (option == "-n")
            {
              // Read 'new' history from file.
              octave::command_history::read_range ();
              octave_link::set_history (octave::command_history::list ());
            }

          else
            panic_impossible ();

          return hlist;
        }
      else if (option == "-c")
        {
          octave::command_history::clear ();
          octave_link::clear_history ();
        }
      else if (option == "-q")
        numbered_output = false;
      else if (option == "--")
        {
          i++;
          break;
        }
      else
        {
          // The last argument found in the command list that looks like
          // an integer will be used
          int tmp;

          if (sscanf (option.c_str (), "%d", &tmp) == 1)
            {
              if (tmp > 0)
                limit = tmp;
              else
                limit = -tmp;
            }

          else
            {
              if (option.length () > 0 && option[0] == '-')
                error ("history: unrecognized option '%s'", option.c_str ());
              else
                error ("history: bad non-numeric arg '%s'", option.c_str ());
            }
        }
    }

  hlist = octave::command_history::list (limit, numbered_output);

  int len = hlist.numel ();

  if (nargout == 0)
    {
      for (octave_idx_type i = 0; i < len; i++)
        octave_stdout << hlist[i] << "\n";
    }

  return hlist;
}

// Read the edited history lines from STREAM and return them
// one at a time.  This can read unlimited length lines.  The
// caller should free the storage.

static char *
edit_history_readline (std::fstream& stream)
{
  char c;
  int line_len = 128;
  int lindex = 0;
  char *line = new char [line_len];
  line[0] = '\0';

  while (stream.get (c))
    {
      if (lindex + 2 >= line_len)
        {
          char *tmp_line = new char [line_len += 128];
          strcpy (tmp_line, line);
          delete [] line;
          line = tmp_line;
        }

      if (c == '\n')
        {
          line[lindex++] = '\n';
          line[lindex++] = '\0';
          return line;
        }
      else
        line[lindex++] = c;
    }

  if (! lindex)
    {
      delete [] line;
      return 0;
    }

  if (lindex + 2 >= line_len)
    {
      char *tmp_line = new char [lindex+3];
      strcpy (tmp_line, line);
      delete [] line;
      line = tmp_line;
    }

  // Finish with newline if none in file.

  line[lindex++] = '\n';
  line[lindex++] = '\0';
  return line;
}

static void
edit_history_add_hist (const std::string& line)
{
  if (! line.empty ())
    {
      std::string tmp = line;

      int len = tmp.length ();

      if (len > 0 && tmp[len-1] == '\n')
        tmp.resize (len - 1);

      if (! tmp.empty ())
        if (octave::command_history::add (tmp))
          octave_link::append_history (tmp);
    }
}

static bool
get_int_arg (const octave_value& arg, int& val)
{
  bool ok = true;

  if (arg.is_string ())
    {
      std::string tmp = arg.string_value ();

      ok = sscanf (tmp.c_str (), "%d", &val) == 1;
    }
  else if (arg.isnumeric ())
    val = arg.int_value ();
  else
    ok = false;

  return ok;
}

static std::string
mk_tmp_hist_file (const octave_value_list& args,
                  bool insert_curr, const char *warn_for)
{
  string_vector hlist = octave::command_history::list ();

  int hist_count = hlist.numel () - 1;  // switch to zero-based indexing

  // The current command line is already part of the history list by
  // the time we get to this point.  Delete the cmd from the list when
  // executing 'edit_history' so that it doesn't show up in the history
  // but the actual commands performed will.

  if (! insert_curr)
    octave::command_history::remove (hist_count);

  hist_count--;  // skip last entry in history list

  // If no numbers have been specified, the default is to edit the
  // last command in the history list.

  int hist_beg = hist_count;
  int hist_end = hist_count;

  bool reverse = false;

  // Process options.

  int nargin = args.length ();

  if (nargin == 2)
    {
      if (! get_int_arg (args(0), hist_beg)
          || ! get_int_arg (args(1), hist_end))
        error ("%s: arguments must be integers", warn_for);

      if (hist_beg < 0)
        hist_beg += (hist_count + 1);
      else
        hist_beg--;
      if (hist_end < 0)
        hist_end += (hist_count + 1);
      else
        hist_end--;
    }
  else if (nargin == 1)
    {
      if (! get_int_arg (args(0), hist_beg))
        error ("%s: argument must be an integer", warn_for);

      if (hist_beg < 0)
        hist_beg += (hist_count + 1);
      else
        hist_beg--;

      hist_end = hist_beg;
    }

  if (hist_beg > hist_count || hist_end > hist_count)
    error ("%s: history specification out of range", warn_for);

  if (hist_end < hist_beg)
    {
      std::swap (hist_end, hist_beg);
      reverse = true;
    }

  std::string name = octave::sys::tempnam ("", "oct-");

  std::fstream file (name.c_str (), std::ios::out);

  if (! file)
    error ("%s: couldn't open temporary file '%s'", warn_for,
           name.c_str ());

  if (reverse)
    {
      for (int i = hist_end; i >= hist_beg; i--)
        file << hlist[i] << "\n";
    }
  else
    {
      for (int i = hist_beg; i <= hist_end; i++)
        file << hlist[i] << "\n";
    }

  file.close ();

  return name;
}

static void
unlink_cleanup (const char *file)
{
  octave_unlink_wrapper (file);
}

static void
do_edit_history (octave::interpreter& interp, const octave_value_list& args)
{
  std::string name = mk_tmp_hist_file (args, false, "edit_history");

  if (name.empty ())
    return;

  // Call up our favorite editor on the file of commands.

  octave::environment& env = interp.get_environment ();
  std::string cmd = env.editor ();
  cmd.append (" \"" + name + "\"");

  // Ignore interrupts while we are off editing commands.  Should we
  // maybe avoid using system()?

  volatile octave::interrupt_handler old_interrupt_handler
    = octave::ignore_interrupts ();

  int status = system (cmd.c_str ());

  octave::set_interrupt_handler (old_interrupt_handler);

  // Check if text edition was successfull.  Abort the operation
  // in case of failure.
  if (status != EXIT_SUCCESS)
    error ("edit_history: text editor command failed");

  // Write the commands to the history file since source_file
  // disables command line history while it executes.

  std::fstream file (name.c_str (), std::ios::in);

  char *line;
  //int first = 1;
  while ((line = edit_history_readline (file)) != 0)
    {
      // Skip blank lines.

      if (line[0] == '\n')
        {
          delete [] line;
          continue;
        }

      edit_history_add_hist (line);

      delete [] line;
    }

  file.close ();

  octave::unwind_protect frame;

  frame.add_fcn (unlink_cleanup, name.c_str ());
  frame.protect_var (input_from_tmp_history_file);

  input_from_tmp_history_file = true;

  // FIXME: instead of sourcing a file, we should just iterate through
  // the list of commands, parsing and executing them one at a time as
  // if they were entered interactively.

  octave::source_file (name);
}

static void
do_run_history (const octave_value_list& args)
{
  std::string name = mk_tmp_hist_file (args, false, "run_history");

  if (name.empty ())
    return;

  octave::unwind_protect frame;

  frame.add_fcn (unlink_cleanup, name.c_str ());
  frame.protect_var (input_from_tmp_history_file);

  input_from_tmp_history_file = true;

  // FIXME: instead of sourcing a file, we should just iterate through
  // the list of commands, parsing and executing them one at a time as
  // if they were entered interactively.

  octave::source_file (name);
}

void
initialize_history (bool read_history_file)
{
  octave::command_history::initialize (read_history_file,
                                       default_history_file (),
                                       default_history_size (),
                                       octave::sys::env::getenv ("OCTAVE_HISTCONTROL"));

  octave_link::set_history (octave::command_history::list ());
}

void
octave_history_write_timestamp (void)
{
  octave::sys::localtime now;

  std::string timestamp = now.strftime (Vhistory_timestamp_format_string);

  if (! timestamp.empty ())
    if (octave::command_history::add (timestamp))
      octave_link::append_history (timestamp);
}

DEFMETHOD (edit_history, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} edit_history
@deftypefnx {} {} edit_history @var{cmd_number}
@deftypefnx {} {} edit_history @var{first} @var{last}
Edit the history list using the editor named by the variable @env{EDITOR}.

The commands to be edited are first copied to a temporary file.  When you
exit the editor, Octave executes the commands that remain in the file.  It
is often more convenient to use @code{edit_history} to define functions
rather than attempting to enter them directly on the command line.
The block of commands is executed as soon as you exit the editor.
To avoid executing any commands, simply delete all the lines from the buffer
before leaving the editor.

When invoked with no arguments, edit the previously executed command;
With one argument, edit the specified command @var{cmd_number};
With two arguments, edit the list of commands between @var{first} and
@var{last}.  Command number specifiers may also be negative where -1
refers to the most recently executed command.
The following are equivalent and edit the most recently executed command.

@example
@group
edit_history
edit_history -1
@end group
@end example

When using ranges, specifying a larger number for the first command than the
last command reverses the list of commands before they are placed in the
buffer to be edited.
@seealso{run_history, history}
@end deftypefn */)
{
  // FIXME: should this be limited to the top-level context?

  if (args.length () > 2)
    print_usage ();

  do_edit_history (interp, args);

  return ovl ();
}

DEFUN (history, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} history
@deftypefnx {} {} history @var{opt1} @dots{}
@deftypefnx {} {@var{h} =} history ()
@deftypefnx {} {@var{h} =} history (@var{opt1}, @dots{})
If invoked with no arguments, @code{history} displays a list of commands
that you have executed.

Valid options are:

@table @code
@item   @var{n}
@itemx -@var{n}
Display only the most recent @var{n} lines of history.

@item -c
Clear the history list.

@item -q
Don't number the displayed lines of history.  This is useful for cutting
and pasting commands using the X Window System.

@item -r @var{file}
Read the file @var{file}, appending its contents to the current
history list.  If the name is omitted, use the default history file
(normally @file{~/.octave_hist}).

@item -w @var{file}
Write the current history to the file @var{file}.  If the name is
omitted, use the default history file (normally @file{~/.octave_hist}).
@end table

For example, to display the five most recent commands that you have
typed without displaying line numbers, use the command
@kbd{history -q 5}.

If invoked with a single output argument, the history will be saved to that
argument as a cell string and will not be output to screen.
@seealso{edit_history, run_history}
@end deftypefn */)
{
  // FIXME: should this be limited to the top-level context?

  // Call do_history even if nargout is zero to display history list.

  string_vector hlist = do_history (args, nargout);

  return nargout > 0 ? ovl (Cell (hlist)) : ovl ();
}

DEFUN (run_history, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} run_history
@deftypefnx {} {} run_history @var{cmd_number}
@deftypefnx {} {} run_history @var{first} @var{last}
Run commands from the history list.

When invoked with no arguments, run the previously executed command;

With one argument, run the specified command @var{cmd_number};

With two arguments, run the list of commands between @var{first} and
@var{last}.  Command number specifiers may also be negative where -1
refers to the most recently executed command.  For example, the command

@example
@group
run_history
     OR
run_history -1
@end group
@end example

@noindent
executes the most recent command again.
The command

@example
run_history 13 169
@end example

@noindent
executes commands 13 through 169.

Specifying a larger number for the first command than the last command
reverses the list of commands before executing them.
For example:

@example
@group
disp (1)
disp (2)
run_history -1 -2
@result{}
 2
 1
@end group
@end example

@seealso{edit_history, history}
@end deftypefn */)
{
  // FIXME: should this be limited to the top-level context?

  if (args.length () > 2)
    print_usage ();

  do_run_history (args);

  return ovl ();
}

DEFUN (history_control, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} history_control ()
@deftypefnx {} {@var{old_val} =} history_control (@var{new_val})
Query or set the internal variable that specifies how commands are saved
to the history list.

The default value is an empty character string, but may be overridden by the
environment variable @w{@env{OCTAVE_HISTCONTROL}}.

The value of @code{history_control} is a colon-separated list of values
controlling how commands are saved on the history list.  If the list
of values includes @code{ignorespace}, lines which begin with a space
character are not saved in the history list.  A value of @code{ignoredups}
causes lines matching the previous history entry to not be saved.
A value of @code{ignoreboth} is shorthand for @code{ignorespace} and
@code{ignoredups}.  A value of @code{erasedups} causes all previous lines
matching the current line to be removed from the history list before that
line is saved.  Any value not in the above list is ignored.  If
@code{history_control} is the empty string, all commands are saved on
the history list, subject to the value of @code{history_save}.
@seealso{history_file, history_size, history_timestamp_format_string, history_save}
@end deftypefn */)
{
  octave_value retval;

  std::string old_history_control = octave::command_history::histcontrol ();

  std::string tmp = old_history_control;

  retval = set_internal_variable (tmp, args, nargout, "history_control");

  if (tmp != old_history_control)
    octave::command_history::process_histcontrol (tmp);

  return retval;
}

DEFUN (history_size, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} history_size ()
@deftypefnx {} {@var{old_val} =} history_size (@var{new_val})
Query or set the internal variable that specifies how many entries
to store in the history file.

The default value is @code{1000}, but may be overridden by the environment
variable @w{@env{OCTAVE_HISTSIZE}}.
@seealso{history_file, history_timestamp_format_string, history_save}
@end deftypefn */)
{
  octave_value retval;

  int old_history_size = octave::command_history::size ();

  int tmp = old_history_size;

  retval = set_internal_variable (tmp, args, nargout,
                                  "history_size", -1,
                                  std::numeric_limits<int>::max ());

  if (tmp != old_history_size)
    octave::command_history::set_size (tmp);

  return retval;
}

DEFUN (history_file, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} history_file ()
@deftypefnx {} {@var{old_val} =} history_file (@var{new_val})
Query or set the internal variable that specifies the name of the
file used to store command history.

The default value is @file{~/.octave_hist}, but may be overridden by the
environment variable @w{@env{OCTAVE_HISTFILE}}.
@seealso{history_size, history_save, history_timestamp_format_string}
@end deftypefn */)
{
  octave_value retval;

  std::string old_history_file = octave::command_history::file ();

  std::string tmp = old_history_file;

  retval = set_internal_variable (tmp, args, nargout, "history_file");

  if (tmp != old_history_file)
    octave::command_history::set_file (tmp);

  return retval;
}

DEFUN (history_timestamp_format_string, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} history_timestamp_format_string ()
@deftypefnx {} {@var{old_val} =} history_timestamp_format_string (@var{new_val})
@deftypefnx {} {} history_timestamp_format_string (@var{new_val}, "local")
Query or set the internal variable that specifies the format string
for the comment line that is written to the history file when Octave
exits.

The format string is passed to @code{strftime}.  The default value is

@example
"# Octave VERSION, %a %b %d %H:%M:%S %Y %Z <USER@@HOST>"
@end example

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{strftime, history_file, history_size, history_save}
@end deftypefn */)
{
  return SET_INTERNAL_VARIABLE (history_timestamp_format_string);
}

DEFUN (history_save, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} history_save ()
@deftypefnx {} {@var{old_val} =} history_save (@var{new_val})
@deftypefnx {} {} history_save (@var{new_val}, "local")
Query or set the internal variable that controls whether commands entered
on the command line are saved in the history file.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{history_control, history_file, history_size, history_timestamp_format_string}
@end deftypefn */)
{
  octave_value retval;

  bool old_history_save = ! octave::command_history::ignoring_entries ();

  bool tmp = old_history_save;

  retval = set_internal_variable (tmp, args, nargout, "history_save");

  if (tmp != old_history_save)
    octave::command_history::ignore_entries (! tmp);

  return retval;
}
