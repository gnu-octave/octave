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

// Thomas Baier <baier@ci.tuwien.ac.at> added the original versions of
// the following functions:
//
//   mkfifo  unlink  waitpid

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstdio>
#include <cstring>

#include "cmd-hist.h"
#include "fcntl-wrappers.h"
#include "file-ops.h"
#include "file-stat.h"
#include "lo-utils.h"
#include "oct-env.h"
#include "oct-syscalls.h"
#include "oct-uname.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "event-manager.h"
#include "input.h"
#include "interpreter.h"
#include "oct-hist.h"
#include "oct-map.h"
#include "oct-stdstrm.h"
#include "oct-stream.h"
#include "ovl.h"
#include "sysdep.h"
#include "utils.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static octave_scalar_map
mk_stat_map (const sys::base_file_stat& fs)
{
  static bool have_rdev
    = sys::base_file_stat::have_struct_stat_st_rdev ();
  static bool have_blksize
    = sys::base_file_stat::have_struct_stat_st_blksize ();
  static bool have_blocks
    = sys::base_file_stat::have_struct_stat_st_blocks ();

  static double nan = numeric_limits<double>::NaN ();

  octave_scalar_map m;

  m.assign ("dev", static_cast<double> (fs.dev ()));
  m.assign ("ino", fs.ino ());
  m.assign ("mode", fs.mode ());
  m.assign ("modestr", fs.mode_as_string ());
  m.assign ("nlink", fs.nlink ());
  m.assign ("uid", fs.uid ());
  m.assign ("gid", fs.gid ());
  m.assign ("rdev", have_rdev ? static_cast<double> (fs.rdev ()) : nan);
  m.assign ("size", fs.size ());
  m.assign ("atime", fs.atime ());
  m.assign ("mtime", fs.mtime ());
  m.assign ("ctime", fs.ctime ());

  if (have_blksize)
    m.assign ("blksize", fs.blksize ());
  else
    m.assign ("blksize", nan);

  if (have_blocks)
    m.assign ("blocks", fs.blocks ());
  else
    m.assign ("blocks", nan);

  return m;
}

static octave_value_list
mk_stat_result (const sys::base_file_stat& fs)
{
  if (fs)
    return ovl (octave_value (mk_stat_map (fs)), 0, "");
  else
    return ovl (Matrix (), -1, fs.error ());
}

DEFMETHODX ("dup2", Fdup2, interp, args, ,
            doc: /* -*- texinfo -*-
@deftypefn {} {[@var{fid}, @var{msg}] =} dup2 (@var{old}, @var{new})
Duplicate a file descriptor.

If successful, @var{fid} is greater than zero and contains the new file ID@.
Otherwise, @var{fid} is negative and @var{msg} contains a system-dependent
error message.
@seealso{fopen, fclose, fcntl}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  stream_list& streams = interp.get_stream_list ();

  stream old_stream = streams.lookup (args(0), "dup2");

  stream new_stream = streams.lookup (args(1), "dup2");

  int i_old = old_stream.file_number ();
  int i_new = new_stream.file_number ();

  if (i_old >= 0 && i_new >= 0)
    {
      std::string msg;

      int status = sys::dup2 (i_old, i_new, msg);

      return ovl (status, msg);
    }
  else
    return ovl (-1, "");
}

DEFMETHODX ("exec", Fexec, interp, args, ,
            doc: /* -*- texinfo -*-
@deftypefn {} {[@var{err}, @var{msg}] =} exec (@var{file}, @var{args})
Replace current process with a new process.

Calling @code{exec} without first calling @code{fork} will terminate your
current Octave process and replace it with the program named by @var{file}.
For example,

@example
exec ("ls", "-l")
@end example

@noindent
will run @code{ls} and return you to your shell prompt.

If successful, @code{exec} does not return.  If @code{exec} does return,
@var{err} will be nonzero, and @var{msg} will contain a system-dependent
error message.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  std::string exec_file = args(0).xstring_value ("exec: FILE must be a string");

  string_vector exec_args;

  if (nargin == 2)
    {
      string_vector tmp = args(1).xstring_vector_value ("exec: all arguments must be strings");

      int len = tmp.numel ();

      exec_args.resize (len + 1);

      exec_args[0] = exec_file;

      for (int i = 0; i < len; i++)
        exec_args[i+1] = tmp[i];
    }
  else
    {
      exec_args.resize (1);

      exec_args[0] = exec_file;
    }

  history_system& history_sys = interp.get_history_system ();

  history_sys.write_timestamp ();

  if (! command_history::ignoring_entries ())
    command_history::clean_up_and_save ();

  std::string msg;

  int status = sys::execvp (exec_file, exec_args, msg);

  return ovl (status, msg);
}

DEFMETHODX ("popen2", Fpopen2, interp, args, ,
            doc: /* -*- texinfo -*-
@deftypefn {} {[@var{in}, @var{out}, @var{pid}] =} popen2 (@var{command}, @var{args})
Start a subprocess with two-way communication.

The name of the process is given by @var{command}, and @var{args} is an
array or cell array of strings containing options for the command.

The file identifiers for the input and output streams of the subprocess are
returned in @var{in} and @var{out}.  If execution of the command is
successful, @var{pid} contains the process ID of the subprocess.  Otherwise,
@var{pid} is @minus{}1.

For example:

@example
[in, out, pid] = popen2 ("sort", "-r");
fputs (in, "these\nare\nsome\nstrings\n");
fclose (in);
EAGAIN = errno ("EAGAIN");
done = false;
do
  s = fgets (out);
  if (ischar (s))
    fputs (stdout, s);
  elseif (errno () == EAGAIN)
    pause (0.1);
    fclear (out);
  else
    done = true;
  endif
until (done)
fclose (out);
waitpid (pid);

   @print{} these
   @print{} strings
   @print{} some
   @print{} are
@end example

Note that @code{popen2}, unlike @code{popen}, will not @nospell{"reap"}
the child process.  If you don't use @code{waitpid} to check the child's
exit status, it will linger until Octave exits.
@seealso{popen, waitpid}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    print_usage ();

  std::string exec_file = args(0).xstring_value ("popen2: COMMAND argument must be a string");

  string_vector arg_list;

  if (nargin >= 2)
    {
      string_vector tmp = args(1).xstring_vector_value ("popen2: all arguments must be strings");

      int len = tmp.numel ();

      arg_list.resize (len + 1);

      arg_list[0] = exec_file;

      for (int i = 0; i < len; i++)
        arg_list[i+1] = tmp[i];
    }
  else
    {
      arg_list.resize (1);

      arg_list[0] = exec_file;
    }

  bool sync_mode = (nargin == 3 ? args(2).bool_value () : false);

  int filedesc[2];
  std::string msg;
  pid_t pid;

  pid = sys::popen2 (exec_file, arg_list, sync_mode, filedesc, msg);

  if (pid < 0)
    error ("%s", msg.c_str ());

  FILE *ifile = fdopen (filedesc[1], "r");
  FILE *ofile = fdopen (filedesc[0], "w");

  stream is = stdiostream::create (exec_file + "-in", ifile, std::ios::in);

  stream os = stdiostream::create (exec_file + "-out", ofile, std::ios::out);

  stream_list& streams = interp.get_stream_list ();

  return ovl (streams.insert (os), streams.insert (is), pid);
}

/*

%!test  # UNIX-style test
%! if (isunix () || ismac ())
%!   [in, out, pid] = popen2 ("sort", "-r");
%!   EAGAIN = errno ("EAGAIN");
%!   fputs (in, "these\nare\nsome\nstrings\n");
%!   fclose (in);
%!   done = false;
%!   str = {};
%!   idx = 0;
%!   errs = 0;
%!   do
%!     if (ismac ())  # FIXME: Is this necessary?
%!       errno (0);
%!     endif
%!     s = fgets (out);
%!     if (ischar (s))
%!       idx++;
%!       str{idx} = s;
%!     elseif (errno () == EAGAIN)
%!       fclear (out);
%!       pause (0.1);
%!       if (++errs == 100)
%!         done = true;
%!       endif
%!     else
%!       done = true;
%!     endif
%!   until (done)
%!   fclose (out);
%!   waitpid (pid);
%!   assert (str, {"these\n","strings\n","some\n","are\n"});
%! endif

%!test  # Windows-style test
%! if (ispc () && ! isunix ())
%!   [in, out, pid] = popen2 ('C:\Windows\system32\sort.exe', "/R");
%!   EAGAIN = errno ("EINVAL");
%!   fputs (in, "these\r\nare\r\nsome\r\nstrings\r\n");
%!   fclose (in);
%!   done = false;
%!   str = {};
%!   idx = 0;
%!   errs = 0;
%!   do
%!     errno (0);
%!     s = fgets (out);
%!     if (ischar (s))
%!       idx++;
%!       str{idx} = s;
%!     elseif (errno () == EAGAIN)
%!       fclear (out);
%!       pause (0.1);
%!       if (++errs == 100)
%!         done = true;
%!       endif
%!     else
%!       done = true;
%!     endif
%!   until (done)
%!   fclose (out);
%!   waitpid (pid);
%!   assert (str, {"these\r\n","strings\r\n","some\r\n","are\r\n"});
%! endif

*/

DEFMETHODX ("fcntl", Ffcntl, interp, args, nargout,
            doc: /* -*- texinfo -*-
@deftypefn  {} {} fcntl (@var{fid}, @var{request}, @var{arg})
@deftypefnx {} {[@var{status}, @var{msg}] =} fcntl (@var{fid}, @var{request}, @var{arg})
Change the properties of the open file @var{fid}.

The following values may be passed as @var{request}:

@vtable @code
@item F_DUPFD
Return a duplicate file descriptor.

@item F_GETFD
Return the file descriptor flags for @var{fid}.

@item F_SETFD
Set the file descriptor flags for @var{fid}.

@item F_GETFL
Return the file status flags for @var{fid}.  The following codes may be
returned (some of the flags may be undefined on some systems).

@vtable @code
@item O_RDONLY
Open for reading only.

@item O_WRONLY
Open for writing only.

@item O_RDWR
Open for reading and writing.

@item O_APPEND
Append on each write.

@item O_CREAT
Create the file if it does not exist.

@item O_NONBLOCK
Non-blocking mode.

@item O_SYNC
Wait for writes to complete.

@item O_ASYNC
Asynchronous I/O.
@end vtable

@item F_SETFL
Set the file status flags for @var{fid} to the value specified by @var{arg}.
 The only flags that can be changed are @w{@code{O_APPEND}} and
@w{@code{O_NONBLOCK}}.
@end vtable

If successful, @var{status} is 0 and @var{msg} is an empty string.  Otherwise,
@var{status} is -1 and @var{msg} contains a system-dependent error
message.
@seealso{fopen, dup2}
@end deftypefn */)
{
  if (args.length () != 3)
    print_usage ();

  stream_list& streams = interp.get_stream_list ();

  stream strm = streams.lookup (args(0), "fcntl");

  int fid = strm.file_number ();

  // FIXME: Do we want to use xint_value and throw a warning message
  //        if input validation fails?
  int req = args(1).int_value (true);
  int arg = args(2).int_value (true);

  // FIXME: Need better checking here?
  if (fid < 0)
    error ("fcntl: invalid file id");

  octave_value_list retval;
  std::string msg;

  int status = sys::fcntl (fid, req, arg, msg);

  if (nargout == 0)
    {
      if (status < 0)
        error ("fcntl: operation failed: %s", msg.c_str ());
    }
  else
    {
      if (status < 0)
        retval = ovl (-1.0, msg);
      else
        retval = ovl (0.0, "");
    }

  return retval;
}

DEFMETHODX ("fork", Ffork, interp, args, ,
            doc: /* -*- texinfo -*-
@deftypefn {} {[@var{pid}, @var{msg}] =} fork ()
Create a copy of the current process.

Fork can return one of the following values:

@table @asis
@item > 0
You are in the parent process.  The value returned from @code{fork} is the
process id of the child process.  You should probably arrange to wait for
any child processes to exit.

@item 0
You are in the child process.  You can call @code{exec} to start another
process.  If that fails, you should probably call @code{exit}.

@item < 0
The call to @code{fork} failed for some reason.  You must take evasive
action.  A system dependent error message will be waiting in @var{msg}.
@end table
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  if (interp.at_top_level ())
    error ("fork: cannot be called from command line");

  std::string msg;

  pid_t pid = sys::fork (msg);

  return ovl (pid, msg);
}

DEFUNX ("getpgrp", Fgetpgrp, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {pgid =} getpgrp ()
Return the process group id of the current process.
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  std::string msg;

  pid_t pid = sys::getpgrp (msg);

  return ovl (pid, msg);
}

DEFUNX ("getpid", Fgetpid, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {pid =} getpid ()
Return the process id of the current process.
@seealso{getppid}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (sys::getpid ());
}

DEFUNX ("getppid", Fgetppid, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {pid =} getppid ()
Return the process id of the parent process.
@seealso{getpid}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (sys::getppid ());
}

DEFUNX ("getegid", Fgetegid, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {egid =} getegid ()
Return the effective group id of the current process.
@seealso{getgid}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (sys::getegid ());
}

DEFUNX ("getgid", Fgetgid, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {gid =} getgid ()
Return the real group id of the current process.
@seealso{getegid}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (sys::getgid ());
}

DEFUNX ("geteuid", Fgeteuid, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {euid =} geteuid ()
Return the effective user id of the current process.
@seealso{getuid}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (sys::geteuid ());
}

DEFUNX ("getuid", Fgetuid, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {uid =} getuid ()
Return the real user id of the current process.
@seealso{geteuid}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (sys::getuid ());
}

DEFUNX ("kill", Fkill, args, nargout,
        doc: /* -*- texinfo -*-
@deftypefn  {} {} kill (@var{pid}, @var{sig})
@deftypefnx {} {[@var{status}, @var{msg}] =} kill (@var{pid}, @var{sig})
Send signal @var{sig} to process @var{pid}.

If @var{pid} is positive, then signal @var{sig} is sent to @var{pid}.

If @var{pid} is 0, then signal @var{sig} is sent to every process in the
process group of the current process.

If @var{pid} is -1, then signal @var{sig} is sent to every process except
process 1.

If @var{pid} is less than -1, then signal @var{sig} is sent to every process in
the process group @var{-pid}.

If @var{sig} is 0, then no signal is sent, but error checking is still
performed.

If successful, @var{status} is 0 and @var{msg} is an empty string.
Otherwise, @var{status} is -1 and @var{msg} contains a system-dependent
error message.
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  pid_t pid = args(0).int_value (true);

  int sig = args(1).int_value (true);

  octave_value_list retval;
  std::string msg;

  int status = sys::kill (pid, sig, msg);

  if (nargout == 0)
    {
      if (status < 0)
        error ("kill: operation failed: %s", msg.c_str ());
    }
  else
    {
      if (status < 0)
        retval = ovl (-1.0, msg);
      else
        retval = ovl (0.0, "");
    }

  return retval;
}

DEFUNX ("lstat", Flstat, args, ,
        doc: /* -*- texinfo -*-
@deftypefn  {} {@var{info} =} lstat (@var{symlink})
@deftypefnx {} {[@var{info}, @var{err}, @var{msg}] =} lstat (@var{symlink})
Return a structure @var{info} containing information about the symbolic link
@var{symlink}.

The function outputs are described in the documentation for @code{stat}.
@seealso{stat, symlink}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  std::string fname = args(0).xstring_value ("lstat: NAME must be a string");

  sys::file_stat fs (fname, false);

  return mk_stat_result (fs);
}

// FIXME: This routine also exists verbatim in file-io.cc.
//        Maybe change to be a general utility routine.
static int
convert (int x, int ibase, int obase)
{
  int retval = 0;

  int tmp = x % obase;

  if (tmp > ibase - 1)
    error ("mkfifo: invalid digit");

  retval = tmp;
  int mult = ibase;
  while ((x = (x - tmp) / obase))
    {
      tmp = x % obase;

      if (tmp > ibase - 1)
        error ("mkfifo: invalid digit");

      retval += mult * tmp;
      mult *= ibase;
    }

  return retval;
}

DEFUNX ("mkfifo", Fmkfifo, args, nargout,
        doc: /* -*- texinfo -*-
@deftypefn  {} {} mkfifo (@var{name}, @var{mode})
@deftypefnx {} {[@var{status}, @var{msg}] =} mkfifo (@var{name}, @var{mode})
Create a FIFO special file named @var{name} with file mode @var{mode}.

@var{mode} is interpreted as an octal number and is subject to umask
processing.  The final calculated mode is @code{@var{mode} - @var{umask}}.

If successful, @var{status} is 0 and @var{msg} is an empty string.
Otherwise, @var{status} is -1 and @var{msg} contains a system-dependent
error message.
@seealso{pipe, umask}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  std::string name = args(0).xstring_value ("mkfifo: FILE must be a string");

  int octal_mode = args(1).xint_value ("mkfifo: MODE must be an integer");

  if (octal_mode < 0)
    error ("mkfifo: MODE must be a positive integer value");

  int mode = convert (octal_mode, 8, 10);

  octave_value_list retval;
  std::string msg;

  int status = sys::mkfifo (name, mode, msg);

  if (nargout == 0)
    {
      if (status < 0)
        error ("mkfifo: operation failed: %s", msg.c_str ());
    }
  else
    {
      if (status < 0)
        retval = ovl (-1.0, msg);
      else
        retval = ovl (0.0, "");
    }

  return retval;
}

/*

## Test input validation
%!error mkfifo ()
%!error mkfifo ("abc")
%!error mkfifo ("abc", 777, 123)
%!error <FILE must be a string> mkfifo (123, 456)
## FIXME: These tests should work, but lasterr is not being set correctly.
#%!error <MODE must be an integer> mkfifo ("abc", {456})
#%!error <MODE must be a positive integer value> mkfifo ("abc", -1)

*/

DEFMETHODX ("pipe", Fpipe, interp, args, ,
            doc: /* -*- texinfo -*-
@deftypefn {} {[@var{read_fd}, @var{write_fd}, @var{err}, @var{msg}] =} pipe ()
Create a pipe and return the reading and writing ends of the pipe into
@var{read_fd} and @var{write_fd} respectively.

If successful, @var{err} is 0 and @var{msg} is an empty string.
Otherwise, @var{err} is nonzero and @var{msg} contains a system-dependent
error message.
@seealso{mkfifo}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  int fid[2];
  std::string msg;

  int status = sys::pipe (fid, msg);

  if (status < 0)
    return ovl (-1, -1, -1, msg);
  else
    {
      FILE *ifile = fdopen (fid[0], "r");
      FILE *ofile = fdopen (fid[1], "w");

      stream is = stdiostream::create ("pipe-in", ifile, std::ios::in);

      stream os = stdiostream::create ("pipe-out", ofile, std::ios::out);

      stream_list& streams = interp.get_stream_list ();

      return ovl (streams.insert (is), streams.insert (os), status, msg);
    }
}

DEFMETHODX ("stat", Fstat, interp, args, ,
            doc: /* -*- texinfo -*-
@deftypefn  {} {[@var{info}, @var{err}, @var{msg}] =} stat (@var{file})
@deftypefnx {} {[@var{info}, @var{err}, @var{msg}] =} stat (@var{fid})
@deftypefnx {} {[@var{info}, @var{err}, @var{msg}] =} lstat (@var{file})
@deftypefnx {} {[@var{info}, @var{err}, @var{msg}] =} lstat (@var{fid})
Return a structure @var{info} containing the following information about
@var{file} or file identifier @var{fid}.

@table @code
@item dev
ID of device containing a directory entry for this file.

@item ino
File number of the file.

@item mode
File mode, as an integer.  Use the functions @w{@code{S_ISREG}},
@w{@code{S_ISDIR}}, @w{@code{S_ISCHR}}, @w{@code{S_ISBLK}},
@w{@code{S_ISFIFO}}, @w{@code{S_ISLNK}}, or @w{@code{S_ISSOCK}} to extract
information from this value.

@item modestr
File mode, as a string of ten letters or dashes as would be returned by
@kbd{ls -l}.

@item nlink
Number of links.

@item uid
User ID of file's owner.

@item gid
Group ID of file's group.

@item rdev
ID of device for block or character special files.

@item size
Size in bytes.

@item atime
Time of last access in the same form as time values returned from
@code{time}.  @xref{Timing Utilities}.

@item mtime
Time of last modification in the same form as time values returned from
@code{time}.  @xref{Timing Utilities}.

@item ctime
Time of last file status change in the same form as time values
returned from @code{time}.  @xref{Timing Utilities}.

@item blksize
Size of blocks in the file.

@item blocks
Number of blocks allocated for file.
@end table

If the call is successful @var{err} is 0 and @var{msg} is an empty string.
If the file does not exist, or some other error occurs, @var{info} is an
empty matrix, @var{err} is @minus{}1, and @var{msg} contains the
corresponding system error message.

If @var{file} is a symbolic link, @code{stat} will return information about
the actual file that is referenced by the link.  Use @code{lstat} if you
want information about the symbolic link itself.

For example:

@example
[info, err, msg] = stat ("/vmlinuz")
  @result{} info =
     @{
       atime = 855399756
       rdev = 0
       ctime = 847219094
       uid = 0
       size = 389218
       blksize = 4096
       mtime = 847219094
       gid = 6
       nlink = 1
       blocks = 768
       mode = -rw-r--r--
       modestr = -rw-r--r--
       ino = 9316
       dev = 2049
     @}
  @result{} err = 0
  @result{} msg =
@end example
@seealso{lstat, ls, dir, isfile, isfolder}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_value_list retval;

  if (args(0).is_scalar_type ())
    {
      stream_list& streams = interp.get_stream_list ();

      int fid = streams.get_file_number (args(0));

      sys::file_fstat fs (fid);

      retval = mk_stat_result (fs);
    }
  else
    {
      std::string fname = args(0).xstring_value ("stat: NAME must be a string");

      sys::file_stat fs (fname);

      retval = mk_stat_result (fs);
    }

  return retval;
}

DEFUNX ("S_ISREG", FS_ISREG, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} S_ISREG (@var{mode})
Return true if @var{mode} corresponds to a regular file.

The value of @var{mode} is assumed to be returned from a call to
@code{stat}.
@seealso{stat, lstat}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  double mode = args(0).xdouble_value ("S_ISREG: invalid MODE value");

  return ovl (sys::file_stat::is_reg (static_cast<mode_t> (mode)));
}

DEFUNX ("S_ISDIR", FS_ISDIR, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} S_ISDIR (@var{mode})
Return true if @var{mode} corresponds to a directory.

The value of @var{mode} is assumed to be returned from a call to
@code{stat}.
@seealso{stat, lstat}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  double mode = args(0).xdouble_value ("S_ISDIR: invalid MODE value");

  return ovl (sys::file_stat::is_dir (static_cast<mode_t> (mode)));
}

DEFUNX ("S_ISCHR", FS_ISCHR, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} S_ISCHR (@var{mode})
Return true if @var{mode} corresponds to a character device.

The value of @var{mode} is assumed to be returned from a call to
@code{stat}.
@seealso{stat, lstat}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  double mode = args(0).xdouble_value ("S_ISCHR: invalid MODE value");

  return ovl (sys::file_stat::is_chr (static_cast<mode_t> (mode)));
}

DEFUNX ("S_ISBLK", FS_ISBLK, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} S_ISBLK (@var{mode})
Return true if @var{mode} corresponds to a block device.

The value of @var{mode} is assumed to be returned from a call to
@code{stat}.
@seealso{stat, lstat}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  double mode = args(0).xdouble_value ("S_ISBLK: invalid MODE value");

  return ovl (sys::file_stat::is_blk (static_cast<mode_t> (mode)));
}

DEFUNX ("S_ISFIFO", FS_ISFIFO, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} S_ISFIFO (@var{mode})
Return true if @var{mode} corresponds to a fifo.

The value of @var{mode} is assumed to be returned from a call to
@code{stat}.
@seealso{stat, lstat}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  double mode = args(0).xdouble_value ("S_ISFIFO: invalid MODE value");

  return ovl (sys::file_stat::is_fifo (static_cast<mode_t> (mode)));
}

DEFUNX ("S_ISLNK", FS_ISLNK, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} S_ISLNK (@var{mode})
Return true if @var{mode} corresponds to a symbolic link.

The value of @var{mode} is assumed to be returned from a call to
@code{stat}.
@seealso{stat, lstat}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  double mode = args(0).xdouble_value ("S_ISLNK: invalid MODE value");

  return ovl (sys::file_stat::is_lnk (static_cast<mode_t> (mode)));
}

DEFUNX ("S_ISSOCK", FS_ISSOCK, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} S_ISSOCK (@var{mode})
Return true if @var{mode} corresponds to a socket.

The value of @var{mode} is assumed to be returned from a call to
@code{stat}.
@seealso{stat, lstat}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  double mode = args(0).xdouble_value ("S_ISSOCK: invalid MODE value");

  return ovl (sys::file_stat::is_sock (static_cast<mode_t> (mode)));
}

DEFUN (gethostname, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{name} =} gethostname ()
Return the hostname of the system where Octave is running.
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (sys::env::get_host_name ());
}

DEFUN (uname, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{uts}, @var{err}, @var{msg}] =} uname ()
Return system information in the structure.

For example:

@example
@group
uname ()
   @result{} @{
         sysname = x86_64
         nodename = segfault
         release = 2.6.15-1-amd64-k8-smp
         version = Linux
         machine = #2 SMP Thu Feb 23 04:57:49 UTC 2006
      @}
@end group
@end example

If successful, @var{err} is 0 and @var{msg} is an empty string.
Otherwise, @var{err} is nonzero and @var{msg} contains a
system-dependent error message.
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  sys::uname sysinfo;

  octave_scalar_map m;

  m.assign ("sysname", sysinfo.sysname ());
  m.assign ("nodename", sysinfo.nodename ());
  m.assign ("release", sysinfo.release ());
  m.assign ("version", sysinfo.version ());
  m.assign ("machine", sysinfo.machine ());

  return ovl (m, sysinfo.error (), sysinfo.message ());
}

/*
%!test <*51869>
%! [info, status, msg] = uname ();
%! if (status == 0)
%!   assert (isstruct (info))
%!   assert (ischar (msg) && isempty (msg))
%! endif
*/

DEFMETHODX ("unlink", Funlink, interp, args, nargout,
            doc: /* -*- texinfo -*-
@deftypefn  {} {} unlink (@var{file})
@deftypefnx {} {[@var{status}, @var{msg}] =} unlink (@var{file})
Delete the file named @var{file}.

If successful, @var{status} is 0 and @var{msg} is an empty string.
Otherwise, @var{status} is -1 and @var{msg} contains a system-dependent
error message.
@seealso{delete, rmdir}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  std::string name = args(0).xstring_value ("unlink: FILE must be a string");

  octave_value_list retval;
  std::string msg;

  event_manager& evmgr = interp.get_event_manager ();

  evmgr.file_remove (name, "");

  int status = sys::unlink (name, msg);

  evmgr.file_renamed (status == 0);

  if (nargout == 0)
    {
      if (status < 0)
        error ("unlink: operation failed: %s", msg.c_str ());
    }
  else
    {
      if (status < 0)
        retval = ovl (-1.0, msg);
      else
        retval = ovl (0.0, "");
    }

  return retval;
}

/*
%!test
%! file = tempname ();
%! fid = fopen (file, "wt");
%! if (fid < 0)
%!   error ("Could not open temporary file for unlink BIST test");
%! endif
%! fdisp (fid, pi);
%! fclose (fid);
%! [status, msg] = unlink (file);
%! assert (status, 0);
%! assert (msg, "");

## Test input validation
%!error <Invalid call> unlink ()
%!error <Invalid call> unlink ("a", "b")
%!error <FILE must be a string> unlink (123)
*/

DEFUNX ("waitpid", Fwaitpid, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {[@var{pid}, @var{status}, @var{msg}] =} waitpid (@var{pid}, @var{options})
Wait for process @var{pid} to terminate.

The @var{pid} argument can be:

@table @asis
@item @minus{}1
Wait for any child process.

@item 0
Wait for any child process whose process group ID is equal to that of the
Octave interpreter process.

@item > 0
Wait for termination of the child process with ID @var{pid}.
@end table

The @var{options} argument can be a bitwise OR of zero or more of the
following constants:

@table @code
@item 0
Wait until signal is received or a child process exits (this is the default
if the @var{options} argument is missing).

@item WNOHANG
Do not hang if status is not immediately available.

@item WUNTRACED
Report the status of any child processes that are stopped, and whose status
has not yet been reported since they stopped.

@item WCONTINUE
Return if a stopped child has been resumed by delivery of @code{SIGCONT}.
This value may not be meaningful on all systems.
@end table

If the returned value of @var{pid} is greater than 0, it is the process ID
of the child process that exited.  If an error occurs, @var{pid} will be
less than zero and @var{msg} will contain a system-dependent error message.
The value of @var{status} contains additional system-dependent information
about the subprocess that exited.
@seealso{WCONTINUE, WCOREDUMP, WEXITSTATUS, WIFCONTINUED, WIFSIGNALED,
WIFSTOPPED, WNOHANG, WSTOPSIG, WTERMSIG, WUNTRACED}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin != 1 && nargin != 2)
    print_usage ();

  pid_t pid = args(0).xint_value ("waitpid: OPTIONS must be an integer");

  int options = 0;

  if (nargin == 2)
    options = args(1).xint_value ("waitpid: PID must be an integer value");

  std::string msg;
  int status;

  pid_t result = sys::waitpid (pid, &status, options, msg);

  return ovl (result, status, msg);
}

DEFUNX ("WIFEXITED", FWIFEXITED, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} WIFEXITED (@var{status})
Given @var{status} from a call to @code{waitpid}, return
true if the child terminated normally.
@seealso{waitpid, WEXITSTATUS, WIFSIGNALED, WTERMSIG, WCOREDUMP, WIFSTOPPED,
WSTOPSIG, WIFCONTINUED}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  int status = args(0).xint_value ("WIFEXITED: STATUS must be an integer");

  return ovl (sys::wifexited (status));
}

DEFUNX ("WEXITSTATUS", FWEXITSTATUS, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} WEXITSTATUS (@var{status})
Given @var{status} from a call to @code{waitpid}, return
the exit status of the child.

This function should only be employed if @code{WIFEXITED} returned true.
@seealso{waitpid, WIFEXITED, WIFSIGNALED, WTERMSIG, WCOREDUMP, WIFSTOPPED,
WSTOPSIG, WIFCONTINUED}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  int status = args(0).xint_value ("WEXITSTATUS: STATUS must be an integer");

  return ovl (sys::wexitstatus (status));
}

DEFUNX ("WIFSIGNALED", FWIFSIGNALED, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} WIFSIGNALED (@var{status})
Given @var{status} from a call to @code{waitpid}, return
true if the child process was terminated by a signal.
@seealso{waitpid, WIFEXITED, WEXITSTATUS, WTERMSIG, WCOREDUMP, WIFSTOPPED,
WSTOPSIG, WIFCONTINUED}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  int status = args(0).xint_value ("WIFSIGNALED: STATUS must be an integer");

  return ovl (sys::wifsignaled (status));
}

DEFUNX ("WTERMSIG", FWTERMSIG, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} WTERMSIG (@var{status})
Given @var{status} from a call to @code{waitpid}, return
the number of the signal that caused the child process to terminate.

This function should only be employed if @code{WIFSIGNALED} returned true.
@seealso{waitpid, WIFEXITED, WEXITSTATUS, WIFSIGNALED, WCOREDUMP, WIFSTOPPED,
WSTOPSIG, WIFCONTINUED}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  int status = args(0).xint_value ("WTERMSIG: STATUS must be an integer");

  return ovl (sys::wtermsig (status));
}

DEFUNX ("WCOREDUMP", FWCOREDUMP, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} WCOREDUMP (@var{status})
Given @var{status} from a call to @code{waitpid}, return
true if the child produced a core dump.

This function should only be employed if @code{WIFSIGNALED} returned true.
The macro used to implement this function is not specified in POSIX.1-2001
and is not available on some Unix implementations (e.g., @nospell{AIX, SunOS}).
@seealso{waitpid, WIFEXITED, WEXITSTATUS, WIFSIGNALED, WTERMSIG, WIFSTOPPED,
WSTOPSIG, WIFCONTINUED}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  int status = args(0).xint_value ("WCOREDUMP: STATUS must be an integer");

  return ovl (sys::wcoredump (status));
}

DEFUNX ("WIFSTOPPED", FWIFSTOPPED, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} WIFSTOPPED (@var{status})
Given @var{status} from a call to @code{waitpid}, return
true if the child process was stopped by delivery of a signal.

This is only possible if the call was done using @code{WUNTRACED} or when
the child is being traced (see ptrace(2)).
@seealso{waitpid, WIFEXITED, WEXITSTATUS, WIFSIGNALED, WTERMSIG, WCOREDUMP,
WSTOPSIG, WIFCONTINUED}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  int status = args(0).xint_value ("WIFSTOPPED: STATUS must be an integer");

  return ovl (sys::wifstopped (status));
}

DEFUNX ("WSTOPSIG", FWSTOPSIG, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} WSTOPSIG (@var{status})
Given @var{status} from a call to @code{waitpid}, return
the number of the signal which caused the child to stop.

This function should only be employed if @code{WIFSTOPPED} returned true.
@seealso{waitpid, WIFEXITED, WEXITSTATUS, WIFSIGNALED, WTERMSIG, WCOREDUMP,
WIFSTOPPED, WIFCONTINUED}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  int status = args(0).xint_value ("WSTOPSIG: STATUS must be an integer");

  return ovl (sys::wstopsig (status));
}

DEFUNX ("WIFCONTINUED", FWIFCONTINUED, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} WIFCONTINUED (@var{status})
Given @var{status} from a call to @code{waitpid}, return
true if the child process was resumed by delivery of @code{SIGCONT}.
@seealso{waitpid, WIFEXITED, WEXITSTATUS, WIFSIGNALED, WTERMSIG, WCOREDUMP,
WIFSTOPPED, WSTOPSIG}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  int status = args(0).xint_value ("WIFCONTINUED: STATUS must be an integer");

  return ovl (sys::wifcontinued (status));
}

DEFUNX ("canonicalize_file_name", Fcanonicalize_file_name, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {[@var{cname}, @var{status}, @var{msg}] =} canonicalize_file_name (@var{fname})
Return the canonical name of file @var{fname}.

If the file does not exist the empty string ("") is returned.  No tilde
expansion of @var{fname} is performed.
@seealso{make_absolute_filename, is_absolute_filename,
is_rooted_relative_filename, is_same_file, tilde_expand}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  std::string name = args(0).xstring_value ("canonicalize_file_name: NAME must be a string");

  std::string msg;

  std::string result = sys::canonicalize_file_name (name, msg);

  return ovl (result, msg.empty () ? 0 : -1, msg);
}

static inline octave_value
const_value (const octave_value_list& args, int val)
{
  if (args.length () != 0)
    print_usage ();

  return octave_value (val);
}

DEFUNX ("F_DUPFD", FF_DUPFD, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} F_DUPFD ()
Return the numerical value to pass to @code{fcntl} to return
a duplicate file descriptor.
@seealso{fcntl, F_GETFD, F_GETFL, F_SETFD, F_SETFL}
@end deftypefn */)
{
  static const int val = octave_f_dupfd_wrapper ();

  if (val < 0)
    err_disabled_feature ("F_DUPFD", "F_DUPFD");

  return const_value (args, val);
}

DEFUNX ("F_GETFD", FF_GETFD, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} F_GETFD ()
Return the numerical value to pass to @code{fcntl} to return
the file descriptor flags.
@seealso{fcntl, F_DUPFD, F_GETFL, F_SETFD, F_SETFL}
@end deftypefn */)
{
  static const int val = octave_f_getfd_wrapper ();

  if (val < 0)
    err_disabled_feature ("F_GETFD", "F_GETFD");

  return const_value (args, val);
}

DEFUNX ("F_GETFL", FF_GETFL, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} F_GETFL ()
Return the numerical value to pass to @code{fcntl} to return
the file status flags.
@seealso{fcntl, F_DUPFD, F_GETFD, F_SETFD, F_SETFL}
@end deftypefn */)
{
  static const int val = octave_f_getfl_wrapper ();

  if (val < 0)
    err_disabled_feature ("F_GETFL", "F_GETFL");

  return const_value (args, val);
}

DEFUNX ("F_SETFD", FF_SETFD, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} F_SETFD ()
Return the numerical value to pass to @code{fcntl} to set the file
descriptor flags.
@seealso{fcntl, F_DUPFD, F_GETFD, F_GETFL, F_SETFL}
@end deftypefn */)
{
  static const int val = octave_f_setfd_wrapper ();

  if (val < 0)
    err_disabled_feature ("F_SETFD", "F_SETFD");

  return const_value (args, val);
}

DEFUNX ("F_SETFL", FF_SETFL, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} F_SETFL ()
Return the numerical value to pass to @code{fcntl} to set the file
status flags.
@seealso{fcntl, F_DUPFD, F_GETFD, F_GETFL, F_SETFD}
@end deftypefn */)
{
  static const int val = octave_f_setfl_wrapper ();

  if (val < 0)
    err_disabled_feature ("F_SETFL", "F_SETFL");

  return const_value (args, val);
}

DEFUNX ("O_APPEND", FO_APPEND, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} O_APPEND ()
Return the numerical value of the @code{O_APPEND} macro.

@code{O_APPEND} is file status flag that may be returned by @code{fcntl}
to indicate each write operation appends, or that may be passed to
@code{fcntl} to set the write mode to append.
@seealso{fcntl, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC,
O_TRUNC, O_WRONLY}
@end deftypefn */)
{
  static const int val = octave_o_append_wrapper ();

  if (val < 0)
    err_disabled_feature ("O_APPEND", "O_APPEND");

  return const_value (args, val);
}

DEFUNX ("O_ASYNC", FO_ASYNC, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} O_ASYNC ()
Return the numerical value of the @code{O_ASYNC} macro.

@code{O_ASYNC} is the file status flag that may be returned by
@code{fcntl} to indicate asynchronous I/O.
@seealso{fcntl, O_APPEND, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC,
O_TRUNC, O_WRONLY}
@end deftypefn */)
{
  static const int val = octave_o_async_wrapper ();

  if (val < 0)
    err_disabled_feature ("O_ASYNC", "O_ASYNC");

  return const_value (args, val);
}

DEFUNX ("O_CREAT", FO_CREAT, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} O_CREAT ()
Return the numerical value of the @code{O_CREAT}.

@code{O_CREAT} is the file status flag that may be returned by
@code{fcntl} to indicate that a file should be created if it does not
exist.
@seealso{fcntl, O_APPEND, O_ASYNC, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC,
O_TRUNC, O_WRONLY}
@end deftypefn */)
{
  static const int val = octave_o_creat_wrapper ();

  if (val < 0)
    err_disabled_feature ("O_CREAT", "O_CREAT");

  return const_value (args, val);
}

DEFUNX ("O_EXCL", FO_EXCL, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} O_EXCL ()
Return the numerical value of the @code{O_EXCL}.

@code{O_EXCL} is the file status flag that may be returned by
@code{fcntl} to indicate that file locking is used.
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_NONBLOCK, O_RDONLY, O_RDWR,
O_SYNC, O_TRUNC, O_WRONLY}
@end deftypefn */)
{
  static const int val = octave_o_excl_wrapper ();

  if (val < 0)
    err_disabled_feature ("O_EXCL", "O_EXCL");

  return const_value (args, val);
}

DEFUNX ("O_NONBLOCK", FO_NONBLOCK, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} O_NONBLOCK ()
Return the numerical value of the @code{O_NONBLOCK}.

@code{O_NONBLOCK} is the file status flag that may be returned by
@code{fcntl} to indicate that non-blocking I/O is in use, or that may be
passsed to @code{fcntl} to set non-blocking I/O.
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_RDONLY, O_RDWR, O_SYNC,
O_TRUNC, O_WRONLY}
@end deftypefn */)
{
  static const int val = octave_o_nonblock_wrapper ();

  if (val < 0)
    err_disabled_feature ("O_NONBLOCK", "O_NONBLOCK");

  return const_value (args, val);
}

DEFUNX ("O_RDONLY", FO_RDONLY, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} O_RDONLY ()
Return the numerical value of the @code{O_RDONLY}.

@code{O_RDONLY} is the file status flag that may be returned by
@code{fcntl} to indicate that a file is open for reading only.
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDWR, O_SYNC,
O_TRUNC, O_WRONLY}
@end deftypefn */)
{
  static const int val = octave_o_rdonly_wrapper ();

  if (val < 0)
    err_disabled_feature ("O_RDONLY", "O_RDONLY");

  return const_value (args, val);
}

DEFUNX ("O_RDWR", FO_RDWR, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} O_RDWR ()
Return the numerical value of the @code{O_RDWR}.

@code{O_RDWR} is the file status flag that may be returned by
@code{fcntl} to indicate that a file is open for both reading and
writing.
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY,
O_SYNC, O_TRUNC, O_WRONLY}
@end deftypefn */)
{
  static const int val = octave_o_rdwr_wrapper ();

  if (val < 0)
    err_disabled_feature ("O_RDWR", "O_RDWR");

  return const_value (args, val);
}

DEFUNX ("O_SYNC", FO_SYNC, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} O_SYNC ()
Return the numerical value of the @code{O_SYNC}.

@code{O_SYNC} is the file status flag that may be returned by
@code{fcntl} to indicate that a file is open for synchronous I/O
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY,
O_RDWR, O_TRUNC, O_WRONLY}
@end deftypefn */)
{
  static const int val = octave_o_sync_wrapper ();

  if (val < 0)
    err_disabled_feature ("O_SYNC", "O_SYNC");

  return const_value (args, val);
}

DEFUNX ("O_TRUNC", FO_TRUNC, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} O_TRUNC ()
Return the numerical value of the @code{O_TRUNC}.

@code{O_TRUNC} is the file status flag that may be returned by
@code{fcntl} to indicate that if file exists, it should be truncated
when writing.
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY,
O_RDWR, O_SYNC, O_WRONLY}
@end deftypefn */)
{
  static const int val = octave_o_trunc_wrapper ();

  if (val < 0)
    err_disabled_feature ("O_TRUNC", "O_TRUNC");

  return const_value (args, val);
}

DEFUNX ("O_WRONLY", FO_WRONLY, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} O_WRONLY ()
Return the numerical value of the @code{O_WRONLY}.

@code{O_WRONLY} is the file status flag that may be returned by
@code{fcntl} to indicate that a file is open for writing only
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY,
O_RDWR, O_SYNC, O_TRUNC}
@end deftypefn */)
{
  static const int val = octave_o_wronly_wrapper ();

  if (val < 0)
    err_disabled_feature ("O_WRONLY", "O_WRONLY");

  return const_value (args, val);
}

DEFUNX ("WNOHANG", FWNOHANG, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} WNOHANG ()
Return the numerical value of the @code{WNOHANG} macro.

@code{WNOHANG} is the option argument that may be passed to
@code{waitpid} to indicate that it should return its status immediately
instead of waiting for a process to exit.
@seealso{waitpid, WUNTRACED, WCONTINUE}
@end deftypefn */)
{
  return const_value (args, sys::wnohang ());
}

DEFUNX ("WUNTRACED", FWUNTRACED, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} WUNTRACED ()
Return the numerical value of the @code{WUNTRACED} macro.

@code{WUNTRACED} is the option argument that may be passed to
@code{waitpid} to indicate that it should also return if the child
process has stopped but is not traced via the @code{ptrace} system call
@seealso{waitpid, WNOHANG, WCONTINUE}
@end deftypefn */)
{
  return const_value (args, sys::wuntraced ());
}

DEFUNX ("WCONTINUE", FWCONTINUE, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} WCONTINUE ()
Return the numerical value of the @code{WCONTINUE} macro.

@code{WCONTINUE} is the option argument that may be passed to
@code{waitpid} to indicate that it should also return if a stopped child
has been resumed by delivery of a @code{SIGCONT} signal.
@seealso{waitpid, WNOHANG, WUNTRACED}
@end deftypefn */)
{
  return const_value (args, sys::wcontinue ());
}

OCTAVE_END_NAMESPACE(octave)
