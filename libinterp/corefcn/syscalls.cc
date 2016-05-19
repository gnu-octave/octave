/*

Copyright (C) 1996-2015 John W. Eaton
Copyright (C) 2010 VZLU Prague

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

// Thomas Baier <baier@ci.tuwien.ac.at> added the original versions of
// the following functions:
//
//   mkfifo  unlink  waitpid

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstdio>
#include <cstring>

#include <sys/types.h>
#include <unistd.h>

#include <fcntl.h>

#include "cmd-hist.h"
#include "file-ops.h"
#include "file-stat.h"
#include "oct-env.h"
#include "oct-syscalls.h"
#include "oct-uname.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "lo-utils.h"
#include "oct-hist.h"
#include "oct-map.h"
#include "ovl.h"
#include "oct-stdstrm.h"
#include "oct-stream.h"
#include "sysdep.h"
#include "utils.h"
#include "variables.h"
#include "input.h"

static octave_scalar_map
mk_stat_map (const octave::sys::base_file_stat& fs)
{
  octave_scalar_map m;

  m.assign ("dev", static_cast<double> (fs.dev ()));
  m.assign ("ino", fs.ino ());
  m.assign ("mode", fs.mode ());
  m.assign ("modestr", fs.mode_as_string ());
  m.assign ("nlink", fs.nlink ());
  m.assign ("uid", fs.uid ());
  m.assign ("gid", fs.gid ());
#if defined (HAVE_STRUCT_STAT_ST_RDEV)
  m.assign ("rdev", static_cast<double> (fs.rdev ()));
#endif
  m.assign ("size", fs.size ());
  m.assign ("atime", fs.atime ());
  m.assign ("mtime", fs.mtime ());
  m.assign ("ctime", fs.ctime ());
#if defined (HAVE_STRUCT_STAT_ST_BLKSIZE)
  m.assign ("blksize", fs.blksize ());
#endif
#if defined (HAVE_STRUCT_STAT_ST_BLOCKS)
  m.assign ("blocks", fs.blocks ());
#endif

  return m;
}

static octave_value_list
mk_stat_result (const octave::sys::base_file_stat& fs)
{
  if (fs)
    return ovl (octave_value (mk_stat_map (fs)), 0, "");
  else
    return ovl (Matrix (), -1, fs.error ());
}

DEFUNX ("dup2", Fdup2, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {[@var{fid}, @var{msg}] =} dup2 (@var{old}, @var{new})\n\
Duplicate a file descriptor.\n\
\n\
If successful, @var{fid} is greater than zero and contains the new file ID@.\n\
Otherwise, @var{fid} is negative and @var{msg} contains a system-dependent\n\
error message.\n\
@seealso{fopen, fclose, fcntl}\n\
@end deftypefn")
{
  if (args.length () != 2)
    print_usage ();

  octave_stream old_stream = octave_stream_list::lookup (args(0), "dup2");

  octave_stream new_stream = octave_stream_list::lookup (args(1), "dup2");

  int i_old = old_stream.file_number ();
  int i_new = new_stream.file_number ();

  if (i_old >= 0 && i_new >= 0)
    {
      std::string msg;

      int status = octave::sys::dup2 (i_old, i_new, msg);

      return ovl (status, msg);
    }
  else
    return ovl (-1, "");
}

DEFUNX ("exec", Fexec, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {[@var{err}, @var{msg}] =} exec (@var{file}, @var{args})\n\
Replace current process with a new process.\n\
\n\
Calling @code{exec} without first calling @code{fork} will terminate your\n\
current Octave process and replace it with the program named by @var{file}.\n\
For example,\n\
\n\
@example\n\
exec (\"ls\", \"-l\")\n\
@end example\n\
\n\
@noindent\n\
will run @code{ls} and return you to your shell prompt.\n\
\n\
If successful, @code{exec} does not return.  If @code{exec} does return,\n\
@var{err} will be nonzero, and @var{msg} will contain a system-dependent\n\
error message.\n\
@end deftypefn")
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

  octave_history_write_timestamp ();

  if (! command_history::ignoring_entries ())
    command_history::clean_up_and_save ();

  std::string msg;

  int status = octave::sys::execvp (exec_file, exec_args, msg);

  return ovl (status, msg);
}

DEFUNX ("popen2", Fpopen2, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {[@var{in}, @var{out}, @var{pid}] =} popen2 (@var{command}, @var{args})\n\
Start a subprocess with two-way communication.\n\
\n\
The name of the process is given by @var{command}, and @var{args} is an\n\
array of strings containing options for the command.\n\
\n\
The file identifiers for the input and output streams of the subprocess are\n\
returned in @var{in} and @var{out}.  If execution of the command is\n\
successful, @var{pid} contains the process ID of the subprocess.  Otherwise,\n\
@var{pid} is @minus{}1.\n\
\n\
For example:\n\
\n\
@example\n\
[in, out, pid] = popen2 (\"sort\", \"-r\");\n\
fputs (in, \"these\\nare\\nsome\\nstrings\\n\");\n\
fclose (in);\n\
EAGAIN = errno (\"EAGAIN\");\n\
done = false;\n\
do\n\
  s = fgets (out);\n\
  if (ischar (s))\n\
    fputs (stdout, s);\n\
  elseif (errno () == EAGAIN)\n\
    pause (0.1);\n\
    fclear (out);\n\
  else\n\
    done = true;\n\
  endif\n\
until (done)\n\
fclose (out);\n\
waitpid (pid);\n\
\n\
   @print{} these\n\
   @print{} strings\n\
   @print{} some\n\
   @print{} are\n\
@end example\n\
\n\
Note that @code{popen2}, unlike @code{popen}, will not @nospell{\"reap\"}\n\
the child process.  If you don't use @code{waitpid} to check the child's\n\
exit status, it will linger until Octave exits.\n\
@seealso{popen, waitpid}\n\
@end deftypefn")
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

  pid = octave::sys::popen2 (exec_file, arg_list, sync_mode, filedesc,
                             msg, interactive);
  if (pid < 0)
    error (msg.c_str ());

  FILE *ifile = fdopen (filedesc[1], "r");
  FILE *ofile = fdopen (filedesc[0], "w");

  octave_stream is = octave_stdiostream::create (exec_file + "-in",
                                                 ifile,
                                                 std::ios::in);

  octave_stream os = octave_stdiostream::create (exec_file + "-out",
                                                 ofile,
                                                 std::ios::out);

  return ovl (octave_stream_list::insert (os),
              octave_stream_list::insert (is),
              pid);
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

DEFUNX ("fcntl", Ffcntl, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {[@var{err}, @var{msg}] =} fcntl (@var{fid}, @var{request}, @var{arg})\n\
Change the properties of the open file @var{fid}.\n\
\n\
The following values may be passed as @var{request}:\n\
\n\
@vtable @code\n\
@item F_DUPFD\n\
Return a duplicate file descriptor.\n\
\n\
@item F_GETFD\n\
Return the file descriptor flags for @var{fid}.\n\
\n\
@item F_SETFD\n\
Set the file descriptor flags for @var{fid}.\n\
\n\
@item F_GETFL\n\
Return the file status flags for @var{fid}.  The following codes may be\n\
returned (some of the flags may be undefined on some systems).\n\
\n\
@vtable @code\n\
@item O_RDONLY\n\
Open for reading only.\n\
\n\
@item O_WRONLY\n\
Open for writing only.\n\
\n\
@item O_RDWR\n\
Open for reading and writing.\n\
\n\
@item O_APPEND\n\
Append on each write.\n\
\n\
@item O_CREAT\n\
Create the file if it does not exist.\n\
\n\
@item O_NONBLOCK\n\
Non-blocking mode.\n\
\n\
@item O_SYNC\n\
Wait for writes to complete.\n\
\n\
@item O_ASYNC\n\
Asynchronous I/O.\n\
@end vtable\n\
\n\
@item F_SETFL\n\
Set the file status flags for @var{fid} to the value specified by @var{arg}.\n\
 The only flags that can be changed are @w{@code{O_APPEND}} and\n\
@w{@code{O_NONBLOCK}}.\n\
@end vtable\n\
\n\
If successful, @var{err} is 0 and @var{msg} is an empty string.  Otherwise,\n\
@var{err} is nonzero and @var{msg} contains a system-dependent error\n\
message.\n\
@seealso{fopen, dup2}\n\
@end deftypefn")
{
  if (args.length () != 3)
    print_usage ();

  octave_stream strm = octave_stream_list::lookup (args(0), "fcntl");

  int fid = strm.file_number ();

  // FIXME: Do we want to use xint_value and throw a warning message
  //        if input validation fails?
  int req = args(1).int_value (true);
  int arg = args(2).int_value (true);

  // FIXME: Need better checking here?
  if (fid < 0)
    error ("fcntl: invalid file id");

  std::string msg;

  int status = octave::sys::fcntl (fid, req, arg, msg);

  return ovl (status, msg);
}

DEFUNX ("fork", Ffork, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {[@var{pid}, @var{msg}] =} fork ()\n\
Create a copy of the current process.\n\
\n\
Fork can return one of the following values:\n\
\n\
@table @asis\n\
@item > 0\n\
You are in the parent process.  The value returned from @code{fork} is the\n\
process id of the child process.  You should probably arrange to wait for\n\
any child processes to exit.\n\
\n\
@item 0\n\
You are in the child process.  You can call @code{exec} to start another\n\
process.  If that fails, you should probably call @code{exit}.\n\
\n\
@item < 0\n\
The call to @code{fork} failed for some reason.  You must take evasive\n\
action.  A system dependent error message will be waiting in @var{msg}.\n\
@end table\n\
@end deftypefn")
{
  if (args.length () != 0)
    print_usage ();

  if (symbol_table::at_top_level ())
    error ("fork: cannot be called from command line");

  std::string msg;

  pid_t pid = octave::sys::fork (msg);

  return ovl (pid, msg);
}

DEFUNX ("getpgrp", Fgetpgrp, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {pgid =} getpgrp ()\n\
Return the process group id of the current process.\n\
@end deftypefn")
{
  if (args.length () != 0)
    print_usage ();

  std::string msg;

  pid_t pid = octave::sys::getpgrp (msg);

  return ovl (pid, msg);
}

DEFUNX ("getpid", Fgetpid, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {pid =} getpid ()\n\
Return the process id of the current process.\n\
@seealso{getppid}\n\
@end deftypefn")
{
  if (args.length () != 0)
    print_usage ();

  return ovl (octave::sys::getpid ());
}

DEFUNX ("getppid", Fgetppid, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {pid =} getppid ()\n\
Return the process id of the parent process.\n\
@seealso{getpid}\n\
@end deftypefn")
{
  if (args.length () != 0)
    print_usage ();

  return ovl (octave::sys::getppid ());
}

DEFUNX ("getegid", Fgetegid, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {egid =} getegid ()\n\
Return the effective group id of the current process.\n\
@seealso{getgid}\n\
@end deftypefn")
{
  if (args.length () != 0)
    print_usage ();

  return ovl (octave::sys::getegid ());
}

DEFUNX ("getgid", Fgetgid, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {gid =} getgid ()\n\
Return the real group id of the current process.\n\
@seealso{getegid}\n\
@end deftypefn")
{
  if (args.length () != 0)
    print_usage ();

  return ovl (octave::sys::getgid ());
}

DEFUNX ("geteuid", Fgeteuid, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {euid =} geteuid ()\n\
Return the effective user id of the current process.\n\
@seealso{getuid}\n\
@end deftypefn")
{
  if (args.length () != 0)
    print_usage ();

  return ovl (octave::sys::geteuid ());
}

DEFUNX ("getuid", Fgetuid, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {uid =} getuid ()\n\
Return the real user id of the current process.\n\
@seealso{geteuid}\n\
@end deftypefn")
{
  if (args.length () != 0)
    print_usage ();

  return ovl (octave::sys::getuid ());
}

DEFUNX ("kill", Fkill, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {[@var{err}, @var{msg}] =} kill (@var{pid}, @var{sig})\n\
Send signal @var{sig} to process @var{pid}.\n\
\n\
If @var{pid} is positive, then signal @var{sig} is sent to @var{pid}.\n\
\n\
If @var{pid} is 0, then signal @var{sig} is sent to every process\n\
in the process group of the current process.\n\
\n\
If @var{pid} is -1, then signal @var{sig} is sent to every process\n\
except process 1.\n\
\n\
If @var{pid} is less than -1, then signal @var{sig} is sent to every\n\
process in the process group @var{-pid}.\n\
\n\
If @var{sig} is 0, then no signal is sent, but error checking is still\n\
performed.\n\
\n\
Return 0 if successful, otherwise return -1.\n\
@end deftypefn")
{
  if (args.length () != 2)
    print_usage ();

  pid_t pid = args(0).int_value (true);

  int sig = args(1).int_value (true);

  std::string msg;

  int status = octave::sys::kill (pid, sig, msg);

  return ovl (status, msg);
}

DEFUNX ("lstat", Flstat, args, ,
        "-*- texinfo -*-\n\
@deftypefn  {} {@var{info} =} lstat (@var{symlink})\n\
@deftypefnx {} {[@var{info}, @var{err}, @var{msg}] =} lstat (@var{symlink})\n\
Return a structure @var{info} containing information about the symbolic link\n\
@var{symlink}.\n\
\n\
The function outputs are described in the documentation for @code{stat}.\n\
@seealso{stat, symlink}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  std::string fname = args(0).xstring_value ("lstat: NAME must be a string");

  octave::sys::file_stat fs (fname, false);

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

DEFUNX ("mkfifo", Fmkfifo, args, ,
        "-*- texinfo -*-\n\
@deftypefn  {} {@var{err} =} mkfifo (@var{name}, @var{mode})\n\
@deftypefnx {} {[@var{err}, @var{msg}] =} mkfifo (@var{name}, @var{mode})\n\
Create a FIFO special file named @var{name} with file mode @var{mode}.\n\
\n\
@var{mode} is interpreted as an octal number and is subject to umask\n\
processing.  The final calculated mode is @code{@var{mode} - @var{umask}}.\n\
\n\
If successful, @var{err} is 0 and @var{msg} is an empty string.\n\
Otherwise, @var{err} is nonzero and @var{msg} contains a system-dependent\n\
error message.\n\
@seealso{pipe, umask}\n\
@end deftypefn")
{
  if (args.length () != 2)
    print_usage ();

  std::string name = args(0).xstring_value ("mkfifo: FILE must be a string");

  int octal_mode = args(1).xint_value ("mkfifo: MODE must be an integer");

  if (octal_mode < 0)
    error ("mkfifo: MODE must be a positive integer value");

  int mode = convert (octal_mode, 8, 10);

  std::string msg;

  int status = octave::sys::mkfifo (name, mode, msg);

  return ovl (status, msg);
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

DEFUNX ("pipe", Fpipe, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {[@var{read_fd}, @var{write_fd}, @var{err}, @var{msg}] =} pipe ()\n\
Create a pipe and return the reading and writing ends of the pipe into\n\
@var{read_fd} and @var{write_fd} respectively.\n\
\n\
If successful, @var{err} is 0 and @var{msg} is an empty string.\n\
Otherwise, @var{err} is nonzero and @var{msg} contains a system-dependent\n\
error message.\n\
@seealso{mkfifo}\n\
@end deftypefn")
{
  if (args.length () != 0)
    print_usage ();

  int fid[2];
  std::string msg;

  int status = octave::sys::pipe (fid, msg);

  if (status < 0)
    return ovl (-1, -1, -1, msg);
  else
    {
      FILE *ifile = fdopen (fid[0], "r");
      FILE *ofile = fdopen (fid[1], "w");

      octave_stream is = octave_stdiostream::create ("pipe-in", ifile,
                                                       std::ios::in);

      octave_stream os = octave_stdiostream::create ("pipe-out", ofile,
                                                       std::ios::out);

      return ovl (octave_stream_list::insert (is),
                  octave_stream_list::insert (os),
                  status,
                  msg);
    }
}

DEFUNX ("stat", Fstat, args, ,
        "-*- texinfo -*-\n\
@deftypefn  {} {[@var{info}, @var{err}, @var{msg}] =} stat (@var{file})\n\
@deftypefnx {} {[@var{info}, @var{err}, @var{msg}] =} stat (@var{fid})\n\
@deftypefnx {} {[@var{info}, @var{err}, @var{msg}] =} lstat (@var{file})\n\
@deftypefnx {} {[@var{info}, @var{err}, @var{msg}] =} lstat (@var{fid})\n\
Return a structure @var{info} containing the following information about\n\
@var{file} or file identifier @var{fid}.\n\
\n\
@table @code\n\
@item dev\n\
ID of device containing a directory entry for this file.\n\
\n\
@item ino\n\
File number of the file.\n\
\n\
@item mode\n\
File mode, as an integer.  Use the functions @w{@code{S_ISREG}},\n\
@w{@code{S_ISDIR}}, @w{@code{S_ISCHR}}, @w{@code{S_ISBLK}},\n\
@w{@code{S_ISFIFO}}, @w{@code{S_ISLNK}}, or @w{@code{S_ISSOCK}} to extract\n\
information from this value.\n\
\n\
@item modestr\n\
File mode, as a string of ten letters or dashes as would be returned by\n\
@kbd{ls -l}.\n\
\n\
@item nlink\n\
Number of links.\n\
\n\
@item uid\n\
User ID of file's owner.\n\
\n\
@item gid\n\
Group ID of file's group.\n\
\n\
@item rdev\n\
ID of device for block or character special files.\n\
\n\
@item size\n\
Size in bytes.\n\
\n\
@item atime\n\
Time of last access in the same form as time values returned from\n\
@code{time}.  @xref{Timing Utilities}.\n\
\n\
@item mtime\n\
Time of last modification in the same form as time values returned from\n\
@code{time}.  @xref{Timing Utilities}.\n\
\n\
@item ctime\n\
Time of last file status change in the same form as time values\n\
returned from @code{time}.  @xref{Timing Utilities}.\n\
\n\
@item blksize\n\
Size of blocks in the file.\n\
\n\
@item blocks\n\
Number of blocks allocated for file.\n\
@end table\n\
\n\
If the call is successful @var{err} is 0 and @var{msg} is an empty string.\n\
If the file does not exist, or some other error occurs, @var{info} is an\n\
empty matrix, @var{err} is @minus{}1, and @var{msg} contains the\n\
corresponding system error message.\n\
\n\
If @var{file} is a symbolic link, @code{stat} will return information about\n\
the actual file that is referenced by the link.  Use @code{lstat} if you\n\
want information about the symbolic link itself.\n\
\n\
For example:\n\
\n\
@example\n\
[info, err, msg] = stat (\"/vmlinuz\")\n\
  @result{} info =\n\
     @{\n\
       atime = 855399756\n\
       rdev = 0\n\
       ctime = 847219094\n\
       uid = 0\n\
       size = 389218\n\
       blksize = 4096\n\
       mtime = 847219094\n\
       gid = 6\n\
       nlink = 1\n\
       blocks = 768\n\
       mode = -rw-r--r--\n\
       modestr = -rw-r--r--\n\
       ino = 9316\n\
       dev = 2049\n\
     @}\n\
  @result{} err = 0\n\
  @result{} msg =\n\
@end example\n\
@seealso{lstat, ls, dir}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  octave_value_list retval;

  if (args(0).is_scalar_type ())
    {
      int fid = octave_stream_list::get_file_number (args(0));

      octave::sys::file_fstat fs (fid);

      retval = mk_stat_result (fs);
    }
  else
    {
      std::string fname = args(0).xstring_value ("stat: NAME must be a string");

      octave::sys::file_stat fs (fname);

      retval = mk_stat_result (fs);
    }

  return retval;
}

DEFUNX ("S_ISREG", FS_ISREG, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} S_ISREG (@var{mode})\n\
Return true if @var{mode} corresponds to a regular file.\n\
\n\
The value of @var{mode} is assumed to be returned from a call to\n\
@code{stat}.\n\
@seealso{stat, lstat}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  double mode = args(0).xdouble_value ("S_ISREG: invalid MODE value");

  return ovl (octave::sys::file_stat::is_reg (static_cast<mode_t> (mode)));
}

DEFUNX ("S_ISDIR", FS_ISDIR, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} S_ISDIR (@var{mode})\n\
Return true if @var{mode} corresponds to a directory.\n\
\n\
The value of @var{mode} is assumed to be returned from a call to\n\
@code{stat}.\n\
@seealso{stat, lstat}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  double mode = args(0).xdouble_value ("S_ISDIR: invalid MODE value");

  return ovl (octave::sys::file_stat::is_dir (static_cast<mode_t> (mode)));
}

DEFUNX ("S_ISCHR", FS_ISCHR, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} S_ISCHR (@var{mode})\n\
Return true if @var{mode} corresponds to a character device.\n\
\n\
The value of @var{mode} is assumed to be returned from a call to\n\
@code{stat}.\n\
@seealso{stat, lstat}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  double mode = args(0).xdouble_value ("S_ISCHR: invalid MODE value");

  return ovl (octave::sys::file_stat::is_chr (static_cast<mode_t> (mode)));
}

DEFUNX ("S_ISBLK", FS_ISBLK, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} S_ISBLK (@var{mode})\n\
Return true if @var{mode} corresponds to a block device.\n\
\n\
The value of @var{mode} is assumed to be returned from a call to\n\
@code{stat}.\n\
@seealso{stat, lstat}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  double mode = args(0).xdouble_value ("S_ISBLK: invalid MODE value");

  return ovl (octave::sys::file_stat::is_blk (static_cast<mode_t> (mode)));
}

DEFUNX ("S_ISFIFO", FS_ISFIFO, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} S_ISFIFO (@var{mode})\n\
Return true if @var{mode} corresponds to a fifo.\n\
\n\
The value of @var{mode} is assumed to be returned from a call to\n\
@code{stat}.\n\
@seealso{stat, lstat}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  double mode = args(0).xdouble_value ("S_ISFIFO: invalid MODE value");

  return ovl (octave::sys::file_stat::is_fifo (static_cast<mode_t> (mode)));
}

DEFUNX ("S_ISLNK", FS_ISLNK, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} S_ISLNK (@var{mode})\n\
Return true if @var{mode} corresponds to a symbolic link.\n\
\n\
The value of @var{mode} is assumed to be returned from a call to\n\
@code{stat}.\n\
@seealso{stat, lstat}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  double mode = args(0).xdouble_value ("S_ISLNK: invalid MODE value");

  return ovl (octave::sys::file_stat::is_lnk (static_cast<mode_t> (mode)));
}

DEFUNX ("S_ISSOCK", FS_ISSOCK, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} S_ISSOCK (@var{mode})\n\
Return true if @var{mode} corresponds to a socket.\n\
\n\
The value of @var{mode} is assumed to be returned from a call to\n\
@code{stat}.\n\
@seealso{stat, lstat}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  double mode = args(0).xdouble_value ("S_ISSOCK: invalid MODE value");

  return ovl (octave::sys::file_stat::is_sock (static_cast<mode_t> (mode)));
}

DEFUN (gethostname, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} gethostname ()\n\
Return the hostname of the system where Octave is running.\n\
@end deftypefn")
{
  if (args.length () != 0)
    print_usage ();

  return ovl (octave::sys::env::get_host_name ());
}

DEFUN (uname, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {[@var{uts}, @var{err}, @var{msg}] =} uname ()\n\
Return system information in the structure.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
uname ()\n\
   @result{} @{\n\
         sysname = x86_64\n\
         nodename = segfault\n\
         release = 2.6.15-1-amd64-k8-smp\n\
         version = Linux\n\
         machine = #2 SMP Thu Feb 23 04:57:49 UTC 2006\n\
      @}\n\
@end group\n\
@end example\n\
\n\
If successful, @var{err} is 0 and @var{msg} is an empty string.\n\
Otherwise, @var{err} is nonzero and @var{msg} contains a\n\
system-dependent error message.\n\
@end deftypefn")
{
  if (args.length () != 0)
    print_usage ();

  octave::sys::uname sysinfo;

  octave_scalar_map m;

  m.assign ("sysname", sysinfo.sysname ());
  m.assign ("nodename", sysinfo.nodename ());
  m.assign ("release", sysinfo.release ());
  m.assign ("version", sysinfo.version ());
  m.assign ("machine", sysinfo.machine ());

  return ovl (m, sysinfo.error (), sysinfo.message ());
}

DEFUNX ("unlink", Funlink, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {[@var{err}, @var{msg}] =} unlink (@var{file})\n\
Delete the file named @var{file}.\n\
\n\
If successful, @var{err} is 0 and @var{msg} is an empty string.\n\
Otherwise, @var{err} is nonzero and @var{msg} contains a system-dependent\n\
error message.\n\
@seealso{delete, rmdir}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  std::string name = args(0).xstring_value ("unlink: FILE must be a string");

  std::string msg;

  int status = octave::sys::unlink (name, msg);

  return ovl (status, msg);
}

DEFUNX ("waitpid", Fwaitpid, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {[@var{pid}, @var{status}, @var{msg}] =} waitpid (@var{pid}, @var{options})\n\
Wait for process @var{pid} to terminate.\n\
\n\
The @var{pid} argument can be:\n\
\n\
@table @asis\n\
@item @minus{}1\n\
Wait for any child process.\n\
\n\
@item 0\n\
Wait for any child process whose process group ID is equal to that of the\n\
Octave interpreter process.\n\
\n\
@item > 0\n\
Wait for termination of the child process with ID @var{pid}.\n\
@end table\n\
\n\
The @var{options} argument can be a bitwise OR of zero or more of the\n\
following constants:\n\
\n\
@table @code\n\
@item 0\n\
Wait until signal is received or a child process exits (this is the default\n\
if the @var{options} argument is missing).\n\
\n\
@item WNOHANG\n\
Do not hang if status is not immediately available.\n\
\n\
@item WUNTRACED\n\
Report the status of any child processes that are stopped, and whose status\n\
has not yet been reported since they stopped.\n\
\n\
@item WCONTINUE\n\
Return if a stopped child has been resumed by delivery of @code{SIGCONT}.\n\
This value may not be meaningful on all systems.\n\
@end table\n\
\n\
If the returned value of @var{pid} is greater than 0, it is the process ID\n\
of the child process that exited.  If an error occurs, @var{pid} will be\n\
less than zero and @var{msg} will contain a system-dependent error message.\n\
The value of @var{status} contains additional system-dependent information\n\
about the subprocess that exited.\n\
@seealso{WCONTINUE, WCOREDUMP, WEXITSTATUS, WIFCONTINUED, WIFSIGNALED, WIFSTOPPED, WNOHANG, WSTOPSIG, WTERMSIG, WUNTRACED}\n\
@end deftypefn")
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

  pid_t result = octave::sys::waitpid (pid, &status, options, msg);

  return ovl (result, status, msg);
}

DEFUNX ("WIFEXITED", FWIFEXITED, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} WIFEXITED (@var{status})\n\
Given @var{status} from a call to @code{waitpid}, return\n\
true if the child terminated normally.\n\
@seealso{waitpid, WEXITSTATUS, WIFSIGNALED, WTERMSIG, WCOREDUMP, WIFSTOPPED, WSTOPSIG, WIFCONTINUED}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  int status = args(0).xint_value ("WIFEXITED: STATUS must be an integer");

  return ovl (octave_wait::ifexited (status));
}

DEFUNX ("WEXITSTATUS", FWEXITSTATUS, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} WEXITSTATUS (@var{status})\n\
Given @var{status} from a call to @code{waitpid}, return\n\
the exit status of the child.\n\
\n\
This function should only be employed if @code{WIFEXITED} returned true.\n\
@seealso{waitpid, WIFEXITED, WIFSIGNALED, WTERMSIG, WCOREDUMP, WIFSTOPPED, WSTOPSIG, WIFCONTINUED}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  int status = args(0).xint_value ("WEXITSTATUS: STATUS must be an integer");

  return ovl (octave_wait::exitstatus (status));
}

DEFUNX ("WIFSIGNALED", FWIFSIGNALED, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} WIFSIGNALED (@var{status})\n\
Given @var{status} from a call to @code{waitpid}, return\n\
true if the child process was terminated by a signal.\n\
@seealso{waitpid, WIFEXITED, WEXITSTATUS, WTERMSIG, WCOREDUMP, WIFSTOPPED, WSTOPSIG, WIFCONTINUED}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  int status = args(0).xint_value ("WIFSIGNALED: STATUS must be an integer");

  return ovl (octave_wait::ifsignaled (status));
}

DEFUNX ("WTERMSIG", FWTERMSIG, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} WTERMSIG (@var{status})\n\
Given @var{status} from a call to @code{waitpid}, return\n\
the number of the signal that caused the child process to terminate.\n\
\n\
This function should only be employed if @code{WIFSIGNALED} returned true.\n\
@seealso{waitpid, WIFEXITED, WEXITSTATUS, WIFSIGNALED, WCOREDUMP, WIFSTOPPED, WSTOPSIG, WIFCONTINUED}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  int status = args(0).xint_value ("WTERMSIG: STATUS must be an integer");

  return ovl (octave_wait::termsig (status));
}

DEFUNX ("WCOREDUMP", FWCOREDUMP, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} WCOREDUMP (@var{status})\n\
Given @var{status} from a call to @code{waitpid}, return\n\
true if the child produced a core dump.\n\
\n\
This function should only be employed if @code{WIFSIGNALED} returned true.\n\
The macro used to implement this function is not specified in POSIX.1-2001\n\
and is not available on some Unix implementations (e.g., AIX, SunOS).\n\
@seealso{waitpid, WIFEXITED, WEXITSTATUS, WIFSIGNALED, WTERMSIG, WIFSTOPPED, WSTOPSIG, WIFCONTINUED}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  int status = args(0).xint_value ("WCOREDUMP: STATUS must be an integer");

  return ovl (octave_wait::coredump (status));
}

DEFUNX ("WIFSTOPPED", FWIFSTOPPED, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} WIFSTOPPED (@var{status})\n\
Given @var{status} from a call to @code{waitpid}, return\n\
true if the child process was stopped by delivery of a signal.\n\
\n\
This is only possible if the call was done using @code{WUNTRACED} or when\n\
the child is being traced (see ptrace(2)).\n\
@seealso{waitpid, WIFEXITED, WEXITSTATUS, WIFSIGNALED, WTERMSIG, WCOREDUMP, WSTOPSIG, WIFCONTINUED}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  int status = args(0).xint_value ("WIFSTOPPED: STATUS must be an integer");

  return ovl (octave_wait::ifstopped (status));
}

DEFUNX ("WSTOPSIG", FWSTOPSIG, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} WSTOPSIG (@var{status})\n\
Given @var{status} from a call to @code{waitpid}, return\n\
the number of the signal which caused the child to stop.\n\
\n\
This function should only be employed if @code{WIFSTOPPED} returned true.\n\
@seealso{waitpid, WIFEXITED, WEXITSTATUS, WIFSIGNALED, WTERMSIG, WCOREDUMP, WIFSTOPPED, WIFCONTINUED}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  int status = args(0).xint_value ("WSTOPSIG: STATUS must be an integer");

  return ovl (octave_wait::stopsig (status));
}

DEFUNX ("WIFCONTINUED", FWIFCONTINUED, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} WIFCONTINUED (@var{status})\n\
Given @var{status} from a call to @code{waitpid}, return\n\
true if the child process was resumed by delivery of @code{SIGCONT}.\n\
@seealso{waitpid, WIFEXITED, WEXITSTATUS, WIFSIGNALED, WTERMSIG, WCOREDUMP, WIFSTOPPED, WSTOPSIG}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  int status = args(0).xint_value ("WIFCONTINUED: STATUS must be an integer");

  return ovl (octave_wait::ifcontinued (status));
}

DEFUNX ("canonicalize_file_name", Fcanonicalize_file_name, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {[@var{cname}, @var{status}, @var{msg}] =} canonicalize_file_name (@var{fname})\n\
Return the canonical name of file @var{fname}.\n\
\n\
If the file does not exist the empty string (\"\") is returned.\n\
@seealso{make_absolute_filename, is_absolute_filename, is_rooted_relative_filename}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  std::string name = args(0).xstring_value ("canonicalize_file_name: NAME must be a string");

  std::string msg;

  std::string result = octave::sys::canonicalize_file_name (name, msg);

  return ovl (result, msg.empty () ? 0 : -1, msg);
}

static inline octave_value
const_value (const octave_value_list& args, int val)
{
  if (args.length () != 0)
    print_usage ();

  return octave_value (val);
}

#if ! defined (O_NONBLOCK) && defined (O_NDELAY)
#define O_NONBLOCK O_NDELAY
#endif

DEFUNX ("F_DUPFD", FF_DUPFD, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} F_DUPFD ()\n\
Return the numerical value to pass to @code{fcntl} to return\n\
a duplicate file descriptor.\n\
@seealso{fcntl, F_GETFD, F_GETFL, F_SETFD, F_SETFL}\n\
@end deftypefn")
{
#if defined (F_DUPFD)
  return const_value (args, F_DUPFD);
#else
  err_disabled_feature ("F_DUPFD", "F_DUPFD");
#endif
}

DEFUNX ("F_GETFD", FF_GETFD, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} F_GETFD ()\n\
Return the numerical value to pass to @code{fcntl} to return\n\
the file descriptor flags.\n\
@seealso{fcntl, F_DUPFD, F_GETFL, F_SETFD, F_SETFL}\n\
@end deftypefn")
{
#if defined (F_GETFD)
  return const_value (args, F_GETFD);
#else
  err_disabled_feature ("F_GETFD", "F_GETFD");
#endif
}

DEFUNX ("F_GETFL", FF_GETFL, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} F_GETFL ()\n\
Return the numerical value to pass to @code{fcntl} to return\n\
the file status flags.\n\
@seealso{fcntl, F_DUPFD, F_GETFD, F_SETFD, F_SETFL}\n\
@end deftypefn")
{
#if defined (F_GETFL)
  return const_value (args, F_GETFL);
#else
  err_disabled_feature ("F_GETFL", "F_GETFL");
#endif
}

DEFUNX ("F_SETFD", FF_SETFD, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} F_SETFD ()\n\
Return the numerical value to pass to @code{fcntl} to set the file\n\
descriptor flags.\n\
@seealso{fcntl, F_DUPFD, F_GETFD, F_GETFL, F_SETFL}\n\
@end deftypefn")
{
#if defined (F_SETFD)
  return const_value (args, F_SETFD);
#else
  err_disabled_feature ("F_SETFD", "F_SETFD");
#endif
}

DEFUNX ("F_SETFL", FF_SETFL, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} F_SETFL ()\n\
Return the numerical value to pass to @code{fcntl} to set the file\n\
status flags.\n\
@seealso{fcntl, F_DUPFD, F_GETFD, F_GETFL, F_SETFD}\n\
@end deftypefn")
{
#if defined (F_SETFL)
  return const_value (args, F_SETFL);
#else
  err_disabled_feature ("F_SETFL", "F_SETFL");
#endif
}

DEFUNX ("O_APPEND", FO_APPEND, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} O_APPEND ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate each write operation appends,\n\
or that may be passed to @code{fcntl} to set the write mode to append.\n\
@seealso{fcntl, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end deftypefn")
{
#if defined (O_APPEND)
  return const_value (args, O_APPEND);
#else
  err_disabled_feature ("O_APPEND", "O_APPEND");
#endif
}

DEFUNX ("O_ASYNC", FO_ASYNC, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} O_ASYNC ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate asynchronous I/O.\n\
@seealso{fcntl, O_APPEND, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end deftypefn")
{
#if defined (O_ASYNC)
  return const_value (args, O_ASYNC);
#else
  err_disabled_feature ("O_ASYNC", "O_ASYNC");
#endif
}

DEFUNX ("O_CREAT", FO_CREAT, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} O_CREAT ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate that a file should be created if it\n\
does not exist.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end deftypefn")
{
#if defined (O_CREAT)
  return const_value (args, O_CREAT);
#else
  err_disabled_feature ("O_CREAT", "O_CREAT");
#endif
}

DEFUNX ("O_EXCL", FO_EXCL, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} O_EXCL ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate that file locking is used.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end deftypefn")
{
#if defined (O_EXCL)
  return const_value (args, O_EXCL);
#else
  err_disabled_feature ("O_EXCL", "O_EXCL");
#endif
}

DEFUNX ("O_NONBLOCK", FO_NONBLOCK, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} O_NONBLOCK ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate that non-blocking I/O is in use,\n\
or that may be passsed to @code{fcntl} to set non-blocking I/O.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_RDONLY, O_RDWR, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end deftypefn")
{
#if defined (O_NONBLOCK)
  return const_value (args, O_NONBLOCK);
#else
  err_disabled_feature ("O_NONBLOCK", "O_NONBLOCK");
#endif
}

DEFUNX ("O_RDONLY", FO_RDONLY, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} O_RDONLY ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate that a file is open for reading only.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDWR, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end deftypefn")
{
#if defined (O_RDONLY)
  return const_value (args, O_RDONLY);
#else
  err_disabled_feature ("O_RDONLY", "O_RDONLY");
#endif
}

DEFUNX ("O_RDWR", FO_RDWR, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} O_RDWR ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate that a file is open for both reading\n\
and writing.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end deftypefn")
{
#if defined (O_RDWR)
  return const_value (args, O_RDWR);
#else
  err_disabled_feature ("O_RDWR", "O_RDWR");
#endif
}

DEFUNX ("O_SYNC", FO_SYNC, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} O_SYNC ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate that a file is open for synchronous\n\
I/O.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_TRUNC, O_WRONLY}\n\
@end deftypefn")
{
#if defined (O_SYNC)
  return const_value (args, O_SYNC);
#else
  err_disabled_feature ("O_SYNC", "O_SYNC");
#endif
}

DEFUNX ("O_TRUNC", FO_TRUNC, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} O_TRUNC ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate that if file exists, it should be\n\
truncated when writing.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC, O_WRONLY}\n\
@end deftypefn")
{
#if defined (O_TRUNC)
  return const_value (args, O_TRUNC);
#else
  err_disabled_feature ("O_TRUNC", "O_TRUNC");
#endif
}

DEFUNX ("O_WRONLY", FO_WRONLY, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} O_WRONLY ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate that a file is open for writing only.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC, O_TRUNC}\n\
@end deftypefn")
{
#if defined (O_WRONLY)
  return const_value (args, O_WRONLY);
#else
  err_disabled_feature ("O_WRONLY", "O_WRONLY");
#endif
}

#if ! defined (WNOHANG)
#define WNOHANG 0
#endif

DEFUNX ("WNOHANG", FWNOHANG, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} WNOHANG ()\n\
Return the numerical value of the option argument that may be\n\
passed to @code{waitpid} to indicate that it should return its status\n\
immediately instead of waiting for a process to exit.\n\
@seealso{waitpid, WUNTRACED, WCONTINUE}\n\
@end deftypefn")
{
  return const_value (args, WNOHANG);
}

#if ! defined (WUNTRACED)
#define WUNTRACED 0
#endif

DEFUNX ("WUNTRACED", FWUNTRACED, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} WUNTRACED ()\n\
Return the numerical value of the option argument that may be\n\
passed to @code{waitpid} to indicate that it should also return if the child\n\
process has stopped but is not traced via the @code{ptrace} system call\n\
@seealso{waitpid, WNOHANG, WCONTINUE}\n\
@end deftypefn")
{
  return const_value (args, WUNTRACED);
}

#if ! defined (WCONTINUE)
#define WCONTINUE 0
#endif

DEFUNX ("WCONTINUE", FWCONTINUE, args, ,
        "-*- texinfo -*-\n\
@deftypefn {} {} WCONTINUE ()\n\
Return the numerical value of the option argument that may be\n\
passed to @code{waitpid} to indicate that it should also return if a stopped\n\
child has been resumed by delivery of a @code{SIGCONT} signal.\n\
@seealso{waitpid, WNOHANG, WUNTRACED}\n\
@end deftypefn")
{
  return const_value (args, WCONTINUE);
}
