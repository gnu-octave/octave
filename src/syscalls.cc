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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

// Thomas Baier <baier@ci.tuwien.ac.at> added the original versions of
// the following functions:
//
//   mkfifo  unlink  waitpid

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdio>
#include <cstring>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include "file-ops.h"
#include "file-stat.h"
#include "oct-syscalls.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "lo-utils.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "oct-stdstrm.h"
#include "oct-stream.h"
#include "sysdep.h"
#include "syswait.h"
#include "utils.h"
#include "variables.h"

static Octave_map
mk_stat_map (const file_stat& fs)
{
  Octave_map m;

  m.assign ("dev", static_cast<double> (fs.dev ()));
  m.assign ("ino", fs.ino ());
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

DEFUN (dup2, args, ,
 "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{fid}, @var{msg}] =} dup2 (@var{old}, @var{new})\n\
Duplicate a file descriptor.\n\
\n\
If successful, @var{fid} is greater than zero and contains the new file\n\
ID.  Otherwise, @var{fid} is negative and @var{msg} contains a\n\
system-dependent error message.\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 2)
    {
      octave_stream old_stream
	= octave_stream_list::lookup (args(0), "dup2");

      if (! error_state)
	{
	  octave_stream new_stream
	    = octave_stream_list::lookup (args(1), "dup2");

	  if (! error_state)
	    {
	      int i_old = old_stream.file_number ();
	      int i_new = new_stream.file_number ();

	      if (i_old >= 0 && i_new >= 0)
		{
		  std::string msg;

		  int status = octave_syscalls::dup2 (i_old, i_new, msg);

		  retval(0) = status;
		  retval(1) = msg;
		}
	    }
	}
      else
	error ("dup2: invalid stream");
    }
  else
    print_usage ("dup2");

  return retval;
}

DEFUN (exec, args, ,
 "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{err}, @var{msg}] =} exec (@var{file}, @var{args})\n\
Replace current process with a new process.  Calling @code{exec} without\n\
first calling @code{fork} will terminate your current Octave process and\n\
replace it with the program named by @var{file}.  For example,\n\
\n\
@example\n\
exec (\"ls\" \"-l\")\n\
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
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      std::string exec_file = args(0).string_value ();

      if (! error_state)
	{
	  string_vector exec_args;

	  if (nargin == 2)
	    {
	      string_vector tmp = args(1).all_strings ();

	      if (! error_state)
		{
		  int len = tmp.length ();

		  exec_args.resize (len + 1);

		  exec_args[0] = exec_file;

		  for (int i = 0; i < len; i++)
		    exec_args[i+1] = tmp[i];
		}
	      else
		error ("exec: arguments must be character strings");
	    }
	  else
	    {
	      exec_args.resize (1);

	      exec_args[0] = exec_file;
	    }

	  if (! error_state)
	    {
	      std::string msg;

	      int status = octave_syscalls::execvp (exec_file, exec_args, msg);

	      retval(0) = status;
	      retval(1) = msg;
	    }
	}
      else
	error ("exec: first argument must be a string");
    }
  else
    print_usage ("exec");

  return retval;
}

DEFUN (fcntl, args, ,
 "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{err}, @var{msg}] =} fcntl (@var{fid}, @var{request}, @var{arg})\n\
Change the properties of the open file @var{fid}.  The following values\n\
may be passed as @var{request}:\n\
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
Nonblocking mode.\n\
\n\
@item O_SYNC\n\
Wait for writes to complete.\n\
\n\
@item O_ASYNC\n\
Asynchronous I/O.\n\
@end vtable\n\
\n\
@item F_SETFL\n\
Set the file status flags for @var{fid} to the value specified by\n\
@var{arg}.  The only flags that can be changed are @code{O_APPEND} and\n\
@code{O_NONBLOCK}.\n\
@end vtable\n\
\n\
If successful, @var{err} is 0 and @var{msg} is an empty string.\n\
Otherwise, @var{err} is nonzero and @var{msg} contains a\n\
system-dependent error message.\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 3)
    {
      octave_stream strm = octave_stream_list::lookup (args (0), "fcntl");

      if (! error_state)
	{
	  int fid = strm.file_number ();

	  int req = args(1).int_value (true);
	  int arg = args(2).int_value (true);

	  if (! error_state)
	    {
	      // XXX FIXME XXX -- Need better checking here?
	      if (fid < 0)
		error ("fcntl: invalid file id");
	      else
		{
		  std::string msg;

		  int status = octave_syscalls::fcntl (fid, req, arg, msg);

		  retval(0) = status;
		  retval(1) = msg;
		}
	    }
	}
      else
	error ("fcntl: file id, request, and argument must be integers");
    }
  else
    print_usage ("fcntl");

  return retval;
}

DEFUN (fork, args, ,
 "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{pid}, @var{msg}] =} fork ()\n\
Create a copy of the current process.\n\
\n\
Fork can return one of the following values:\n\
\n\
@table @asis\n\
@item > 0\n\
You are in the parent process.  The value returned from @code{fork} is\n\
the process id of the child process.  You should probably arrange to\n\
wait for any child processes to exit.\n\
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
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 0)
    {
      std::string msg;

      pid_t pid = octave_syscalls::fork (msg);

      retval(0) = pid;
      retval(1) = msg;
    }
  else
    print_usage ("fork");

  return retval;
}

DEFUN (getpgrp, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {pgid =} getpgrp ()\n\
Return the process group id of the current process.\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 0)
    {
      std::string msg;

      retval(0) = octave_syscalls::getpgrp (msg);
      retval(1) = msg;
    }
  else
    print_usage ("getpgrp");

  return retval;
}

DEFUN (getpid, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {pid =} getpid ()\n\
Return the process id of the current process.\n\
@end deftypefn")
{
  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin == 0)
    retval = octave_syscalls::getpid ();
  else
    print_usage ("getpid");

  return retval;
}

DEFUN (getppid, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {pid =} getppid ()\n\
Return the process id of the parent process.\n\
@end deftypefn")
{
  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin == 0)
    retval = octave_syscalls::getppid ();
  else
    print_usage ("getppid");

  return retval;
}

DEFUN (getegid, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {egid =} getegid ()\n\
Return the effective group id of the current process.\n\
@end deftypefn")
{
  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin == 0)
    retval = octave_syscalls::getegid ();
  else
    print_usage ("getegid");

  return retval;
}

DEFUN (getgid, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {gid =} getgid ()\n\
Return the real group id of the current process.\n\
@end deftypefn")
{
  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin == 0)
    retval = octave_syscalls::getgid ();
  else
    print_usage ("getgid");

  return retval;
}

DEFUN (geteuid, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {euid =} geteuid ()\n\
Return the effective user id of the current process.\n\
@end deftypefn")
{
  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin == 0)
    retval = octave_syscalls::geteuid ();
  else
    print_usage ("geteuid");

  return retval;
}

DEFUN (getuid, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {uid =} getuid ()\n\
Return the real user id of the current process.\n\
@end deftypefn")
{
  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin == 0)
    retval = octave_syscalls::getuid ();
  else
    print_usage ("getuid");

  return retval;
}

DEFUN (kill, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{err}, @var{msg}] =} kill (@var{pid}, @var{sig})\n\
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
Return 0 if sucessful, otherwise return -1.\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1;

  if (args.length () == 2)
    {
      pid_t pid = args(0).int_value (true);

      if (! error_state)
	{
	  int sig = args(1).int_value (true);

	  if (! error_state)
	    {
	      std::string msg;

	      int status = octave_syscalls::kill (pid, sig, msg);

	      retval(1) = msg;
	      retval(0) = status;
	    }
	}
    }
  else
    print_usage ("kill");

  return retval;
}

DEFUN (lstat, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{info}, @var{err}, @var{msg}] =} lstat (@var{file})\n\
See stat.\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      std::string fname = file_ops::tilde_expand (args(0).string_value ());

      if (! error_state)
	{
	  file_stat fs (fname, false);

	  if (fs)
	    {
	      retval(2) = std::string ();
	      retval(1) = 0;
	      retval(0) = mk_stat_map (fs);
	    }
	  else
	    {
	      retval(2) = fs.error ();
	      retval(1) = -1;
	      retval(0) = Matrix ();
	    }
	}
    }
  else
    print_usage ("lstat");

  return retval;
}



DEFUN (mkfifo, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{err}, @var{msg}] =} mkfifo (@var{name}, @var{mode})\n\
Create a @var{fifo} special file named @var{name} with file mode @var{mode}\n\
\n\
If successful, @var{err} is 0 and @var{msg} is an empty string.\n\
Otherwise, @var{err} is nonzero and @var{msg} contains a\n\
system-dependent error message.\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 2)
    {
      if (args(0).is_string ())
	{
	  std::string name = args(0).string_value ();

	  if (args(1).is_scalar_type ())
	    {
	      long mode = args(1).long_value ();

	      if (! error_state)
		{
		  std::string msg;

		  int status = file_ops::mkfifo (name, mode, msg);

		  retval(0) = status;

		  if (status < 0)
		    retval(1) = msg;
		}
	      else
		error ("mkfifo: invalid MODE");
	    }
	  else
	    error ("mkfifo: MODE must be an integer");
	}
      else
	error ("mkfifo: file name must be a string");
    }
  else
    print_usage ("mkfifo");

  return retval;
}

DEFUN (pipe, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{file_ids}, @var{err}, @var{msg}] =} pipe ()\n\
Create a pipe and return the vector @var{file_ids}, which corresponding\n\
to the reading and writing ends of the pipe.\n\
\n\
If successful, @var{err} is 0 and @var{msg} is an empty string.\n\
Otherwise, @var{err} is nonzero and @var{msg} contains a\n\
system-dependent error message.\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(2) = std::string ();
  retval(1) = -1;
  retval(0) = Matrix ();

  int nargin = args.length ();

  if (nargin == 0)
    {
      int fid[2];

      std::string msg;

      int status = octave_syscalls::pipe (fid, msg);

      if (status < 0)
	retval(2) = msg;
      else
	{
	  FILE *ifile = fdopen (fid[0], "r");
	  FILE *ofile = fdopen (fid[1], "w");

	  std::string nm;

	  octave_stream is = octave_stdiostream::create (nm, ifile,
							 std::ios::in);

	  octave_stream os = octave_stdiostream::create (nm, ofile,
							 std::ios::out);

	  octave_value_list file_ids;

	  file_ids(1) = octave_stream_list::insert (os);
	  file_ids(0) = octave_stream_list::insert (is);

	  retval(1) = status;
          retval(0) = octave_value (file_ids);
	}
    }
  else
    print_usage ("pipe");

  return retval;
}

DEFUN (stat, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{info}, @var{err}, @var{msg}] =} stat (@var{file})\n\
@deftypefnx {Built-in Function} {[@var{info}, @var{err}, @var{msg}] =} lstat (@var{file})\n\
Return a structure @var{s} containing the following information about\n\
@var{file}.\n\
\n\
@table @code\n\
@item dev\n\
ID of device containing a directory entry for this file.\n\
\n\
@item ino\n\
File number of the file.\n\
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
If the call is successful @var{err} is 0 and @var{msg} is an empty\n\
string.  If the file does not exist, or some other error occurs, @var{s}\n\
is an empty matrix, @var{err} is @minus{}1, and @var{msg} contains the\n\
corresponding system error message.\n\
\n\
If @var{file} is a symbolic link, @code{stat} will return information\n\
about the actual file the is referenced by the link.  Use @code{lstat}\n\
if you want information about the symbolic link itself.\n\
\n\
For example,\n\
\n\
@example\n\
@group\n\
[s, err, msg] = stat (\"/vmlinuz\")\n\
      @result{} s =\n\
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
          modestr = -rw-r--r--\n\
          ino = 9316\n\
          dev = 2049\n\
        @}\n\
     @result{} err = 0\n\
     @result{} msg = \n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      std::string fname = file_ops::tilde_expand (args(0).string_value ());

      if (! error_state)
	{
	  file_stat fs (fname);

	  if (fs)
	    {
	      retval(2) = std::string ();
	      retval(1) = 0;
	      retval(0) = octave_value (mk_stat_map (fs));
	    }
	  else
	    {
	      retval(2) = fs.error ();
	      retval(1) = -1;
	      retval(0) = Matrix ();
	    }
	}
    }
  else
    print_usage ("stat");

  return retval;
}

DEFUN (unlink, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{err}, @var{msg}] =} unlink (@var{file})\n\
Delete the file named @var{file}.\n\
\n\
If successful, @var{err} is 0 and @var{msg} is an empty string.\n\
Otherwise, @var{err} is nonzero and @var{msg} contains a\n\
system-dependent error message.\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 1)
    {
      if (args(0).is_string ())
	{
	  std::string name = args(0).string_value ();

	  std::string msg;

	  int status = file_ops::unlink (name, msg);

	  retval(0) = status;
	  retval(1) = msg;	    
	}
      else
	error ("unlink: file name must be a string");
    }
  else
    print_usage ("unlink");

  return retval;
}

DEFUN (waitpid, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{pid}, @var{msg}] =} waitpid (@var{pid}, @var{options})\n\
Wait for process @var{pid} to terminate.  The @var{pid} argument can be:\n\
\n\
@table @asis\n\
@item @minus{}1\n\
Wait for any child process.\n\
\n\
@item 0\n\
Wait for any child process whose process group ID is equal to that of\n\
the Octave interpreter process.\n\
\n\
@item > 0\n\
Wait for termination of the child process with ID @var{pid}.\n\
@end table\n\
\n\
The @var{options} argument can be:\n\
\n\
@table @asis\n\
@item 0\n\
Wait until signal is received or a child process exits (this is the\n\
default if the @var{options} argument is missing).\n\
\n\
@item 1\n\
Do not hang if status is not immediately available.\n\
\n\
@item 2\n\
Report the status of any child processes that are stopped, and whose\n\
status has not yet been reported since they stopped.\n\
\n\
@item 3\n\
Implies both 1 and 2.\n\
@end table\n\
\n\
If the returned value of @var{pid} is greater than 0, it is the process\n\
ID of the child process that exited.  If an error occurs, @var{pid} will\n\
be less than zero and @var{msg} will contain a system-dependent error\n\
message.\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      pid_t pid = args(0).int_value (true);
  
      if (! error_state)
	{
	  int options = 0;

	  if (args.length () == 2)
	    {
	      options = args(1).int_value (true);

	      if (! error_state)
		{
		  if (options < 0 || options > 3)
		    error ("waitpid: invalid OPTIONS value specified");
		}
	      else
		error ("waitpid: OPTIONS must be in integer");
	    }

	  if (! error_state)
	    {
	      std::string msg;

	      pid_t status = octave_syscalls::waitpid (pid, options, msg);

	      retval(0) = status;
	      retval(1) = msg;
	    }
	}
      else
	error ("waitpid: PID must be an integer value");
    }
  else
    print_usage ("waitpid");

  return retval;
}

DEFUN (canonicalize_file_name, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{cname}, @var{status}, @var{msg}]} canonicalize_file_name (@var{name})\n\
Return the canonical name of file @var{name}.\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      std::string name = args(0).string_value ();

      if (! error_state)
	{
	  std::string msg;

	  std::string result = file_ops::canonicalize_file_name (name, msg);

	  retval(2) = msg;
	  retval(1) = msg.empty () ? 0 : -1;
	  retval(0) = result;
	}
      else
	error ("canonicalize_file_name: argument must be a character string");
    }
  else
    print_usage ("canonicalize_file_name");

  return retval;
}

#if !defined (O_NONBLOCK) && defined (O_NDELAY)
#define O_NONBLOCK O_NDELAY
#endif

void
symbols_of_syscalls (void)
{
#if defined (F_DUPFD)
  DEFCONSTX ("F_DUPFD", SBV_F_DUPFD, F_DUPFD,
    "-*- texinfo -*-\n\
@defvr {Built-in Constant} F_DUPFD\n\
Request to @code{fcntl} to return a duplicate file descriptor.\n\
@seealso{fcntl, F_GETFD, F_GETFL, F_SETFD, F_SETFL}\n\
@end defvr");
#endif

#if defined (F_GETFD)
  DEFCONSTX ("F_GETFD", SBV_F_GETFD, F_GETFD,
    "-*- texinfo -*-\n\
@defvr {Built-in Constant} F_GETFD\n\
Request to @code{fcntl} to return the file descriptor flags.\n\
@seealso{fcntl, F_DUPFD, F_GETFL, F_SETFD, F_SETFL}\n\
@end defvr");
#endif

#if defined (F_GETFL)
  DEFCONSTX ("F_GETFL", SBV_F_GETFL, F_GETFL,
    "-*- texinfo -*-\n\
@defvr {Built-in Constant} F_GETFL\n\
Request to @code{fcntl} to return the file status flags.\n\
@seealso{fcntl, F_DUPFD, F_GETFD, F_SETFD, F_SETFL}\n\
@end defvr");
#endif

#if defined (F_SETFD)
  DEFCONSTX ("F_SETFD", SBV_F_SETFD, F_SETFD,
    "-*- texinfo -*-\n\
@defvr {Built-in Constant} F_SETFD\n\
Request to @code{fcntl} to set the file descriptor flags.\n\
@seealso{fcntl, F_DUPFD, F_GETFD, F_GETFL, F_SETFL}\n\
@end defvr");
#endif

#if defined (F_SETFL)
  DEFCONSTX ("F_SETFL", SBV_F_SETFL, F_SETFL,
    "-*- texinfo -*-\n\
@defvr {Built-in Constant} F_SETFL\n\
Request to @code{fcntl} to set the file status flags.\n\
@seealso{fcntl, F_DUPFD, F_GETFD, F_GETFL, F_SETFD}\n\
@end defvr");
#endif

#if defined (O_APPEND)
  DEFCONSTX ("O_APPEND", SBV_O_APPEND, O_APPEND,
    "-*- texinfo -*-\n\
@defvr {Built-in Constant} O_APPEND\n\
File status flag, append on each write.\n\
@seealso{fcntl, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end defvr");
#endif

#if defined (O_ASYNC)
  DEFCONSTX ("O_ASYNC", SBV_O_ASYNC, O_ASYNC,
    "-*- texinfo -*-\n\
@defvr {Built-in Constant} O_ASYNC\n\
File status flag, asynchronous I/O.\n\
@seealso{fcntl, O_APPEND, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end defvr");
#endif

#if defined (O_CREAT)
  DEFCONSTX ("O_CREAT", SBV_O_CREAT, O_CREAT,
    "-*- texinfo -*-\n\
@defvr {Built-in Constant} O_CREAT\n\
File status flag, create file if it does not exist.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end defvr");
#endif

#if defined (O_EXCL)
  DEFCONSTX ("O_EXCL", SBV_O_EXCL, O_EXCL,
    "-*- texinfo -*-\n\
@defvr {Built-in Constant} O_EXCL\n\
File status flag, file locking.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end defvr");
#endif

#if defined (O_NONBLOCK)
  DEFCONSTX ("O_NONBLOCK", SBV_O_NONBLOCK, O_NONBLOCK,
    "-*- texinfo -*-\n\
@defvr {Built-in Constant} O_NONBLOCK\n\
File status flag, non-blocking I/O.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_RDONLY, O_RDWR, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end defvr");
#endif

#if defined (O_RDONLY)
  DEFCONSTX ("O_RDONLY", SBV_O_RDONLY, O_RDONLY,
    "-*- texinfo -*-\n\
@defvr {Built-in Constant} O_RDONLY\n\
File status flag, file opened for reading only.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDWR, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end defvr");
#endif

#if defined (O_RDWR)
  DEFCONSTX ("O_RDWR", SBV_O_RDWR, O_RDWR,
    "-*- texinfo -*-\n\
@defvr {Built-in Constant} O_RDWR\n\
File status flag, file open for both reading and writing.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end defvr");
#endif

#if defined (O_SYNC)
  DEFCONSTX ("O_SYNC", SBV_O_SYNC, O_SYNC,
    "-*- texinfo -*-\n\
@defvr {Built-in Constant} O_SYNC\n\
File status flag, file opened for synchronous I/O.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_TRUNC, O_WRONLY}\n\
@end defvr");
#endif

#if defined (O_TRUNC)
  DEFCONSTX ("O_TRUNC", SBV_O_TRUNC, O_TRUNC,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} O_TRUNC\n\
File status flag, if file exists, truncate it when writing.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC, O_WRONLY}\n\
@end defvr");
#endif

#if defined (O_WRONLY)
  DEFCONSTX ("O_WRONLY", SBV_O_WRONLY, O_WRONLY,
    "-*- texinfo -*-\n\
@defvr {Built-in Constant} O_WRONLY\n\
File status flag, file opened for writing only.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC, O_TRUNC}\n\
@end defvr");
#endif

}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
