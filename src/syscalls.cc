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

// Thomas Baier <baier@ci.tuwien.ac.at> added the original versions of
// the following functions:
//
//   mkfifo  unlink  waitpid

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cerrno>
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

  m["dev"] = static_cast<double> (fs.dev ());
  m["ino"] = static_cast<double> (fs.ino ());
  m["modestr"] = fs.mode_as_string ();
  m["nlink"] = static_cast<double> (fs.nlink ());
  m["uid"] = static_cast<double> (fs.uid ());
  m["gid"] = static_cast<double> (fs.gid ());
#if defined (HAVE_ST_RDEV)
  m["rdev"] = static_cast<double> (fs.rdev ());
#endif
  m["size"] = static_cast<double> (fs.size ());
  m["atime"] = static_cast<double> (fs.atime ());
  m["mtime"] = static_cast<double> (fs.mtime ());
  m["ctime"] = static_cast<double> (fs.ctime ());
#if defined (HAVE_ST_BLKSIZE)
  m["blksize"] = static_cast<double> (fs.blksize ());
#endif
#if defined (HAVE_ST_BLOCKS)
  m["blocks"] = static_cast<double> (fs.blocks ());
#endif

  return m;
}

DEFUN (dup2, args, ,
 "[FID, MSG] = dup2 (OLD, NEW)\n\
\n\
Duplicate a file descriptor.\n\
\n\
If successful, FID is greater than zero and contains the new file ID.\n\
Otherwise, FID is negative and MSG contains a system-dependent error message.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 2)
    {
      octave_stream *old_stream = octave_stream_list::lookup (args(0));
      octave_stream *new_stream = octave_stream_list::lookup (args(1));

      if (! error_state)
	{
	  int i_old = old_stream->file_number ();
	  int i_new = new_stream->file_number ();

	  if (i_old >= 0 && i_new >= 0)
	    {
	      string msg;

	      int status = octave_syscalls::dup2 (i_old, i_new, msg);

	      retval(0) = static_cast<double> (status);
	      retval(1) = msg;
	    }
	  else
	    error ("dup2: invalid file id");
	}
      else
	error ("dup2: invalid stream");
    }
  else
    print_usage ("dup2");

  return retval;
}

DEFUN (exec, args, ,
 "[STATUS, MSG] = exec (FILE, ARGS)\n\
\n\
Replace current process with a new process.\n\
\n\
If successful, exec does not return.  If exec does return, status will\n\
be nonzero, and MSG will contain a system-dependent error message.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      string exec_file = args(0).string_value ();

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
		error ("exec: arguments must be strings");
	    }
	  else
	    {
	      exec_args.resize (1);

	      exec_args[0] = exec_file;
	    }

	  if (! error_state)
	    {
	      string msg;

	      int status = octave_syscalls::execvp (exec_file, exec_args, msg);

	      retval(0) = static_cast<double> (status);
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
 "[STATUS, MSG] = fcntl (FID, REQUEST, ARGUMENT)\n\
\n\
Control open file descriptors.\n\
\n\
If successful, STATUS is 0 and MSG is an empty string.  Otherwise,\n\
STATUS is nonzero and MSG contains a system-dependent error message.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 3)
    {
      double d_fid = args(0).double_value ();
      double d_req = args(1).double_value ();
      double d_arg = args(2).double_value ();

      if (! error_state
	  && D_NINT (d_fid) == d_fid
	  && D_NINT (d_req) == d_req
	  && D_NINT (d_arg) == d_arg)
	{
	  int fid = NINT (d_fid);
	  int req = NINT (d_req);
	  int arg = NINT (d_arg);

	  // XXX FIXME XXX -- Need better checking here?
	  if (fid < 0)
	    error ("fcntl: invalid file id");
	  else
	    {
	      string msg;

	      int status = octave_syscalls::fcntl (fid, req, arg, msg);

	      retval(0) = static_cast<double> (status);
	      retval(1) = msg;
	    }
	}
      else
	error ("fcntl: file id must be an integer");
    }
  else
    print_usage ("fcntl");

  return retval;
}

DEFUN (fork, args, ,
 "[PID, MSG] = fork ()\n\
\n\
Create a copy of the current process.\n\
\n\
If successful, PID is either the process ID and you are in the parent,\n\
or 0, and you are in the child.  If PID is less than zero, an error\n\
has occured, and MSG contains a system-dependent error message.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 0)
    {
      string msg;

      pid_t pid = octave_syscalls::fork (msg);

      retval(0) = static_cast<double> (pid);
      retval(1) = msg;
    }
  else
    print_usage ("fork");

  return retval;
}

DEFUN (vfork, args, ,
 "[PID, MSG] = vfork ()\n\
\n\
Create a copy of the current process.\n\
\n\
If successful, PID is either the process ID and you are in the parent,\n\
or 0, and you are in the child.  If PID is less than zero, an error\n\
has occured, and MSG contains a system-dependent error message.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 0)
    {
      string msg;

      pid_t pid = octave_syscalls::vfork (msg);

      retval(0) = static_cast<double> (pid);
      retval(1) = msg;
    }
  else
    print_usage ("vfork");

  return retval;
}

DEFUN (getpgrp, args, ,
  "pgid = getpgrp (): return the process group id of the current process")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 0)
    {
      string msg;

      retval(0) = static_cast<double> (octave_syscalls::getpgrp (msg));
      retval(1) = msg;
    }
  else
    print_usage ("getpgrp");

  return retval;
}

DEFUN (getpid, args, ,
  "pid = getpid (): return the process id of the current process")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 0)
    retval = octave_syscalls::getpid ();
  else
    print_usage ("getpid");

  return retval;
}

DEFUN (getppid, args, ,
  "pid = getppid (): return the process id of the parent process")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 0)
    retval = octave_syscalls::getppid ();
  else
    print_usage ("getppid");

  return retval;
}

DEFUN (getegid, args, ,
  "gid = getegid (): return the effective group id of the current process")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 0)
    retval = octave_syscalls::getegid ();
  else
    print_usage ("getegid");

  return retval;
}

DEFUN (getgid, args, ,
  "gid = getgid (): return the real group id of the current process")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 0)
    retval = octave_syscalls::getgid ();
  else
    print_usage ("getgid");

  return retval;
}

DEFUN (geteuid, args, ,
  "uid = geteuid (): return the effective user id of the current process")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 0)
    retval = octave_syscalls::geteuid ();
  else
    print_usage ("geteuid");

  return retval;
}

DEFUN (getuid, args, ,
  "uid = getuid (): return the real user id of the current process")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 0)
    retval = octave_syscalls::getuid ();
  else
    print_usage ("getuid");

  return retval;
}

DEFUN (lstat, args, ,
  "[S, ERR, MSG] = lstat (NAME)\n\
\n\
Like [S, ERR, MSG] = stat (NAME), but if NAME refers to a symbolic\n\
link, returns information about the link itself, not the file that it\n\
points to.")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      string fname = file_ops::tilde_expand (args(0).string_value ());

      if (! error_state)
	{
	  file_stat fs (fname, false);

	  if (fs)
	    {
	      retval(2) = string ();
	      retval(1) = 0.0;
	      retval(0) = octave_value (mk_stat_map (fs));
	    }
	  else
	    {
	      retval(2) = fs.error ();
	      retval(1) = -1.0;
	      retval(0) = Matrix ();
	    }
	}
    }
  else
    print_usage ("lstat");

  return retval;
}

DEFUN (mkfifo, args, ,
  "[STATUS, MSG] = mkfifo (NAME, MODE)\n\
\n\
Create a FIFO special file named NAME with file mode MODE\n\
\n\
If successful, STATUS is 0 and MSG is an empty string.  Otherwise,\n\
STATUS is nonzero and MSG contains a system-dependent error message.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 2)
    {
      if (args(0).is_string ())
	{
	  string name = args(0).string_value ();

	  if (args(1).is_scalar_type ())
	    {
	      long mode = static_cast<long> (args(1).double_value ());

	      string msg;

	      int status = file_ops::mkfifo (name, mode, msg);

	      retval(0) = static_cast<double> (status);

	      if (status < 0)
		retval(1) = msg;
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
  "[FILE_LIST, STATUS, MSG] = pipe (): create an interprocess channel.\n\
\n\
Return the file objects corresponding to the reading and writing ends of\n\
the pipe, as a two-element list.\n\
\n\
If successful, STATUS is 0 and MSG is an empty string.  Otherwise,\n\
STATUS is nonzero and MSG contains a system-dependent error message.")
{
  octave_value_list retval;

  retval(2) = string ();
  retval(1) = -1.0;
  retval(0) = Matrix ();

  int nargin = args.length ();

  if (nargin == 0)
    {
      int fid[2];

      string msg;

      int status = octave_syscalls::pipe (fid, msg);

      if (status < 0)
	retval(2) = msg;
      else
	{
	  FILE *in_file = fdopen (fid[0], "r");
	  FILE *out_file = fdopen (fid[1], "w");

	  octave_istdiostream *is
	    = new octave_istdiostream (string (), in_file);

	  octave_ostdiostream *os
	    = new octave_ostdiostream (string (), out_file);

	  octave_value_list file_ids;

	  file_ids(1) = octave_stream_list::insert (os);
	  file_ids(0) = octave_stream_list::insert (is);

	  retval(1) = static_cast<double> (status);
          retval(0) = octave_value (file_ids);
	}
    }
  else
    print_usage ("pipe");

  return retval;
}

DEFUN (stat, args, ,
  "[S, ERR, MSG] = stat (NAME)\n\
\n\
  Given the name of a file, return a structure S with the following\n\
  elements:\n\
\n\
    dev     : id of device containing a directory entry for this file\n\
    ino     : file number of the file\n\
    modestr : file mode, as a string of ten letters or dashes as in ls -l\n\
    nlink   : number of links\n\
    uid     : user id of file's owner\n\
    gid     : group id of file's group \n\
    rdev    : id of device for block or character special files\n\
    size    : size in bytes\n\
    atime   : time of last access\n\
    mtime   : time of last modification\n\
    ctime   : time of last file status change\n\
    blksize : size of blocks in the file\n\
    blocks  : number of blocks allocated for file\n\
\n\
  If the call is successful, ERR is 0 and MSG is an empty string.\n\
\n\
  If the file does not exist, or some other error occurs, S is an\n\
  empty matrix, ERR is -1, and MSG contains the corresponding\n\
  system error message.")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      string fname = file_ops::tilde_expand (args(0).string_value ());

      if (! error_state)
	{
	  file_stat fs (fname);

	  if (fs)
	    {
	      retval(2) = string ();
	      retval(1) = 0.0;
	      retval(0) = octave_value (mk_stat_map (fs));
	    }
	  else
	    {
	      retval(2) = fs.error ();
	      retval(1) = -1.0;
	      retval(0) = Matrix ();
	    }
	}
    }
  else
    print_usage ("stat");

  return retval;
}

DEFUN (unlink, args, ,
  "[STATUS, MSG] = unlink (NAME)\n\
\n\
Delete the file NAME\n\
\n\
If successful, STATUS is 0 and MSG is an empty string.  Otherwise,\n\
STATUS is nonzero and MSG contains a system-dependent error message.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 1)
    {
      if (args(0).is_string ())
	{
	  string name = args(0).string_value ();

	  string msg;

	  int status = file_ops::unlink (name, msg);

	  retval(0) = static_cast<double> (status);
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
  "[PID, MSG] = waitpid (PID, OPTIONS)\n\
\n\
Wait for process PID to terminate\n\
\n\
  PID can be:\n\
\n\
     -1 : wait for any child process\n\
      0 : wait for any child process whose process group ID is equal to\n\
          that of the Octave interpreter process.\n\
    > 0 : wait for termination of the child process with ID PID.\n\
\n\
  OPTIONS is:\n\
\n\
     0 : wait until signal is received or a child process exits (this\n\
         is the default if the OPTIONS argument is missing) \n\
     1 : do not hang if status is not immediately available\n\
     2 : report the status of any child processes that are\n\
         stopped, and whose status has not yet been reported\n\
         since they stopped\n\
     3 : implies both 1 and 2\n\
\n\
If successful, PID is greater than 0 and contains the process ID of\n\
the child process that exited and MSG is an empty string.\n\
Otherwise, PID is less than zero and MSG contains a system-dependent\n\
error message.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      double pid_num = args(0).double_value ();
  
      if (! error_state)
	{
	  if (D_NINT (pid_num) != pid_num)
	    error ("waitpid: PID must be an integer value");
	  else
	    {
	      pid_t pid = (pid_t) pid_num;

	      int options = 0;

	      if (args.length () == 2)
		{
		  double options_num = args(1).double_value ();

		  if (! error_state)
		    {
		      if (D_NINT (options_num) != options_num)
			error ("waitpid: PID must be an integer value");
		      else
			{
			  options = NINT (options_num);
			  if (options < 0 || options > 3)
			    error ("waitpid: invalid OPTIONS value specified");
			}
		    }
		}

	      if (! error_state)
		{
		  string msg;

		  pid_t status
		    = octave_syscalls::waitpid (pid, options, msg);

		  retval(0) = static_cast<double> (status);
		  retval(1) = msg;
		}
	    }
	}
    }
  else
    print_usage ("waitpid");

  return retval;
}

#if !defined (O_NONBLOCK) && defined (O_NDELAY)
#define O_NONBLOCK O_NDELAY
#endif

void
symbols_of_syscalls (void)
{
#if defined (F_DUPFD)
  DEFCONST (F_DUPFD, static_cast<double> (F_DUPFD),
    "");
#endif

#if defined (F_GETFD)
  DEFCONST (F_GETFD, static_cast<double> (F_GETFD),
    "");
#endif

#if defined (F_GETFL)
  DEFCONST (F_GETFL, static_cast<double> (F_GETFL),
    "");
#endif

#if defined (F_SETFD)
  DEFCONST (F_SETFD, static_cast<double> (F_SETFD),
    "");
#endif

#if defined (F_SETFL)
  DEFCONST (F_SETFL, static_cast<double> (F_SETFL),
    "");
#endif

#if defined (O_APPEND)
  DEFCONST (O_APPEND, static_cast<double> (O_APPEND),
    "");
#endif

#if defined (O_ASYNC)
  DEFCONST (O_ASYNC, static_cast<double> (O_ASYNC),
    "");
#endif

#if defined (O_CREAT)
  DEFCONST (O_CREAT, static_cast<double> (O_CREAT),
    "");
#endif

#if defined (O_EXCL)
  DEFCONST (O_EXCL, static_cast<double> (O_EXCL),
    "");
#endif

#if defined (O_NONBLOCK)
  DEFCONST (O_NONBLOCK, static_cast<double> (O_NONBLOCK),
    "");
#endif

#if defined (O_RDONLY)
  DEFCONST (O_RDONLY, static_cast<double> (O_RDONLY),
    "");
#endif

#if defined (O_RDWR)
  DEFCONST (O_RDWR, static_cast<double> (O_RDWR),
    "");
#endif

#if defined (O_SYNC)
  DEFCONST (O_SYNC, static_cast<double> (O_SYNC),
    "");
#endif

#if defined (O_TRUNC)
  DEFCONST (O_TRUNC, static_cast<double> (O_TRUNC),
    "");
#endif

#if defined (O_WRONLY)
  DEFCONST (O_WRONLY, static_cast<double> (O_WRONLY),
    "");
#endif
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
