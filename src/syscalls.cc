/*

Copyright (C) 1996 John W. Eaton

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

#include "defun.h"
#include "error.h"
#include "file-ops.h"
#include "gripes.h"
#include "help.h"
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

  m["dev"] = (double) fs.dev ();
  m["ino"] = (double) fs.ino ();
  m["modestr"] = fs.mode_as_string ();
  m["nlink"] = (double) fs.nlink ();
  m["uid"] = (double) fs.uid ();
  m["gid"] = (double) fs.gid ();
#if defined (HAVE_ST_RDEV)
  m["rdev"] = (double) fs.rdev ();
#endif
  m["size"] = (double) fs.size ();
  m["atime"] = (double) fs.atime ();
  m["mtime"] = (double) fs.mtime ();
  m["ctime"] = (double) fs.ctime ();
#if defined (HAVE_ST_BLKSIZE)
  m["blksize"] = (double) fs.blksize ();
#endif
#if defined (HAVE_ST_BLOCKS)
  m["blocks"] = (double) fs.blocks ();
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
#if defined (HAVE_DUP2)
      double d_old = args(0).double_value ();
      double d_new = args(1).double_value ();

      if (! error_state)
	{
	  if (D_NINT (d_old) == d_old && D_NINT (d_new) == d_new)
	    {
	      int i_old = NINT (d_old);
	      int i_new = NINT (d_new);

	      // XXX FIXME XXX -- are these checks sufficient?
	      if (i_old >= 0 && i_new >= 0)
		{
		  int status = dup2 (i_old, i_new);

		  retval(0) = (double) status;

		  if (status < 0)
		    retval(1) = strerror (errno);
		}
	      else
		error ("dup2: invalid file id");
	    }
	  else
	    error ("dup2: arguments must be integer values");
	}
#else
      gripe_not_supported ("dup2");
#endif
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
If successful, exec does not return.  If exec does return, status will
be nonzero, and MSG will contain a system-dependent error message.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
#if defined (HAVE_EXECVP)
      string exec_file = args(0).string_value ();

      if (! error_state)
	{
	  char **exec_args = 0;

	  if (nargin == 2)
	    {
	      charMatrix chm = args(1).all_strings ();

	      if (! error_state)
		{
		  int nr = chm.rows ();
		  int nc = chm.cols ();

		  exec_args = new char * [nr+2];

		  // XXX FIXME XXX -- potential leak?

		  exec_args[0] = strsave (exec_file.c_str ());
		  exec_args[nr+1] = 0;

		  for (int i = 0; i < nr; i++)
		    {
		      exec_args[i+1] = new char [nc+1];

		      for (int j = 0; j < nc; j++)
			exec_args[i+1][j] = chm (i, j);

		      exec_args[i+1][nc] = '\0';
		    }
		}
	      else
		error ("exec: arguments must be strings");
	    }
	  else
	    {
	      exec_args = new char * [2];

	      exec_args[0] = strsave (exec_file.c_str ());
	      exec_args[1] = 0;
	    }

	  if (! error_state)
	    {
	      int status = execvp (exec_file.c_str (), exec_args);

	      retval(0) = (double) status;

	      if (status < 0)
		retval(1) = strerror (errno);
	    }
	}
      else
	error ("exec: first argument must be a string");
#else
      gripe_not_supported ("exec");
#endif
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
#if defined (HAVE_FCNTL)
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
	      int status = fcntl (fid, req, arg);

	      retval(0) = (double) status;

	      if (status < 0)
		retval(1) = strerror (errno);
	    }
	}
      else
	error ("fcntl: file id must be an integer");
#else
      gripe_not_supported ("fcntl");
#endif
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
#if defined (HAVE_FORK)
      pid_t pid = fork ();

      retval(0) = (double) pid;

      if (pid < 0)
	retval(1) = strerror (errno);
#else
      gripe_not_supported ("fork");
#endif
    }
  else
    print_usage ("fork");

  return retval;
}

DEFUN (getpgrp, args, ,
  "pgid = getpgrp (): return the process group id of the current process")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 0)
    {
#if defined (HAVE_GETPGRP)
      retval = getpgrp ();
#else
      gripe_not_supported ("getpgrp");
#endif
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
    {
#if defined (HAVE_GETPID)
      retval = getpid ();
#else
      gripe_not_supported ("getpid");
#endif
    }
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
    {
#if defined (HAVE_GETPPID)
      retval = getppid ();
#else
      gripe_not_supported ("getppid");
#endif
    }
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
    {
#if defined (HAVE_GETEGID)
      retval = getegid ();
#else
      gripe_not_supported ("getegid");
#endif
    }
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
    {
#if defined (HAVE_GETGID)
      retval = getgid ();
#else
      gripe_not_supported ("getgid");
#endif
    }
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
    {
#if defined (HAVE_GETEUID)
      retval = geteuid ();
#else
      gripe_not_supported ("geteuid");
#endif
    }
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
    {
#if defined (HAVE_GETUID)
      retval = getuid ();
#else
      gripe_not_supported ("getuid");
#endif
    }
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
      string fname = oct_tilde_expand (args(0).string_value ());

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
	      long mode = (long) args(1).double_value ();

	      string msg;

	      int status = oct_mkfifo (name, mode, msg);

	      retval(0) = (double) status;

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
  "[FILE_IDS, STATUS, MSG] = pipe (): create an interprocess channel.\n\
\n\
Return the FILE_IDS corresponding to the reading and writing ends of\n\
the pipe, as a vector.\n\
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
#if defined (HAVE_PIPE)
      int fid[2];

      int status = pipe (fid);

      if (status < 0)
	{
	  retval(2) = strerror (errno);
	}
      else
	{
	  FILE *in_file = fdopen (fid[0], "r");
	  FILE *out_file = fdopen (fid[1], "w");

	  octave_istdiostream *is
	    = new octave_istdiostream (string (), in_file);

	  octave_ostdiostream *os
	    = new octave_ostdiostream (string (), out_file);

	  Matrix file_ids (1, 2);

	  file_ids (0, 0) = octave_stream_list::insert (is);
	  file_ids (0, 1) = octave_stream_list::insert (os);

          retval(0) = file_ids;
	  retval(1) = (double) status;
	}
#else
      gripe_not_supported ("pipe");
#endif
    }
  else
    print_usage ("pipe");

  return retval;
}

DEFUN (stat, args, ,
  "[S, ERR, MSG] = stat (NAME)\n\
\n\
  Given the name of a file, return a structure S with the following
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
      string fname = oct_tilde_expand (args(0).string_value ());

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

	  int status = oct_unlink (name, msg);

	  retval(0) = (double) status;

	  if (status < 0)
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
#if defined (HAVE_WAITPID)
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
		  pid_t status = waitpid (pid, 0, options);

		  retval(0) = (double) status;

		  if (status < 0)
		    retval(1) = strerror (errno);
		}
	    }
	}
#else
      gripe_not_supported ("waitpid");
#endif
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
  DEFCONST (F_DUPFD, (double) F_DUPFD, 0, 0,
    "");
#endif

#if defined (F_GETFD)
  DEFCONST (F_GETFD, (double) F_GETFD, 0, 0,
    "");
#endif

#if defined (F_GETFL)
  DEFCONST (F_GETFL, (double) F_GETFL, 0, 0,
    "");
#endif

#if defined (F_SETFD)
  DEFCONST (F_SETFD, (double) F_SETFD, 0, 0,
    "");
#endif

#if defined (F_SETFL)
  DEFCONST (F_SETFL, (double) F_SETFL, 0, 0,
    "");
#endif

#if defined (O_APPEND)
  DEFCONST (O_APPEND, (double) O_APPEND, 0, 0,
    "");
#endif

#if defined (O_ASYNC)
  DEFCONST (O_ASYNC, (double) O_ASYNC, 0, 0,
    "");
#endif

#if defined (O_CREAT)
  DEFCONST (O_CREAT, (double) O_CREAT, 0, 0,
    "");
#endif

#if defined (O_EXCL)
  DEFCONST (O_EXCL, (double) O_EXCL, 0, 0,
    "");
#endif

#if defined (O_NONBLOCK)
  DEFCONST (O_NONBLOCK, (double) O_NONBLOCK, 0, 0,
    "");
#endif

#if defined (O_RDONLY)
  DEFCONST (O_RDONLY, (double) O_RDONLY, 0, 0,
    "");
#endif

#if defined (O_RDWR)
  DEFCONST (O_RDWR, (double) O_RDWR, 0, 0,
    "");
#endif

#if defined (O_SYNC)
  DEFCONST (O_SYNC, (double) O_SYNC, 0, 0,
    "");
#endif

#if defined (O_TRUNC)
  DEFCONST (O_TRUNC, (double) O_TRUNC, 0, 0,
    "");
#endif

#if defined (O_WRONLY)
  DEFCONST (O_WRONLY, (double) O_WRONLY, 0, 0,
    "");
#endif
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
