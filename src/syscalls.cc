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

#include <cstdio>

#ifdef HAVE_UNISTD_H
#include <sys/types.h>
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

DEFUN(dup2, args, ,
 "fid = dup2 (old, new): duplicate a file descriptor")
{
  double retval = -1.0;

#if defined (HAVE_DUP2)
  int nargin = args.length ();

  if (nargin == 2)
    {
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
		retval = (double) dup2 (i_old, i_new);
	      else
		error ("dup2: invalid file id");
	    }
	  else
	    error ("dup2: arguments must be integer values");
	}
    }
  else
    print_usage ("dup2");
#else
  gripe_not_supported ("dup2");
#endif

  return retval;
}

DEFUN(exec, args, ,
 "exec (file, args): replace current process with a new process")
{
  double retval = -1.0;

#if defined (HAVE_EXECVP)
  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
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
	    execvp (exec_file.c_str (), exec_args);
	}
      else
	error ("exec: first argument must be a string");
    }
  else
    print_usage ("exec");
#else
  gripe_not_supported ("exec");
#endif

  return retval;
}

DEFUN(fcntl, args, ,
 "fcntl (fid, request, argument): control open file descriptors")
{
  double retval = -1.0;

#if defined (HAVE_FCNTL)
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
	    retval = fcntl (fid, req, arg);
	}
      else
	error ("fcntl: file id must be an integer");
    }
  else
    print_usage ("fcntl");
#else
  gripe_not_supported ("fcntl");
#endif

  return retval;
}

DEFUN(fork, args, ,
 "fork (): create a copy of the current process")
{
  double retval = -1.0;

#if defined (HAVE_FORK)
  int nargin = args.length ();

  if (nargin == 0)
    retval = fork ();
  else
    print_usage ("fork");
#else
  gripe_not_supported ("fork");
#endif

  return retval;
}

DEFUN(getpgrp, args, ,
  "pgid = getpgrp (): return the process group id of the current process")
{
  double retval = -1.0;

#if defined (HAVE_GETPGRP)
  int nargin = args.length ();

  if (nargin == 0)
    retval = getpgrp ();
  else
    print_usage ("getpgrp");
#else
  gripe_not_supported ("getpgrp");
#endif

  return retval;
}

DEFUN(getpid, args, ,
  "pid = getpid (): return the process id of the current process")
{
  double retval = -1.0;

#if defined (HAVE_GETPID)
  int nargin = args.length ();

  if (nargin == 0)
    retval = getpid ();
  else
    print_usage ("getpid");
#else
  gripe_not_supported ("getpid");
#endif

  return retval;
}

DEFUN(getppid, args, ,
  "pid = getppid (): return the process id of the parent process")
{
  double retval = -1.0;

#if defined (HAVE_GETPPID)
  int nargin = args.length ();

  if (nargin == 0)
    retval = getppid ();
  else
    print_usage ("getppid");
#else
  gripe_not_supported ("getppid");
#endif

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
  "STATUS = mkfifo (NAME, MODE)\n\
\n\
  Create a FIFO special file named NAME with file mode MODE\n\
\n\
  STATUS is:\n\
\n\
    != 0 : if mkfifo failed\n\
       0 : if the FIFO special file could be created")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 2)
    {
      if (args(0).is_string ())
	{
	  string name = args(0).string_value ();

	  if (args(1).is_scalar_type ())
	    {
	      long mode = (long) args(1).double_value ();

	      retval = oct_mkfifo (name, mode);
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
  "[file_ids, status] = pipe (): create an interprocess channel")
{
  octave_value_list retval (2, octave_value (-1.0));

#if defined (HAVE_PIPE)
  int nargin = args.length ();

  if (nargin == 0)
    {
      int fid[2];

      if (pipe (fid) >= 0)
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
	  retval(1) = 0.0;
	}	  
    }
  else
    print_usage ("pipe");
#else
  gripe_not_supported ("pipe");
#endif

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
  "STATUS = unlink (NAME)\n\
\n\
  Delete the file NAME\n\
\n\
  STATUS is:\n\
\n\
    != 0 : if unlink failed\n\
       0 : if the file could be successfully deleted")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 1)
    {
      if (args(0).is_string ())
	{
	  string name = args(0).string_value ();

	  retval = oct_unlink (name);
	}
      else
	error ("unlink: file name must be a string");
    }
  else
    print_usage ("unlink");

  return retval;
}

DEFUN (waitpid, args, ,
  "STATUS = waitpid (PID, OPTIONS)\n\
\n\
  wait for process PID to terminate\n\
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
  STATUS is:\n\
\n\
     -1 : if an error occured\n\
    > 0 : the process ID of the child process that exited")
{
  double retval = -1.0;

#if defined (HAVE_WAITPID)
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
		retval = waitpid (pid, 0, options);
	    }
	}
    }
  else
    print_usage ("waitpid");
#else
  gripe_not_supported ("waitpid");
#endif

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
