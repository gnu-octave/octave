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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cerrno>

#include <string.h>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

// We can't use csignal as kill is not in the std namespace, and picky
// compiler runtimes will also exclude it from global scope as well.

#include <signal.h>

#include "lo-utils.h"
#include "oct-syscalls.h"
#include "str-vec.h"

#define NOT_SUPPORTED(nm) \
  nm ": not supported on this system"

int
octave_syscalls::dup2 (int old_fd, int new_fd)
{
  std::string msg;
  return dup2 (old_fd, new_fd, msg);
}

int
octave_syscalls::dup2 (int old_fd, int new_fd, std::string& msg)
{
  msg = std::string ();

  int status = -1;

#if defined (HAVE_DUP2)
  status = ::dup2 (old_fd, new_fd);

  if (status < 0)
    {
      using namespace std;
      msg = ::strerror (errno);
    }
#else
  msg = NOT_SUPPORTED ("dup2");
#endif

  return status;
}

int
octave_syscalls::execvp (const std::string& file, const string_vector& argv)
{
  std::string msg;
  return execvp (file, argv, msg);
}

int
octave_syscalls::execvp (const std::string& file, const string_vector& args,
			 std::string& msg)
{
  msg = std::string ();

  int status = -1;

#if defined (HAVE_EXECVP)
  char **argv = args.c_str_vec ();

  status = ::execvp (file.c_str (), argv);

  string_vector::delete_c_str_vec (argv);

  if (status < 0)
    {
      using namespace std;
      msg = ::strerror (errno);
    }
#else
  msg = NOT_SUPPORTED ("execvp");
#endif

  return status;
}

int
octave_syscalls::fcntl (int fd, int cmd, long arg)
{
  std::string msg;
  return fcntl (fd, cmd, arg, msg);
}

int
octave_syscalls::fcntl (int fd, int cmd, long arg, std::string& msg)
{
  msg = std::string ();

  int status = -1;

#if defined (HAVE_FCNTL)
  status = ::fcntl (fd, cmd, arg);

  if (status < 0)
    {
      using namespace std;
      msg = ::strerror (errno);
    }
#else
  msg = NOT_SUPPORTED ("fcntl");
#endif

  return status;
}

pid_t
octave_syscalls::fork (std::string& msg)
{
  pid_t status = -1;

#if defined (HAVE_FORK)
  status = ::fork ();

  if (status < 0)
    {
      using namespace std;
      msg = ::strerror (errno);
    }
#else
  msg = NOT_SUPPORTED ("fork");
#endif

  return status;
}

pid_t
octave_syscalls::vfork (std::string& msg)
{
  pid_t status = -1;

#if defined (HAVE_VFORK) || defined (HAVE_FORK)
#if defined (HAVE_VFORK)
  status = ::vfork ();
#else
  status = ::fork ();
#endif

  if (status < 0)
    {
      using namespace std;
      msg = ::strerror (errno);
    }
#else
  msg = NOT_SUPPORTED ("vfork");
#endif

  return status;
}

pid_t
octave_syscalls::getpgrp (std::string& msg)
{
  pid_t status = -1;

#if defined (HAVE_GETPGRP)
  status = ::getpgrp ();

  if (status < 0)
    {
      using namespace std;
      msg = ::strerror (errno);
    }
#else
  msg = NOT_SUPPORTED ("getpgrp");
#endif

  return status;
}

pid_t
octave_syscalls::getpid (void)
{
#if defined (HAVE_GETPID)
  return ::getpid ();
#else
  return 0;
#endif
}

pid_t
octave_syscalls::getppid (void)
{
#if defined (HAVE_GETPPID)
  return ::getppid ();
#else
  return 0;
#endif
}

gid_t
octave_syscalls::getgid (void)
{
#if defined (HAVE_GETGID)
  return ::getgid ();
#else
  return 0;
#endif
}

gid_t
octave_syscalls::getegid (void)
{
#if defined (HAVE_GETEGID)
  return ::getegid ();
#else
  return 0;
#endif
}

uid_t
octave_syscalls::getuid (void)
{
#if defined (HAVE_GETUID)
  return ::getuid ();
#else
  return 0;
#endif
}

uid_t
octave_syscalls::geteuid (void)
{
#if defined (HAVE_GETEUID)
  return ::geteuid ();
#else
  return 0;
#endif
}

int
octave_syscalls::pipe (int *fildes)
{
  std::string msg;
  return pipe (fildes);
}

int
octave_syscalls::pipe (int *fildes, std::string& msg)
{
  msg = std::string ();

  int status = -1;

#if defined (HAVE_PIPE)
  status = ::pipe (fildes);

  if (status < 0)
    {
      using namespace std;
      msg = ::strerror (errno);
    }
#else
  msg = NOT_SUPPORTED ("pipe");
#endif

  return status;
}

pid_t
octave_syscalls::waitpid (pid_t pid, int *status, int options)
{
  std::string msg;
  return waitpid (pid, status, options, msg);
}

pid_t
octave_syscalls::waitpid (pid_t pid, int *status, int options,
			  std::string& msg)
{
  pid_t retval = -1;
  msg = std::string ();

#if defined (HAVE_WAITPID)
  retval = ::octave_waitpid (pid, status, options);

  if (retval < 0)
    {
      using namespace std;
      msg = ::strerror (errno);
    }
#else
  msg = NOT_SUPPORTED ("waitpid");
#endif

  return retval;
}

int
octave_syscalls::kill (pid_t pid, int sig)
{
  std::string msg;
  return kill (pid, sig, msg);
}

int
octave_syscalls::kill (pid_t pid, int sig, std::string& msg)
{
  msg = std::string ();

  int status = -1;

#if defined (HAVE_KILL)
  status = ::kill (pid, sig);

  if (status < 0)
    {
      using namespace std;
      msg = ::strerror (errno);
    }
#else
  msg = NOT_SUPPORTED ("kill");
#endif

  return status;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
