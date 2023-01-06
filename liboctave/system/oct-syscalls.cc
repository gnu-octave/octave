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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cerrno>
#include <cstdlib>
#include <cstring>

#include "fcntl-wrappers.h"
#include "lo-utils.h"
#include "lo-sysdep.h"
#include "oct-syscalls.h"
#include "octave-popen2.h"
#include "signal-wrappers.h"
#include "str-vec.h"
#include "unistd-wrappers.h"
#include "wait-wrappers.h"

#define NOT_SUPPORTED(nm)                       \
  nm ": not supported on this system"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(sys)

int
dup2 (int old_fd, int new_fd)
{
  std::string msg;
  return sys::dup2 (old_fd, new_fd, msg);
}

int
dup2 (int old_fd, int new_fd, std::string& msg)
{
  msg = "";

  int status = -1;

  status = octave_dup2_wrapper (old_fd, new_fd);

  if (status < 0)
    msg = std::strerror (errno);

  return status;
}

int
execvp (const std::string& file, const string_vector& argv)
{
  std::string msg;
  return sys::execvp (file, argv, msg);
}

int
execvp (const std::string& file, const string_vector& args,
        std::string& msg)
{
  msg = "";

  char **argv = args.c_str_vec ();

  int status = octave_execvp_wrapper (file.c_str (), argv);

  string_vector::delete_c_str_vec (argv);

  if (status < 0)
    msg = std::strerror (errno);

  return status;
}

pid_t
fork (std::string& msg)
{
  pid_t status = -1;

  if (octave_have_fork ())
    {
      status = octave_fork_wrapper ();

      if (status < 0)
        msg = std::strerror (errno);
    }
  else
    msg = NOT_SUPPORTED ("fork");

  return status;
}

pid_t
vfork (std::string& msg)
{
  pid_t status = -1;

  if (octave_have_vfork () || octave_have_fork ())
    {
      if (octave_have_vfork ())
        status = octave_vfork_wrapper ();
      else
        status = octave_fork_wrapper ();

      if (status < 0)
        msg = std::strerror (errno);
    }
  else
    msg = NOT_SUPPORTED ("vfork");

  return status;
}

pid_t
getpgrp (std::string& msg)
{
  pid_t status = octave_getpgrp_wrapper ();

  if (status < 0)
    msg = std::strerror (errno);

  return status;
}

pid_t
getpid (void)
{
  return octave_getpid_wrapper ();
}

pid_t
getppid (void)
{
  return octave_getppid_wrapper ();
}

gid_t
getgid (void)
{
  return octave_getgid_wrapper ();
}

gid_t
getegid (void)
{
  return octave_getegid_wrapper ();
}

uid_t
getuid (void)
{
  return octave_getuid_wrapper ();
}

uid_t
geteuid (void)
{
  return octave_geteuid_wrapper ();
}

int
pipe (int *fildes)
{
  std::string msg;
  return sys::pipe (fildes, msg);
}

int
pipe (int *fildes, std::string& msg)
{
  msg = "";

  int status = -1;

  status = octave_pipe_wrapper (fildes);

  if (status < 0)
    msg = std::strerror (errno);

  return status;
}

pid_t
waitpid (pid_t pid, int *status, int options)
{
  std::string msg;
  return sys::waitpid (pid, status, options, msg);
}

pid_t
waitpid (pid_t pid, int *status, int options,
         std::string& msg)
{
  pid_t retval = -1;
  msg = "";

  retval = octave_waitpid_wrapper (pid, status, options);

  if (retval < 0)
    msg = std::strerror (errno);

  return retval;
}

int
wcontinue (void)
{
  return octave_wcontinue_wrapper ();
}

int
wcoredump (int status)
{
  return octave_wcoredump_wrapper (status);
}

bool
wifcontinued (int status)
{
  return octave_wifcontinued_wrapper (status);
}

bool
wifexited (int status)
{
  return octave_wifexited_wrapper (status);
}

bool
wifsignaled (int status)
{
  return octave_wifsignaled_wrapper (status);
}

bool
wifstopped (int status)
{
  return octave_wifstopped_wrapper (status);
}

int
wexitstatus (int status)
{
  return octave_wexitstatus_wrapper (status);
}

int
wnohang (void)
{
  return octave_wnohang_wrapper ();
}

int
wstopsig (int status)
{
  return octave_wstopsig_wrapper (status);
}

int
wtermsig (int status)
{
  return octave_wtermsig_wrapper (status);
}

int
wuntraced (void)
{
  return octave_wuntraced_wrapper ();
}

int
kill (pid_t pid, int sig)
{
  std::string msg;
  return sys::kill (pid, sig, msg);
}

int
kill (pid_t pid, int sig, std::string& msg)
{
  msg = "";

  int status = -1;

  if (octave_have_kill ())
    {
      status = octave_kill_wrapper (pid, sig);

      if (status < 0)
        msg = std::strerror (errno);
    }
  else
    msg = NOT_SUPPORTED ("kill");

  return status;
}

pid_t
popen2 (const std::string& cmd, const string_vector& args,
        bool sync_mode, int *fildes)
{
  std::string msg;
  return sys::popen2 (cmd, args, sync_mode, fildes, msg);
}

pid_t
popen2 (const std::string& cmd, const string_vector& args,
        bool sync_mode, int *fildes, std::string& msg)
{
  char **argv = args.c_str_vec ();
  const char *errmsg;

  pid_t pid = octave_popen2 (cmd.c_str (), argv, sync_mode, fildes,
                             &errmsg);

  string_vector::delete_c_str_vec (argv);

  if (pid < 0)
    msg = errmsg;

  return pid;
}

int
fcntl (int fd, int cmd, long arg)
{
  std::string msg;
  return sys::fcntl (fd, cmd, arg, msg);
}

int
fcntl (int fd, int cmd, long arg, std::string& msg)
{
  msg = "";

  int status = -1;

  status = octave_fcntl_wrapper (fd, cmd, arg);

  if (status < 0)
    msg = std::strerror (errno);

  return status;
}

OCTAVE_END_NAMESPACE(sys)
OCTAVE_END_NAMESPACE(octave)
