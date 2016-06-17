/*

Copyright (C) 2016 John W. Eaton

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

// These functions may be provided by gnulib.  We don't include gnulib
// headers directly in Octave's C++ source files to avoid problems that
// may be caused by the way that gnulib overrides standard library
// functions.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <stdio.h>

#include <stdio.h>

#include <sys/types.h>
#include <unistd.h>

#include "unistd-wrappers.h"

int
octave_access_f_ok (void)
{
  return F_OK;
}

int
octave_access_r_ok (void)
{
  return R_OK;
}

int
octave_access_w_ok (void)
{
  return W_OK;
}

int
octave_access_x_ok (void)
{
  return X_OK;
}

int
octave_access_wrapper (const char *nm, int mode)
{
  return access (nm, mode);
}

int
octave_chdir_wrapper (const char *nm)
{
  return chdir (nm);
}

int
octave_close_wrapper (int fd)
{
  return close (fd);
}

const char *
octave_ctermid_wrapper (void)
{
#if defined (HAVE_CTERMID)
  return ctermid (0);
#else
  return "/dev/tty";
#endif
}

int
octave_dup2_wrapper (int fd1, int fd2)
{
  return dup2 (fd1, fd2);
}

int
octave_execv_wrapper (const char *file, char *const *argv)
{
  return execv (file, argv);
}

int
octave_execvp_wrapper (const char *file, char *const *argv)
{
  return execvp (file, argv);
}

pid_t
octave_fork_wrapper (void)
{
  return fork ();
}

int
octave_ftruncate_wrapper (int fd, off_t sz)
{
  return ftruncate (fd, sz);
}

char *
octave_getcwd_wrapper (char *nm, size_t len)
{
  return getcwd (nm, len);
}

gid_t
octave_getegid_wrapper (void)
{
  return getegid ();
}

uid_t
octave_geteuid_wrapper (void)
{
  return geteuid ();
}

gid_t
octave_getgid_wrapper (void)
{
  return getgid ();
}

int
octave_gethostname_wrapper (char *nm, size_t len)
{
  return gethostname (nm, len);
}

pid_t
octave_getpgrp_wrapper (void)
{
  return getpgrp ();
}

pid_t
octave_getpid_wrapper (void)
{
  return getpid ();
}

pid_t
octave_getppid_wrapper (void)
{
  return getppid ();
}

uid_t
octave_getuid_wrapper (void)
{
  return getuid ();
}

int
octave_isatty_wrapper (int fd)
{
  return isatty (fd);
}

int
octave_link_wrapper (const char *nm1, const char *nm2)
{
  return link (nm1, nm2);
}

int
octave_pipe_wrapper (int *fd)
{
  return pipe (fd);
}

int
octave_rmdir_wrapper (const char *nm)
{
  return rmdir (nm);
}

pid_t
octave_setsid_wrapper (void)
{
  return setsid ();
}

int
octave_stdin_fileno (void)
{
  return STDIN_FILENO;
}

int
octave_stdout_fileno (void)
{
  return STDOUT_FILENO;
}

int
octave_symlink_wrapper (const char *nm1, const char *nm2)
{
  return symlink (nm1, nm2);
}

int
octave_unlink_wrapper (const char *nm)
{
  return unlink (nm);
}

pid_t
octave_vfork_wrapper (void)
{
  return vfork ();
}
