////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2023 The Octave Project Developers
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

// These functions may be provided by gnulib.  We don't include gnulib
// headers directly in Octave's C++ source files to avoid problems that
// may be caused by the way that gnulib overrides standard library
// functions.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <stdio.h>

#include <sys/types.h>
#include <unistd.h>

#if defined (__WIN32__) && ! defined (__CYGWIN__)
#  include <process.h>
#endif

#if defined (OCTAVE_USE_WINDOWS_API)
#  include <windows.h>
#  include <wchar.h>
#endif

#if defined (__WIN32__) && ! defined (__CYGWIN__)
#  include "windows-spawn.h"
#endif

#include "uniconv-wrappers.h"
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
#if defined (OCTAVE_USE_WINDOWS_API)
  wchar_t *wnm = u8_to_wchar (nm);
  int status = _wchdir (wnm);
  free ((void *) wnm);
  return status;
#else
  return chdir (nm);
#endif
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
#if defined (__WIN32__) && ! defined (__CYGWIN__)

  char *argv_mem_to_free;
  const char **sanitized_argv = prepare_spawn ((const char * const *) argv,
                                               &argv_mem_to_free);

#  if defined (OCTAVE_USE_WINDOWS_API) && defined (_UNICODE)

  // count number of arguments
  size_t argc;
  for (argc = 0; sanitized_argv[argc] != NULL; argc++)
    ;

  wchar_t *wfile = u8_to_wchar (file);
  const wchar_t **wargv = malloc ((argc + 1) * sizeof (wchar_t *));

  // convert multibyte UTF-8 strings to wide character strings
  for (size_t i_arg = 0; i_arg < argc; i_arg++)
    wargv[i_arg] = u8_to_wchar (sanitized_argv[i_arg]);

  wargv[argc] = NULL;

  free (sanitized_argv);
  free (argv_mem_to_free);

  int status = _wspawnv (P_WAIT, wfile, wargv+1);

#    if 0
  // Code snippet from gnulib execute.c

  // Executing arbitrary files as shell scripts is unsecure.
  if (status == -1 && errno == ENOEXEC)
    {
      // prog is not a native executable.  Try to execute it as a
      // shell script.  Note that prepare_spawn() has already prepended
      // a hidden element "sh.exe" to argv.
      argv[1] = prog_path;
      status = _wspawnv (P_WAIT, wargv[0], wargv);
    }
#    endif

  // This happens when the spawned child process terminates.

  free (wfile);
  const wchar_t **wp = wargv;
  // Casting away the const in the loop is ok here.
  while (*wp)
    free ((wchar_t *) *wp++);
  free (wargv);

#  else

  int status = spawnv (P_OVERLAY, file, sanitized_argv);

  // This only happens if spawnv fails.

  free (sanitized_argv);
  free (argv_mem_to_free);

#  endif

  return status;

#else

  return execv (file, argv);

#endif
}

int
octave_execvp_wrapper (const char *file, char *const *argv)
{
  return execvp (file, argv);
}

pid_t
octave_fork_wrapper (void)
{
#if defined (HAVE_FORK)
  return fork ();
#else
  return -1;
#endif
}

int
octave_ftruncate_wrapper (int fd, off_t sz)
{
  return ftruncate (fd, sz);
}

char *
octave_getcwd_wrapper (char *nm, size_t len)
{
#if defined (OCTAVE_USE_WINDOWS_API)
  wchar_t *tmp = _wgetcwd (NULL, 0);
  char *retval = NULL;

  if (! tmp)
    return retval;

  retval = u8_from_wchar (tmp);
  if (! nm)
    return retval;
  else
    {
      if (strlen (retval) > len)
        return NULL;

      memcpy (nm, retval, len);
      free (retval);
      return nm;
    }
#else
  return getcwd (nm, len);
#endif
}

gid_t
octave_getegid_wrapper (void)
{
#if defined (HAVE_GETEGID)
  return getegid ();
#else
  return -1;
#endif
}

uid_t
octave_geteuid_wrapper (void)
{
#if defined (HAVE_GETEUID)
  return geteuid ();
#else
  return -1;
#endif
}

gid_t
octave_getgid_wrapper (void)
{
#if defined (HAVE_GETGID)
  return getgid ();
#else
  return -1;
#endif
}

int
octave_gethostname_wrapper (char *nm, size_t len)
{
  return gethostname (nm, len);
}

pid_t
octave_getpgrp_wrapper (void)
{
#if defined (HAVE_GETPGRP)
  return getpgrp ();
#else
  return -1;
#endif
}

pid_t
octave_getpid_wrapper (void)
{
#if defined (HAVE_GETPID)
  return getpid ();
#else
  return -1;
#endif
}

pid_t
octave_getppid_wrapper (void)
{
#if defined (HAVE_GETPPID)
  return getppid ();
#else
  return -1;
#endif
}

uid_t
octave_getuid_wrapper (void)
{
#if defined (HAVE_GETUID)
  return getuid ();
#else
  return -1;
#endif
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
#if defined (OCTAVE_USE_WINDOWS_API)
  wchar_t *wnm = u8_to_wchar (nm);
  int status = _wrmdir (wnm);
  free ((void *) wnm);
  return status;
#else
  return rmdir (nm);
#endif
}

pid_t
octave_setsid_wrapper (void)
{
#if defined (HAVE_SETSID)
  return setsid ();
#else
  return -1;
#endif
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
#if defined (OCTAVE_USE_WINDOWS_API)
  wchar_t *wnm = u8_to_wchar (nm);

  // _wunlink fails on files with the read-only flag set. Try to un-set it.
  DWORD file_attributes = GetFileAttributesW (wnm);
  if (file_attributes != INVALID_FILE_ATTRIBUTES
      && file_attributes & FILE_ATTRIBUTE_READONLY)
    SetFileAttributesW (wnm, file_attributes & ~FILE_ATTRIBUTE_READONLY);

  int status = _wunlink (wnm);
  free ((void *) wnm);

  return status;
#else
  return unlink (nm);
#endif
}

pid_t
octave_vfork_wrapper (void)
{
#if defined (HAVE_VFORK)
  return vfork ();
#else
  return -1;
#endif
}

bool
octave_have_fork (void)
{
#if defined (HAVE_FORK)
  return true;
#else
  return false;
#endif
}

bool
octave_have_vfork (void)
{
#if defined (HAVE_VFORK)
  return true;
#else
  return false;
#endif
}
