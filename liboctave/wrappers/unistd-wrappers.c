/*

Copyright (C) 2016-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

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

#if defined (__WIN32__) && ! defined (__CYGWIN__)
#  include <process.h>
#endif

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

#if defined (__WIN32__) && ! defined (__CYGWIN__)

// Adapted from libtool wrapper.

/* Prepares an argument vector before calling spawn().

   Note that spawn() does not by itself call the command interpreter
     (getenv ("COMSPEC") != NULL ? getenv ("COMSPEC") :
      ({ OSVERSIONINFO v; v.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
         GetVersionEx(&v);
         v.dwPlatformId == VER_PLATFORM_WIN32_NT;
      }) ? "cmd.exe" : "command.com").

   Instead it simply concatenates the arguments, separated by ' ', and calls
   CreateProcess().  We must quote the arguments since Win32 CreateProcess()
   interprets characters like ' ', '\t', '\\', '"' (but not '<' and '>') in a
   special way:

   - Space and tab are interpreted as delimiters. They are not treated as
     delimiters if they are surrounded by double quotes: "...".

   - Unescaped double quotes are removed from the input. Their only effect is
     that within double quotes, space and tab are treated like normal
     characters.

   - Backslashes not followed by double quotes are not special.

   - But 2*n+1 backslashes followed by a double quote become
     n backslashes followed by a double quote (n >= 0):
       \" -> "
       \\\" -> \"
       \\\\\" -> \\"
 */

#define SHELL_SPECIAL_CHARS \
  "\"\\ \001\002\003\004\005\006\007\010\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037"

#define SHELL_SPACE_CHARS \
  " \001\002\003\004\005\006\007\010\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037"

static char **
prepare_spawn (char **argv)
{
  size_t argc;
  char **new_argv;
  size_t i;

  /* Count number of arguments.  */
  for (argc = 0; argv[argc] != NULL; argc++)
    ;

  /* Allocate new argument vector.  */
  new_argv = (char **) malloc (argc + 1);

  /* Put quoted arguments into the new argument vector.  */
  for (i = 0; i < argc; i++)
    {
      const char *string = argv[i];

      if (string[0] == '\0')
        new_argv[i] = strdup ("\"\"");
      else if (strpbrk (string, SHELL_SPECIAL_CHARS) != NULL)
        {
          int quote_around = (strpbrk (string, SHELL_SPACE_CHARS) != NULL);
          size_t length;
          unsigned int backslashes;
          const char *s;
          char *quoted_string;
          char *p;

          length = 0;
          backslashes = 0;
          if (quote_around)
            length++;
          for (s = string; *s != '\0'; s++)
            {
              char c = *s;
              if (c == '"')
                length += backslashes + 1;
              length++;
              if (c == '\\')
                backslashes++;
              else
                backslashes = 0;
            }
          if (quote_around)
            length += backslashes + 1;

          quoted_string = (char *) malloc (length + 1);

          p = quoted_string;
          backslashes = 0;
          if (quote_around)
            *p++ = '"';
          for (s = string; *s != '\0'; s++)
            {
              char c = *s;
              if (c == '"')
                {
                  unsigned int j;
                  for (j = backslashes + 1; j > 0; j--)
                    *p++ = '\\';
                }
              *p++ = c;
              if (c == '\\')
                backslashes++;
              else
                backslashes = 0;
            }
          if (quote_around)
            {
              unsigned int j;
              for (j = backslashes; j > 0; j--)
                *p++ = '\\';
              *p++ = '"';
            }
          *p = '\0';

          new_argv[i] = quoted_string;
        }
      else
        new_argv[i] = strdup (string);
    }

  new_argv[argc] = NULL;

  return new_argv;
}

#endif

int
octave_execv_wrapper (const char *file, char *const *argv)
{
#if defined (__WIN32__) && ! defined (__CYGWIN__)

  char **sanitized_argv = prepare_spawn (argv);

  int status = spawnv (P_OVERLAY, file, sanitized_argv);

  // This only happens if spawnv fails.

  char **p = sanitized_argv;
  while (*p)
    free (*p++);
  free (sanitized_argv);

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
  return getcwd (nm, len);
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
  return rmdir (nm);
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
  return unlink (nm);
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
