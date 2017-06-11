/*

Copyright (C) 1993-2017 John W. Eaton

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstddef>
//#include <cstdlib>

#include <iostream>
#include <string>

#if defined (HAVE_TERMIOS_H)
#  include <termios.h>
#elif defined (HAVE_TERMIO_H)
#  include <termio.h>
#elif defined (HAVE_SGTTY_H)
#  include <sgtty.h>
#endif

#if defined (HAVE_CONIO_H)
#  include <conio.h>
#endif

#if defined (HAVE_SYS_IOCTL_H)
#  include <sys/ioctl.h>
#endif

#if defined (HAVE_FLOATINGPOINT_H)
#  include <floatingpoint.h>
#endif

#if defined (HAVE_IEEEFP_H)
#  include <ieeefp.h>
#endif

#if defined (HAVE_OMP_H)
#  include <omp.h>
#endif

#include "cmd-edit.h"
#include "file-ops.h"
#include "lo-mappers.h"
#include "lo-math.h"
#include "mach-info.h"
#include "oct-env.h"
#include "unistd-wrappers.h"
#include "unsetenv-wrapper.h"

#include "builtin-defun-decls.h"
#include "Cell.h"
#include "defun.h"
#include "display.h"
#include "error.h"
#include "errwarn.h"
#include "input.h"
#include "octave.h"
#include "ov.h"
#include "ovl.h"
#include "pager.h"
#include "parse.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "interpreter.h"
#include "utils.h"
#include "file-stat.h"

#if ! defined (STDIN_FILENO)
#  define STDIN_FILENO 1
#endif

#if defined (__386BSD__) || defined (__FreeBSD__) || defined (__NetBSD__)
static void
BSD_init (void)
{
#  if defined (HAVE_FLOATINGPOINT_H)
  // Disable trapping on common exceptions.
#    if ! defined (FP_X_DNML)
#      define FP_X_DNML 0
#    endif
  fpsetmask (~(FP_X_OFL|FP_X_INV|FP_X_DZ|FP_X_DNML|FP_X_UFL|FP_X_IMP));
#  endif
}
#endif

#if defined (__MINGW32__) || defined (_MSC_VER)

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <tlhelp32.h>
#include <shellapi.h>

static void
w32_set_octave_home (void)
{
  std::string bin_dir;

  HANDLE h = CreateToolhelp32Snapshot (TH32CS_SNAPMODULE
#if defined (TH32CS_SNAPMODULE32)
                                       | TH32CS_SNAPMODULE32
#endif
                                       , 0);

  if (h != INVALID_HANDLE_VALUE)
    {
      MODULEENTRY32 mod_info;

      ZeroMemory (&mod_info, sizeof (mod_info));
      mod_info.dwSize = sizeof (mod_info);

      if (Module32First (h, &mod_info))
        {
          do
            {
              std::string mod_name (mod_info.szModule);

              if (mod_name.find ("octinterp") != std::string::npos)
                {
                  bin_dir = mod_info.szExePath;
                  if (bin_dir[bin_dir.length () - 1] != '\\')
                    bin_dir.append (1, '\\');
                  break;
                }
            }
          while (Module32Next (h, &mod_info));
        }

      CloseHandle (h);
    }

  if (! bin_dir.empty ())
    {
      size_t pos = bin_dir.rfind ("\\bin\\");

      if (pos != std::string::npos)
        octave::sys::env::putenv ("OCTAVE_HOME", bin_dir.substr (0, pos));
    }
}

static void
w32_init (void)
{
  w32_set_octave_home ();

  octave::command_editor::prefer_env_winsize (true);
}

static bool
w32_shell_execute (const std::string& file)
{ }

#endif

// Set app id if we have the SetCurrentProcessExplicitAppUserModelID
// available (>= Win7).  FIXME: Could we check for existence of this
// function in the configure script instead of dynamically loading
// shell32.dll?

void
set_application_id (void)
{
#if defined (__MINGW32__) || defined (_MSC_VER)

  typedef HRESULT (WINAPI *SETCURRENTAPPID)(PCWSTR AppID);

  HMODULE hShell = LoadLibrary ("shell32.dll");

  if (hShell)
    {
      SETCURRENTAPPID pfnSetCurrentProcessExplicitAppUserModelID =
        reinterpret_cast<SETCURRENTAPPID> (GetProcAddress (hShell,
                                           "SetCurrentProcessExplicitAppUserModelID"));

      if (pfnSetCurrentProcessExplicitAppUserModelID)
        pfnSetCurrentProcessExplicitAppUserModelID (L"gnu.octave." VERSION);

      FreeLibrary (hShell);
    }

#endif
}

DEFUN (__open_with_system_app__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __open_with_system_app__ (@var{file})
Undocumented internal function.
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  std::string file = args(0).xstring_value ("__open_with_system_app__: argument must be a filename");

  octave_value retval;

#if defined (OCTAVE_USE_WINDOWS_API)
  HINSTANCE status = ShellExecute (0, 0, file.c_str (), 0, 0,
                                   SW_SHOWNORMAL);

  // ShellExecute returns a value greater than 32 if successful.
  retval = (reinterpret_cast<ptrdiff_t> (status) > 32);
#elif defined (__APPLE__)
  octave_value_list tmp
    = Fsystem (ovl ("open " + file + " 2> /dev/null",
                    false, "async"),
               1);

  retval = (tmp(0).double_value () == 0);
#else
  octave_value_list tmp
    = Fsystem (ovl ("xdg-open " + file + " 2> /dev/null",
                    false, "async"),
               1);

  retval = (tmp(0).double_value () == 0);
#endif

  return retval;
}

#if defined (__MINGW32__)
static void
MINGW_init (void)
{
  w32_init ();
}
#endif

#if defined (_MSC_VER)
static void
MSVC_init (void)
{
  w32_init ();
}
#endif

// Return TRUE if FILE1 and FILE2 refer to the same (physical) file.

bool
same_file_internal (const std::string& file1, const std::string& file2)
{
#if defined (OCTAVE_USE_WINDOWS_API)

  bool retval = false;

  const char *f1 = file1.c_str ();
  const char *f2 = file2.c_str ();

  bool f1_is_dir = GetFileAttributes (f1) & FILE_ATTRIBUTE_DIRECTORY;
  bool f2_is_dir = GetFileAttributes (f2) & FILE_ATTRIBUTE_DIRECTORY;

  // Windows native code
  // Reference: http://msdn2.microsoft.com/en-us/library/aa363788.aspx

  DWORD share = FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE;

  HANDLE hfile1
    = CreateFile (f1, 0, share, 0, OPEN_EXISTING,
                  f1_is_dir ? FILE_FLAG_BACKUP_SEMANTICS : 0, 0);

  if (hfile1 != INVALID_HANDLE_VALUE)
    {
      HANDLE hfile2
        = CreateFile (f2, 0, share, 0, OPEN_EXISTING,
                      f2_is_dir ? FILE_FLAG_BACKUP_SEMANTICS : 0, 0);

      if (hfile2 != INVALID_HANDLE_VALUE)
        {
          BY_HANDLE_FILE_INFORMATION hfi1;
          BY_HANDLE_FILE_INFORMATION hfi2;

          if (GetFileInformationByHandle (hfile1, &hfi1)
              && GetFileInformationByHandle (hfile2, &hfi2))
            {
              retval = (hfi1.dwVolumeSerialNumber == hfi2.dwVolumeSerialNumber
                        && hfi1.nFileIndexHigh == hfi2.nFileIndexHigh
                        && hfi1.nFileIndexLow == hfi2.nFileIndexLow);
            }

          CloseHandle (hfile2);
        }

      CloseHandle (hfile1);
    }

  return retval;

#else

  // POSIX Code

  octave::sys::file_stat fs_file1 (file1);
  octave::sys::file_stat fs_file2 (file2);

  return (fs_file1 && fs_file2
          && fs_file1.ino () == fs_file2.ino ()
          && fs_file1.dev () == fs_file2.dev ());

#endif
}

void
sysdep_init (void)
{
  // Use a function from libgomp to force loading of OpenMP library.
  // Otherwise, a dynamically loaded library making use of OpenMP such
  // as GraphicsMagick will segfault on exit (bug #41699).
#if defined (HAVE_OMP_GET_NUM_THREADS)
  omp_get_num_threads ();
#endif

#if defined (__386BSD__) || defined (__FreeBSD__) || defined (__NetBSD__)
  BSD_init ();
#elif defined (__MINGW32__)
  MINGW_init ();
#elif defined (_MSC_VER)
  MSVC_init ();
#endif
}

void
sysdep_cleanup (void)
{
#if defined (OCTAVE_USE_WINDOWS_API)
  // Let us fail immediately without displaying any dialog.
  SetProcessShutdownParameters (0x280, SHUTDOWN_NORETRY);
#endif
}

// Set terminal in raw mode.  From less-177.
//
// Change terminal to "raw mode", or restore to "normal" mode.
// "Raw mode" means
//      1. An outstanding read will complete on receipt of a single keystroke.
//      2. Input is not echoed.
//      3. On output, \n is mapped to \r\n.
//      4. \t is NOT expanded into spaces.
//      5. Signal-causing characters such as ctrl-C (interrupt),
//         etc. are NOT disabled.
// It doesn't matter whether an input \n is mapped to \r, or vice versa.

void
raw_mode (bool on, bool wait)
{
  static bool curr_on = false;

  int tty_fd = STDIN_FILENO;
  if (! octave_isatty_wrapper (tty_fd))
    {
      if (octave::application::interactive ()
          && ! octave::application::forced_interactive ())
        error ("stdin is not a tty!");
    }

  if (on == curr_on)
    return;

#if defined (HAVE_TERMIOS_H)
  {
    struct termios s;
    static struct termios save_term;

    if (on)
      {
        // Get terminal modes.

        tcgetattr (tty_fd, &s);

        // Save modes and set certain variables dependent on modes.

        save_term = s;
//      ospeed = s.c_cflag & CBAUD;
//      erase_char = s.c_cc[VERASE];
//      kill_char = s.c_cc[VKILL];

        // Set the modes to the way we want them.

        s.c_lflag &= ~(ICANON | ECHO | ECHOE | ECHOK | ECHONL);
        s.c_oflag |=  (OPOST | ONLCR);
#if defined (OCRNL)
        s.c_oflag &= ~(OCRNL);
#endif
#if defined (ONOCR)
        s.c_oflag &= ~(ONOCR);
#endif
#if defined (ONLRET)
        s.c_oflag &= ~(ONLRET);
#endif
        s.c_cc[VMIN] = (wait ? 1 : 0);
        s.c_cc[VTIME] = 0;
      }
    else
      {
        // Restore saved modes.

        s = save_term;
      }

    tcsetattr (tty_fd, wait ? TCSAFLUSH : TCSADRAIN, &s);
  }
#elif defined (HAVE_TERMIO_H)
  {
    struct termio s;
    static struct termio save_term;

    if (on)
      {
        // Get terminal modes.

        ioctl (tty_fd, TCGETA, &s);

        // Save modes and set certain variables dependent on modes.

        save_term = s;
//      ospeed = s.c_cflag & CBAUD;
//      erase_char = s.c_cc[VERASE];
//      kill_char = s.c_cc[VKILL];

        // Set the modes to the way we want them.

        s.c_lflag &= ~(ICANON | ECHO | ECHOE | ECHOK | ECHONL);
        s.c_oflag |=  (OPOST | ONLCR);
#if defined (OCRNL)
        s.c_oflag &= ~(OCRNL);
#endif
#if defined (ONOCR)
        s.c_oflag &= ~(ONOCR);
#endif
#if defined (ONLRET)
        s.c_oflag &= ~(ONLRET);
#endif
        s.c_cc[VMIN] = (wait ? 1 : 0);
      }
    else
      {
        // Restore saved modes.

        s = save_term;
      }

    ioctl (tty_fd, TCSETAW, &s);
  }
#elif defined (HAVE_SGTTY_H)
  {
    struct sgttyb s;
    static struct sgttyb save_term;

    if (on)
      {
        // Get terminal modes.

        ioctl (tty_fd, TIOCGETP, &s);

        // Save modes and set certain variables dependent on modes.

        save_term = s;
//      ospeed = s.sg_ospeed;
//      erase_char = s.sg_erase;
//      kill_char = s.sg_kill;

        // Set the modes to the way we want them.

        s.sg_flags |= CBREAK;
        s.sg_flags &= ~(ECHO);
      }
    else
      {
        // Restore saved modes.

        s = save_term;
      }

    ioctl (tty_fd, TIOCSETN, &s);
  }
#else
  warn_disabled_feature ("", "raw mode console I/O");

  // Make sure the current mode doesn't toggle.
  on = curr_on;
#endif

  curr_on = on;
}

FILE *
octave_popen (const char *command, const char *mode)
{
#if defined (__MINGW32__) || defined (_MSC_VER)
  if (mode && mode[0] && ! mode[1])
    {
      // Use binary mode on Windows if unspecified
      char tmode[3] = {mode[0], 'b', '\0'};

      return _popen (command, tmode);
    }
  else
    return _popen (command, mode);
#else
  return popen (command, mode);
#endif
}

int
octave_pclose (FILE *f)
{
#if defined (__MINGW32__) || defined (_MSC_VER)
  return _pclose (f);
#else
  return pclose (f);
#endif
}

// Read one character from the terminal.

int
octave_kbhit (bool wait)
{
#if defined (HAVE__KBHIT) && defined (HAVE__GETCH)
  // This essentially means we are on a Windows system.
  int c;

  if (wait)
    c = _getch ();
  else
    c = (! _kbhit ()) ? 0 : _getch ();

#else
  raw_mode (true, wait);

  // Get current handler.
  octave::interrupt_handler saved_interrupt_handler
    = octave::ignore_interrupts ();

  // Restore it, disabling system call restarts (if possible) so the
  // read can be interrupted.

  octave::set_interrupt_handler (saved_interrupt_handler, false);

  int c = std::cin.get ();

  if (std::cin.fail () || std::cin.eof ())
    std::cin.clear ();

  // Restore it, enabling system call restarts (if possible).
  octave::set_interrupt_handler (saved_interrupt_handler, true);

  raw_mode (false, true);
#endif

  return c;
}

std::string
get_P_tmpdir (void)
{
#if defined (OCTAVE_USE_WINDOWS_API)

  std::string retval;

#if defined (P_tmpdir)
  retval = P_tmpdir;
#endif

  // Apparently some versions of MinGW and MSVC either don't define
  // P_tmpdir, or they define it to a single backslash, neither of which
  // is particularly helpful.

  if (retval.empty () || retval == "\\")
    {
      retval = octave::sys::env::getenv ("TEMP");

      if (retval.empty ())
        retval = octave::sys::env::getenv ("TMP");

      if (retval.empty ())
        retval = "c:\\temp";
    }

  return retval;

#elif defined (P_tmpdir)

  return P_tmpdir;

#else

  return "/tmp";

#endif
}

DEFUN (clc, , ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} clc ()
@deftypefnx {} {} home ()
Clear the terminal screen and move the cursor to the upper left corner.
@end deftypefn */)
{
  bool skip_redisplay = true;

  octave::command_editor::clear_screen (skip_redisplay);

  return ovl ();
}

DEFALIAS (home, clc);

DEFUN (getenv, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} getenv (@var{var})
Return the value of the environment variable @var{var}.

For example,

@example
getenv ("PATH")
@end example

@noindent
returns a string containing the value of your path.
@seealso{setenv, unsetenv}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  std::string name = args(0).string_value ();

  return ovl (octave::sys::env::getenv (name));
}

/*
%!assert (ischar (getenv ("OCTAVE_HOME")))
*/

DEFUN (setenv, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} setenv (@var{var}, @var{value})
@deftypefnx {} {} setenv (@var{var})
@deftypefnx {} {} putenv (@dots{})
Set the value of the environment variable @var{var} to @var{value}.

If no @var{value} is specified then the variable will be assigned the null
string.
@seealso{unsetenv, getenv}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  std::string var = args(0).xstring_value ("setenv: VAR must be a string");

  std::string val = (nargin == 2
                     ? args(1).xstring_value ("setenv: VALUE must be a string")
                     : "");

  octave::sys::env::putenv (var, val);

  return ovl ();
}

DEFALIAS (putenv, setenv);

/*
%!test
%! setenv ("dummy_variable_that_cannot_matter", "foobar");
%! assert (getenv ("dummy_variable_that_cannot_matter"), "foobar");
%! unsetenv ("dummy_variable_that_cannot_matter");
%! assert (getenv ("dummy_variable_that_cannot_matter"), "");
*/

DEFUN (unsetenv, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{status} =} unsetenv (@var{var})
Delete the environment variable @var{var}.

Return 0 if the variable was deleted, or did not exist, and -1 if an error
occurred.
@seealso{setenv, getenv}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  std::string tmp = args(0).string_value ();

  return ovl (octave_unsetenv_wrapper (tmp.c_str ()));
}

/*
## Test for unsetenv is in setenv test
*/

// FIXME: perhaps kbhit should also be able to print a prompt?

DEFUN (kbhit, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} kbhit ()
@deftypefnx {} {} kbhit (1)
Read a single keystroke from the keyboard.

If called with an argument, don't wait for a keypress.

For example,

@example
x = kbhit ();
@end example

@noindent
will set @var{x} to the next character typed at the keyboard as soon as
it is typed.

@example
x = kbhit (1);
@end example

@noindent
is identical to the above example, but doesn't wait for a keypress,
returning the empty string if no key is available.
@seealso{input, pause}
@end deftypefn */)
{
  octave_value retval = "";

  // FIXME: add timeout and default value args?

  if (octave::application::interactive ())
    {
      Fdrawnow ();

      int c = octave_kbhit (args.length () == 0);

      if (c == -1)
        c = 0;

      char s[2] = { static_cast<char> (c), '\0' };

      retval = s;
    }

  return retval;
}

DEFUN (pause, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} pause ()
@deftypefnx {} {} pause (@var{n})
Suspend the execution of the program for @var{n} seconds.

If invoked without an input arguments then the program is suspended until a
character is typed.

@var{n} is a positive real value and may be a fraction of a second,
for example:

@example
@group
tic; pause (0.05); toc
     @print{} Elapsed time is 0.05039 seconds.
@end group
@end example

The following example prints a message and then waits 5 seconds before
clearing the screen.

@example
@group
disp ("wait please...");
pause (5);
clc;
@end group
@end example

@seealso{kbhit}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  if (nargin == 1)
    {
      double dval = args(0).double_value ();

      if (octave::math::isnan (dval))
        warning ("pause: NaN is an invalid delay");
      else
        {
          Fdrawnow ();

          if (octave::math::isinf (dval))
            {
              octave::flush_stdout ();
              octave_kbhit ();
            }
          else
            octave_sleep (dval);
        }
    }
  else
    {
      Fdrawnow ();
      octave::flush_stdout ();
      octave_kbhit ();
    }

  return ovl ();
}

/*
%!test
%! pause (1);

%!error (pause (1, 2))
*/

// FIXME: maybe this should only return 1 if IEEE floating
// point functions really work.

DEFUN (isieee, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} isieee ()
Return true if your computer @emph{claims} to conform to the IEEE standard
for floating point calculations.

No actual tests are performed.
@end deftypefn */)
{
  octave::mach_info::float_format flt_fmt =
    octave::mach_info::native_float_format ();

  return ovl (flt_fmt == octave::mach_info::flt_fmt_ieee_little_endian
              || flt_fmt == octave::mach_info::flt_fmt_ieee_big_endian);
}

/*
%!assert (islogical (isieee ()))
*/

DEFUN (native_float_format, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} native_float_format ()
Return the native floating point format as a string.
@end deftypefn */)
{
  octave::mach_info::float_format flt_fmt =
    octave::mach_info::native_float_format ();

  return ovl (octave::mach_info::float_format_as_string (flt_fmt));
}

/*
%!assert (ischar (native_float_format ()))
*/

DEFUN (tilde_expand, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} tilde_expand (@var{string})
@deftypefnx {} {} tilde_expand (@var{cellstr})
Perform tilde expansion on @var{string}.

If @var{string} begins with a tilde character, (@samp{~}), all of the
characters preceding the first slash (or all characters, if there is no
slash) are treated as a possible user name, and the tilde and the following
characters up to the slash are replaced by the home directory of the named
user.  If the tilde is followed immediately by a slash, the tilde is
replaced by the home directory of the user running Octave.

If the input is a cell array of strings @var{cellstr} then tilde expansion
is performed on each string element.

Examples:

@example
@group
tilde_expand ("~joeuser/bin")
     @result{} "/home/joeuser/bin"
tilde_expand ("~/bin")
     @result{} "/home/jwe/bin"
@end group
@end example
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_value arg = args(0);

  string_vector sv = arg.xstring_vector_value ("tilde_expand: argument must be char or cellstr object");

  sv = octave::sys::file_ops::tilde_expand (sv);

  if (arg.iscellstr ())
    return ovl (Cell (arg.dims (), sv));
  else
    return ovl (sv);
}

/*
%!test
%! home = get_home_directory ();
%! assert (tilde_expand ("~/foobar"), [home "/foobar"]);
%! assert (tilde_expand ("/foo/bar"), "/foo/bar");
%! assert (tilde_expand ("foo/bar"), "foo/bar");
*/

DEFUN (get_home_directory, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{homedir} =} get_home_directory ()
Return the current home directory.

On most systems, this is equivalent to @code{getenv ("HOME")}.  On Windows
systems, if the environment variable @env{HOME} is not set then it is
equivalent to
@code{fullfile (getenv ("HOMEDRIVE"), getenv ("HOMEPATH"))}
@seealso{getenv}
@end deftypefn */)
{
  return ovl (octave::sys::env::get_home_directory ());
}

/*
%!test
%! if (! ispc ())
%!   assert (get_home_directory (), getenv ("HOME"));
%! endif
*/

// This function really belongs in display.cc, but including defun.h in
// that file results in conflicts with symbols from headers that are
// needed for X11 and Carbon functions.

DEFUN (have_window_system, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} have_window_system ()
Return true if a window system is available (X11, Windows, or Apple OS X)
and false otherwise.
@seealso{isguirunning}
@end deftypefn */)
{
  return ovl (display_info::display_available ());
}
