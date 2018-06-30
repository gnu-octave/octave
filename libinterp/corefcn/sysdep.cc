/*

Copyright (C) 1993-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cmath>
#include <cstddef>

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
#include "lo-sysinfo.h"
#include "mach-info.h"
#include "oct-env.h"
#include "uniconv-wrappers.h"
#include "unistd-wrappers.h"
#include "unsetenv-wrapper.h"

#include "builtin-defun-decls.h"
#include "Cell.h"
#include "defun.h"
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
      MODULEENTRY32W mod_info;

      ZeroMemory (&mod_info, sizeof (mod_info));
      mod_info.dwSize = sizeof (mod_info);

      if (Module32FirstW (h, &mod_info))
        {
          do
            {
              std::string mod_name (octave::sys::u8_from_wstring (mod_info.szModule));

              if (mod_name.find ("octinterp") != std::string::npos)
                {
                  bin_dir = octave::sys::u8_from_wstring (mod_info.szExePath);
                  if (! bin_dir.empty () && bin_dir.back () != '\\')
                    bin_dir.push_back ('\\');
                  break;
                }
            }
          while (Module32NextW (h, &mod_info));
        }

      CloseHandle (h);
    }

  if (! bin_dir.empty ())
    {
      size_t pos = bin_dir.rfind (R"(\bin\)");

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
  HINSTANCE status = ShellExecuteW (0, 0,
                                    octave::sys::u8_to_wstring (file).c_str (),
                                    0, 0, SW_SHOWNORMAL);

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

  const wchar_t *f1 = octave::sys::u8_to_wstring (file1).c_str ();
  const wchar_t *f2 = octave::sys::u8_to_wstring (file2).c_str ();

  bool f1_is_dir = GetFileAttributesW (f1) & FILE_ATTRIBUTE_DIRECTORY;
  bool f2_is_dir = GetFileAttributesW (f2) & FILE_ATTRIBUTE_DIRECTORY;

  // Windows native code
  // Reference: http://msdn2.microsoft.com/en-us/library/aa363788.aspx

  DWORD share = FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE;

  HANDLE hfile1
    = CreateFileW (f1, 0, share, 0, OPEN_EXISTING,
                   f1_is_dir ? FILE_FLAG_BACKUP_SEMANTICS : 0, 0);

  if (hfile1 != INVALID_HANDLE_VALUE)
    {
      HANDLE hfile2
        = CreateFileW (f2, 0, share, 0, OPEN_EXISTING,
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
    octave_unused_parameter (wait);

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

  octave_unused_parameter (wait);

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
  wchar_t *wcommand = u8_to_wchar (command);
  wchar_t *wmode = u8_to_wchar (mode);

  octave::unwind_protect frame;
  frame.add_fcn (::free, static_cast<void *> (wcommand));
  frame.add_fcn (::free, static_cast<void *> (wmode));

  if (wmode && wmode[0] && ! wmode[1])
    {
      // Use binary mode on Windows if unspecified
      wchar_t tmode[3] = {wmode[0], L'b', L'\0'};

      return _wpopen (wcommand, tmode);
    }
  else
    return _wpopen (wcommand, wmode);
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

  if (retval.empty () || retval == R"(\)")
    {
      retval = octave::sys::env::getenv ("TEMP");

      if (retval.empty ())
        retval = octave::sys::env::getenv ("TMP");

      if (retval.empty ())
        retval = R"(c:\temp)";
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

#if defined (OCTAVE_USE_WINDOWS_API)

static void
reg_close_key_wrapper (HKEY key)
{
  RegCloseKey (key);
}

LONG
get_regkey_value (HKEY h_rootkey, const std::string subkey,
                  const std::string name, octave_value& value)
{
  LONG result;
  HKEY h_subkey;

  result = RegOpenKeyExW (h_rootkey,
                          octave::sys::u8_to_wstring (subkey).c_str (), 0,
                          KEY_READ, &h_subkey);
  if (result != ERROR_SUCCESS)
    return result;

  octave::unwind_protect frame;

  frame.add_fcn (reg_close_key_wrapper, h_subkey);

  DWORD length = 0;
  result = RegQueryValueExW (h_subkey,
                             octave::sys::u8_to_wstring (name).c_str (),
                             nullptr, nullptr, nullptr, &length);
  if (result != ERROR_SUCCESS)
    return result;

  DWORD type = 0;
  OCTAVE_LOCAL_BUFFER (BYTE, data, length);
  result = RegQueryValueExW (h_subkey,
                             octave::sys::u8_to_wstring (name).c_str (),
                             nullptr, &type, data, &length);
  if (result != ERROR_SUCCESS)
    return result;

  if (type == REG_DWORD)
    value = octave_int32 (*data);
  else if (type == REG_SZ || type == REG_EXPAND_SZ)
    value = string_vector (octave::sys::u8_from_wstring (
                                        reinterpret_cast<wchar_t *> (data)));

  return result;
}

LONG
get_regkey_names (HKEY h_rootkey, const std::string subkey,
                  std::list<std::string> &fields)
{
  LONG retval;
  HKEY h_subkey;

  fields.clear ();

  retval = RegOpenKeyExW (h_rootkey,
                          octave::sys::u8_to_wstring (subkey).c_str (), 0,
                          KEY_READ, &h_subkey);
  if (retval != ERROR_SUCCESS)
    return retval;

  DWORD idx = 0;
  const int MAX_VALUE_NAME_SIZE = 32766;
  wchar_t value_name[MAX_VALUE_NAME_SIZE+1];
  DWORD value_name_size = MAX_VALUE_NAME_SIZE;

  while (true)
    {
      retval = RegEnumValueW (h_subkey, idx, value_name, &value_name_size,
                              nullptr, nullptr, nullptr, nullptr);
      if (retval != ERROR_SUCCESS)
        break;
      fields.push_back (octave::sys::u8_from_wstring (value_name));
      value_name_size = MAX_VALUE_NAME_SIZE;
      idx++;
    }

  if (retval == ERROR_NO_MORE_ITEMS)
    retval = ERROR_SUCCESS;

  RegCloseKey (h_subkey);

  return retval;
}
#endif

DEFUN (winqueryreg, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{value} =} winqueryreg (@var{rootkey}, @var{subkey}, @var{valuename})
@deftypefnx {} {@var{value} =} winqueryreg (@var{rootkey}, @var{subkey})
@deftypefnx {} {@var{names} =} winqueryreg (@code{"name"}, @var{rootkey}, @var{subkey})

Query names or value from the Windows registry.

On Windows, return the value of the registry key @var{subkey} from the root key
@var{rootkey}.  You can specify the name of the queried registry value with the
optional argument @var{valuename}.  Otherwise, if called with only two
arguments or @var{valuename} is empty, then the default value of @var{subkey}
is returned.  If the registry value is of type @nospell{@qcode{"REG_DWORD"}}
then @var{value} is of class int32.  If the value is of the type
@nospell{@qcode{"REG_SZ"}} or @nospell{@qcode{"REG_EXPAND_SZ"}} a string is
returned.

If the first argument is @qcode{"name"}, a cell array of strings with the names
of the values at that key is returned.

The variable @var{rootkey} must be a string with a valid root key identifier:

@table @asis
@item  @nospell{HKCR}
@itemx @nospell{HKEY_CLASSES_ROOT}

@item @nospell{HKEY_CURRENT_CONFIG}

@item  @nospell{HKCU}
@itemx @nospell{HKEY_CURRENT_USER}

@item  @nospell{HKLM}
@itemx @nospell{HKEY_LOCAL_MACHINE}


@item  @nospell{HKU}
@itemx @nospell{HKEY_USERS}


@item @nospell{HKEY_PERFORMANCE_DATA}

@end table

Examples:

Get a list of value names at the key @nospell{@qcode{'HKCU\Environment'}}:

@example
@group
@var{valuenames} = winqueryreg ("name", "HKEY_CURRENT_USER", ...
                          "Environment");
@end group
@end example

For each @var{valuenames}, display the value:

@example
@group
for @var{k} = 1:numel (@var{valuenames})
  @var{val} = winqueryreg ("HKEY_CURRENT_USER", "Environment", ...
                     @var{valuenames}@{@var{k}@});
  @var{str} = sprintf ("%s = %s", @var{valuenames}@{@var{k}@}, num2str (@var{val}));
  disp (@var{str});
endfor
@end group
@end example

On non-Windows platforms this function fails with an error.
@end deftypefn */)
{
#if defined (OCTAVE_USE_WINDOWS_API)
  if ((args.length () < 2) || (args.length () > 3))
    print_usage ();

  // Input check
  std::string rootkey_name =
    args(0).xstring_value ("winqueryreg: the first argument must be 'name' "
                           "or a valid ROOTKEY identifier");
  std::string subkey_name = "";
  std::string value_name = "";
  bool get_names = false;
  if (rootkey_name.compare ("name") == 0)
    {
      if (args.length () < 3)
        error ("winqueryreg: if the first argument is 'name', "
               "ROOTKEY and SUBKEY must be given");
      get_names = true;
      rootkey_name =
        args(1).xstring_value ("winqueryreg: ROOTKEY must be a string");
      subkey_name =
        args(2).xstring_value ("winqueryreg: SUBKEY must be a string");
    }
  else
    {
      subkey_name =
        args(1).xstring_value ("winqueryreg: SUBKEY must be a string");
      if (args.length () == 3)
        value_name =
          args(2).xstring_value ("winqueryreg: VALUENAME must be a string");
    }

  // Get rootkey handle
  HKEY h_rootkey;
  if (rootkey_name == "HKEY_CLASSES_ROOT" || rootkey_name == "HKCR")
    h_rootkey = HKEY_CLASSES_ROOT;
  else if (rootkey_name == "HKEY_CURRENT_CONFIG")
    h_rootkey = HKEY_CURRENT_CONFIG;
  else if (rootkey_name == "HKEY_CURRENT_USER" || rootkey_name == "HKCU")
    h_rootkey = HKEY_CURRENT_USER;
  else if (rootkey_name == "HKEY_LOCAL_MACHINE" || rootkey_name == "HKLM")
    h_rootkey = HKEY_LOCAL_MACHINE;
  else if (rootkey_name == "HKEY_PERFORMANCE_DATA")
    h_rootkey = HKEY_PERFORMANCE_DATA;
  else if (rootkey_name == "HKEY_USERS" || rootkey_name == "HKU")
    h_rootkey = HKEY_USERS;
  else
    error ("winqueryreg: ROOTKEY is not a valid root key identifier");

  if (get_names)
    {
      std::list<std::string> fields;

      LONG retval = get_regkey_names (h_rootkey, subkey_name, fields);
      if (retval != ERROR_SUCCESS)
        error ("winqueryreg: error %d reading names from registry", retval);

      Cell fieldnames (dim_vector (1, fields.size ()));
      size_t i;
      std::list<std::string>::const_iterator it;
      for (i = 0, it = fields.begin (); it != fields.end (); ++it, ++i)
        fieldnames(i) = *it;

      return ovl (fieldnames);
    }
  else
    {
      octave_value key_val;
      LONG retval = get_regkey_value (h_rootkey, subkey_name, value_name,
                                      key_val);
      if (retval == ERROR_FILE_NOT_FOUND)
        error ("winqueryreg: no value found for '%s' at %s\\%s.",
               value_name.c_str (), rootkey_name.c_str (),
               subkey_name.c_str ());
      if (retval != ERROR_SUCCESS)
        error ("winqueryreg: error %d reading the specified key", retval);

      return ovl (key_val);
    }
#else

  octave_unused_parameter (args);

  error ("winqueryreg: function is only supported on Windows platforms");

#endif
}

/*
%!testif ; ispc ()
%! assert (ischar (winqueryreg ("HKEY_LOCAL_MACHINE",
%!                              'SOFTWARE\Microsoft\Windows\CurrentVersion',
%!                              "ProgramFilesDir")));
%!testif ; ispc ()
%! assert (isa (winqueryreg ("HKEY_LOCAL_MACHINE",
%!                           'SYSTEM\CurrentControlSet\Control\FileSystem',
%!                           "Win31FileSystem"), "int32"));
%!testif ; ispc ()
%! assert (iscellstr (winqueryreg ("name", "HKEY_LOCAL_MACHINE",
%!                                 'SOFTWARE\Microsoft\Windows\CurrentVersion')));
%!testif ; ispc ()
%! fail ('winqueryreg (1, ''SOFTWARE\Microsoft\Windows\CurrentVersion'')',
%!       "first argument must be 'name' or a valid ROOTKEY identifier");
%!testif ; ispc ()
%! fail ('winqueryreg ("HKEY_LOCAL_MACHINE", 1)', "SUBKEY must be a string");
%!testif ; ispc ()
%! fail (['winqueryreg ("HKEY_LOCAL_MACHINE", ', ...
%!        '''SOFTWARE\Microsoft\Windows\CurrentVersion'', 1)'],
%!       "VALUENAME must be a string");
%!testif ; ispc ()
%! fail (['winqueryreg ("FOO", ', ...
%!        '''SOFTWARE\Microsoft\Windows\CurrentVersion'')'],
%!       "ROOTKEY is not a valid root key identifier");
%!testif ; ispc ()
%! fail ('winqueryreg ("HKEY_LOCAL_MACHINE", ''FOO\bar'')',
%!       "no value found for");
%!testif ; ispc ()
%! fail (['winqueryreg ("HKEY_LOCAL_MACHINE", ', ...
%!        '''SOFTWARE\Microsoft\Windows\CurrentVersion'', "foo")'],
%!       "no value found for");
%!testif ; ispc ()
%! fail ('winqueryreg ("name", "HKEY_LOCAL_MACHINE")',
%!       "if the first argument is 'name', ROOTKEY and SUBKEY must be given");
%!testif ; ispc ()
%! fail (['winqueryreg ("name", 1, ', ...
%!        '''SOFTWARE\Microsoft\Windows\CurrentVersion'')'],
%!       "ROOTKEY must be a string");
%!testif ; ispc ()
%! fail ('winqueryreg ("name", "HKEY_LOCAL_MACHINE", 1)',
%!       "SUBKEY must be a string");
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
  // FIXME: add timeout and default value args?

  Fdrawnow ();

  int c = octave_kbhit (args.length () == 0);

  if (c == -1)
    c = 0;

  char s[2] = { static_cast<char> (c), '\0' };

  return octave_value (s);
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

DEFUN (__blas_version__, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __blas_version__ ()
Undocumented internal function.
@end deftypefn */)
{
  return ovl (octave::sys::blas_version ());
}

DEFUN (__lapack_version__, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __lapack_version__ ()
Undocumented internal function.
@end deftypefn */)
{
  return ovl (octave::sys::lapack_version ());
}
