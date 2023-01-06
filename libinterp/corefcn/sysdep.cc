////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#include "builtin-defun-decls.h"
#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "input.h"
#include "interpreter-private.h"
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

#if defined (__MINGW32__) || defined (_MSC_VER)

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <tlhelp32.h>
#include <psapi.h>
#include <shellapi.h>
#include <shobjidl.h>

#endif

OCTAVE_BEGIN_NAMESPACE(octave)

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
              std::string mod_name (sys::u8_from_wstring (mod_info.szModule));

              if (mod_name.find ("octinterp") != std::string::npos)
                {
                  bin_dir = sys::u8_from_wstring (mod_info.szExePath);
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
      std::size_t pos = bin_dir.rfind (R"(\bin\)");

      if (pos != std::string::npos)
        sys::env::putenv ("OCTAVE_HOME", bin_dir.substr (0, pos));
    }
}

static void
w32_init (void)
{
  w32_set_octave_home ();

  command_editor::prefer_env_winsize (true);
}

#endif

void set_application_id (void)
{
#if defined (__MINGW32__) || defined (_MSC_VER)

  SetCurrentProcessExplicitAppUserModelID (L"gnu.octave." VERSION);

#endif
}

DEFUN (__open_with_system_app__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{status} =} __open_with_system_app__ (@var{file})
Return 1 on successful system call and 0 otherwise.
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  std::string file = args(0).xstring_value ("__open_with_system_app__: argument must be a filename");

#if defined (OCTAVE_USE_WINDOWS_API)
  std::wstring wfile = sys::u8_to_wstring (file);
  HINSTANCE status = ShellExecuteW (0, 0, wfile.c_str (), 0, 0, SW_SHOWNORMAL);

  // ShellExecute returns a value greater than 32 if successful.
  return octave_value (reinterpret_cast<std::ptrdiff_t> (status) > 32);
#else
  // Quote file path
  file = '"' + file + '"';

#  if defined (__APPLE__)
#    define FSYSTEM_OPEN_STR "open "
#  else
#    define FSYSTEM_OPEN_STR "xdg-open "
#  endif
  octave_value_list tmp
    = Fsystem (ovl (FSYSTEM_OPEN_STR + file + " 2> /dev/null",
                    false, "async"),
               1);
#  undef FSYSTEM_OPEN_STR

  // Asynchronous Fsystem calls return the new child process identifier,
  // which must be greater than 1 if successful.
  return octave_value (tmp(0).double_value () > 1);
#endif
}

DEFUN (__is_elevated_process__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{retval} =} __is_elevated_process__ ()
Check if current process has elevated rights.

On Windows, return true if the current process has elevated right.  Otherwise,
return false.
On non-Windows platforms, this function fails with an error.
@end deftypefn */)
{
#if defined (OCTAVE_USE_WINDOWS_API)
  if (args.length () != 0)
    print_usage ();

  bool retval = false;
  HANDLE h_token = nullptr;

  if (OpenProcessToken (GetCurrentProcess (), TOKEN_QUERY, &h_token))
    {
      TOKEN_ELEVATION elevation;
      DWORD return_length = sizeof (TOKEN_ELEVATION);
      if (GetTokenInformation (h_token, TokenElevation, &elevation,
                               sizeof (elevation), &return_length))
        retval = elevation.TokenIsElevated;
    }

  if (h_token)
    CloseHandle (h_token);

  return ovl (retval);

#else
  octave_unused_parameter (args);
  error ("__is_elevated_process__: "
         "Function is only supported on Windows platforms.");
#endif
}

DEFUN (__wmemory__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{proc}, @var{sys}] =} __wmemory__ ()
Return memory information on Windows.

On non-Windows platforms, this function fails with an error.
@end deftypefn */)
{
#if defined (OCTAVE_USE_WINDOWS_API)
  if (args.length () != 0)
    print_usage ();

  // Get memory usage of the current process
  octave_scalar_map proc_struct;

  HANDLE h_proc = GetCurrentProcess ();
  if (h_proc == nullptr)
    error ("__wmemory__: Couldn't open handle to own process");

  PROCESS_MEMORY_COUNTERS proc_mem_count;
  if (GetProcessMemoryInfo (h_proc, &proc_mem_count, sizeof (proc_mem_count)))
    {
      proc_struct.setfield ("PageFaultCount",
                            proc_mem_count.PageFaultCount);
      proc_struct.setfield ("PeakWorkingSetSize",
                            proc_mem_count.PeakWorkingSetSize);
      proc_struct.setfield ("WorkingSetSize",
                            proc_mem_count.WorkingSetSize);
      proc_struct.setfield ("QuotaPeakPagedPoolUsage",
                            proc_mem_count.QuotaPeakPagedPoolUsage);
      proc_struct.setfield ("QuotaPagedPoolUsage",
                            proc_mem_count.QuotaPagedPoolUsage);
      proc_struct.setfield ("QuotaPeakNonPagedPoolUsage",
                            proc_mem_count.QuotaPeakNonPagedPoolUsage);
      proc_struct.setfield ("QuotaNonPagedPoolUsage",
                            proc_mem_count.QuotaNonPagedPoolUsage);
      proc_struct.setfield ("PagefileUsage",
                            proc_mem_count.PagefileUsage);
      proc_struct.setfield ("PeakPagefileUsage",
                            proc_mem_count.PeakPagefileUsage);
    }
  else
    {
      proc_struct.setfield ("PageFaultCount", 0);
      proc_struct.setfield ("PeakWorkingSetSize", 0);
      proc_struct.setfield ("WorkingSetSize", 0);
      proc_struct.setfield ("QuotaPeakPagedPoolUsage", 0);
      proc_struct.setfield ("QuotaPagedPoolUsage", 0);
      proc_struct.setfield ("QuotaPeakNonPagedPoolUsage", 0);
      proc_struct.setfield ("QuotaNonPagedPoolUsage", 0);
      proc_struct.setfield ("PagefileUsage", 0);
      proc_struct.setfield ("PeakPagefileUsage", 0);
    }

  CloseHandle (h_proc);

  // Get system memory usage
  octave_scalar_map sys_struct;

  MEMORYSTATUSEX mem_stat;

  mem_stat.dwLength = sizeof (mem_stat);

  if (GlobalMemoryStatusEx (&mem_stat))
    {
      sys_struct.setfield ("MemoryLoad", mem_stat.dwMemoryLoad);
      sys_struct.setfield ("TotalPhys", mem_stat.ullTotalPhys);
      sys_struct.setfield ("AvailPhys", mem_stat.ullAvailPhys);
      sys_struct.setfield ("TotalPageFile", mem_stat.ullTotalPageFile);
      sys_struct.setfield ("AvailPageFile", mem_stat.ullAvailPageFile);
      sys_struct.setfield ("TotalVirtual", mem_stat.ullTotalVirtual);
      sys_struct.setfield ("AvailVirtual", mem_stat.ullAvailVirtual);
      sys_struct.setfield ("AvailExtendedVirtual",
                           mem_stat.ullAvailExtendedVirtual);
    }
  else
    {
      sys_struct.setfield ("MemoryLoad", 0);
      sys_struct.setfield ("TotalPhys", 0);
      sys_struct.setfield ("AvailPhys", 0);
      sys_struct.setfield ("TotalPageFile", 0);
      sys_struct.setfield ("AvailPageFile", 0);
      sys_struct.setfield ("TotalVirtual", 0);
      sys_struct.setfield ("AvailVirtual", 0);
      sys_struct.setfield ("AvailExtendedVirtual", 0);
    }

  return ovl (proc_struct, sys_struct);

#else
  octave_unused_parameter (args);
  error ("__wmemory__: Function is only supported on Windows platforms");
#endif
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

bool same_file_internal (const std::string& file1, const std::string& file2)
{
#if defined (OCTAVE_USE_WINDOWS_API)

  // FIXME: When Octave switches to C++17, consider replacing this function
  //        by https://en.cppreference.com/w/cpp/filesystem/equivalent.

  bool retval = false;

  std::wstring file1w = sys::u8_to_wstring (file1);
  std::wstring file2w = sys::u8_to_wstring (file2);
  const wchar_t *f1 = file1w.c_str ();
  const wchar_t *f2 = file2w.c_str ();

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
                        && hfi1.nFileIndexLow == hfi2.nFileIndexLow
                        && hfi1.nFileSizeHigh == hfi2.nFileSizeHigh
                        && hfi1.nFileSizeLow == hfi2.nFileSizeLow
                        && hfi1.ftLastWriteTime.dwLowDateTime
                        == hfi2.ftLastWriteTime.dwLowDateTime
                        && hfi1.ftLastWriteTime.dwHighDateTime
                        == hfi2.ftLastWriteTime.dwHighDateTime);
            }

          CloseHandle (hfile2);
        }

      CloseHandle (hfile1);
    }

  return retval;

#else

  // POSIX Code

  sys::file_stat fs_file1 (file1);
  sys::file_stat fs_file2 (file2);

  return (fs_file1 && fs_file2
          && fs_file1.ino () == fs_file2.ino ()
          && fs_file1.dev () == fs_file2.dev ());

#endif
}

// Return TRUE if NAME refers to an existing drive letter or UNC share

bool drive_or_unc_share (const std::string& name)
{
#if defined (OCTAVE_USE_WINDOWS_API)
  std::size_t len = name.length ();
  bool candidate = false;
  if (len > 1 && isalpha(name[0]) && name[1]==':'
      && (len == 2 || (len == 3 && name[2] == '\\')))
    candidate = true;
  if (len > 4 && name[0] == '\\' && name[1] == '\\')
    {
      // It starts with two slashes.  Find the next slash.
      std::size_t next_slash = name.find ('\\', 3);
      if (next_slash != std::string::npos && len > next_slash+1)
        {
          // Check if it ends with the share
          std::size_t last_slash = name.find ('\\', next_slash+1);
          if (last_slash == std::string::npos
              || (len > next_slash+2 && last_slash == len-1))
            candidate = true;
        }
    }

  if (candidate)
    {
      // Open a handle to the file.
      std::wstring wname = sys::u8_to_wstring (name);
      HANDLE h
        = CreateFileW (wname.c_str (), FILE_READ_ATTRIBUTES,
                       FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                       nullptr, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS,
                       nullptr);
      if (h != INVALID_HANDLE_VALUE)
        {
          CloseHandle (h);
          return true;
        }
    }

  return false;

#else

  octave_unused_parameter (name);

  return false;

#endif
}

void sysdep_init (void)
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

void sysdep_cleanup (void)
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

void raw_mode (bool on, bool wait)
{
  static bool curr_on = false;

  int tty_fd = STDIN_FILENO;
  if (! octave_isatty_wrapper (tty_fd))
    {
      interpreter& interp = __get_interpreter__ ();

      if (interp.interactive () && ! application::forced_interactive ())
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

FILE * popen (const char *command, const char *mode)
{
#if defined (__MINGW32__) || defined (_MSC_VER)
  std::wstring wcommand = sys::u8_to_wstring (command);
  std::wstring wmode = sys::u8_to_wstring (mode);

  // Use binary mode on Windows if unspecified
  if (wmode.length () < 2)
    wmode += L'b';

  return _wpopen (wcommand.c_str (), wmode.c_str ());
#else
  return ::popen (command, mode);
#endif
}

int pclose (FILE *f)
{
#if defined (__MINGW32__) || defined (_MSC_VER)
  return ::_pclose (f);
#else
  return ::pclose (f);
#endif
}

// Read one character from the terminal.

int kbhit (bool wait)
{
#if defined (HAVE__KBHIT) && defined (HAVE__GETCH)
  // This essentially means we are on a Windows system.

  // The value to return when wait is false and no input is ready.
  static constexpr int eof = std::istream::traits_type::eof ();

  int c;

  if (wait)
    c = _getch ();
  else
    c = (! _kbhit ()) ? eof : _getch ();

#else
  raw_mode (true, wait);

  // Get current handler.
  interrupt_handler saved_interrupt_handler
    = ignore_interrupts ();

  // Restore it, disabling system call restarts (if possible) so the
  // read can be interrupted.

  set_interrupt_handler (saved_interrupt_handler, false);

  int c = std::cin.get ();

  if (std::cin.fail () || std::cin.eof ())
    {
      std::cin.clear ();
      clearerr (stdin);
    }

  // Restore it, enabling system call restarts (if possible).
  set_interrupt_handler (saved_interrupt_handler, true);

  raw_mode (false, true);
#endif

  return c;
}

std::string get_P_tmpdir (void)
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
      retval = sys::env::getenv ("TEMP");

      if (retval.empty ())
        retval = sys::env::getenv ("TMP");

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

  command_editor::clear_screen (skip_redisplay);

  return ovl ();
}

DEFALIAS (home, clc);

DEFUN (getenv, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{val} =} getenv ("@var{var}")
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

  return ovl (sys::env::getenv (name));
}

/*
%!assert (ischar (getenv ("OCTAVE_HOME")))
*/

DEFUN (setenv, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} setenv ("@var{var}", @var{value})
@deftypefnx {} {} setenv (@var{var})
@deftypefnx {} {} putenv (@dots{})
Set the value of the environment variable @var{var} to @var{value}.

If no @var{value} is specified then the variable will be assigned the null
string.

Programming Note: @code{putenv} is an alias for @code{setenv} and can be used
interchangeably.
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

  sys::env::putenv (var, val);

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

  return ovl (sys::unsetenv_wrapper (tmp));
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

// This function is also used in ov-java.cc, so don't declare static.
// Maybe the functions that use it should be here instead?

LONG
get_regkey_value (HKEY h_rootkey, const std::string subkey,
                  const std::string name, octave_value& value)
{
  LONG result;
  HKEY h_subkey;

  std::wstring wsubkey = sys::u8_to_wstring (subkey);
  result = RegOpenKeyExW (h_rootkey, wsubkey.c_str (), 0, KEY_READ,
                          &h_subkey);

  if (result != ERROR_SUCCESS)
    return result;

  unwind_action restore_keys ([=] () { reg_close_key_wrapper (h_subkey); });

  std::wstring wname = sys::u8_to_wstring (name);
  DWORD length = 0;
  result = RegQueryValueExW (h_subkey, wname.c_str (), nullptr, nullptr,
                             nullptr, &length);
  if (result != ERROR_SUCCESS)
    return result;

  DWORD type = 0;
  OCTAVE_LOCAL_BUFFER (BYTE, data, length);
  result = RegQueryValueExW (h_subkey, wname.c_str (), nullptr, &type,
                             data, &length);
  if (result != ERROR_SUCCESS)
    return result;

  if (type == REG_DWORD)
    value = octave_int32 (*(reinterpret_cast<DWORD *> (data)));
  else if (type == REG_SZ || type == REG_EXPAND_SZ)
    {
      // strings in registry might not be zero terminated
      wchar_t *dataw = reinterpret_cast<wchar_t *> (data);
      DWORD lengthw = length / sizeof (wchar_t);
      std::wstring reg_string
        = std::wstring (dataw, lengthw - (dataw[lengthw-1]==0));
      value = string_vector (sys::u8_from_wstring (reg_string));
    }

  return result;
}

static LONG
get_regkey_names (HKEY h_rootkey, const std::string subkey,
                  std::list<std::string>& fields)
{
  LONG retval;
  HKEY h_subkey;

  fields.clear ();

  std::wstring wsubkey = sys::u8_to_wstring (subkey);
  retval = RegOpenKeyExW (h_rootkey, wsubkey.c_str (), 0, KEY_READ,
                          &h_subkey);
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
      fields.push_back (sys::u8_from_wstring (value_name));
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

Get a list of value names at the key
@nospell{@qcode{'HKCU@backslashchar{}Environment'}}:

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
  std::string rootkey_name
    = args(0).xstring_value ("winqueryreg: the first argument must be 'name' "
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
      rootkey_name
        = args(1).xstring_value ("winqueryreg: ROOTKEY must be a string");
      subkey_name
        = args(2).xstring_value ("winqueryreg: SUBKEY must be a string");
    }
  else
    {
      subkey_name
        = args(1).xstring_value ("winqueryreg: SUBKEY must be a string");

      if (args.length () == 3)
        value_name
          = args(2).xstring_value ("winqueryreg: VALUENAME must be a string");
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
        error ("winqueryreg: error %ld reading names from registry", retval);

      Cell fieldnames (dim_vector (1, fields.size ()));
      std::size_t i;
      std::list<std::string>::const_iterator it;
      for (i = 0, it = fields.begin (); it != fields.end (); ++it, ++i)
        fieldnames(i) = *it;

      return ovl (fieldnames);
    }
  else
    {
      octave_value key_val;
      LONG retval = get_regkey_value (h_rootkey, subkey_name,
                                      value_name, key_val);
      if (retval == ERROR_FILE_NOT_FOUND)
        error ("winqueryreg: no value found for '%s' at %s\\%s",
               value_name.c_str (), rootkey_name.c_str (),
               subkey_name.c_str ());
      if (retval != ERROR_SUCCESS)
        error ("winqueryreg: error %ld reading the specified key", retval);

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

DEFMETHOD (kbhit, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{c} =} kbhit ()
@deftypefnx {} {@var{c} =} kbhit (1)
Read a single keystroke from the keyboard.

If called with an argument (typically 1), don't wait for a keypress and
immediately return the next keystroke in the keyboard input buffer or an empty
string ("") if no keystroke is available.

For example,

@example
c = kbhit ();
@end example

@noindent
will set @var{c} to the next character typed at the keyboard as soon as it is
typed.

@example
c = kbhit (1);
@end example

@noindent
is identical to the above example, but doesn't wait for a keypress, returning
the empty string if no key is available.
@seealso{input, pause}
@end deftypefn */)
{
  // FIXME: add timeout and default value args?

  Fdrawnow (interp);

  int c = kbhit (args.length () == 0);

  if (c == -1)
    c = 0;

  char s[2] = { static_cast<char> (c), '\0' };

  return octave_value (s);
}

// State of the pause system
static bool Vpause_enabled = true;

DEFMETHOD (pause, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} pause ()
@deftypefnx {} {} pause (@var{n})
@deftypefnx {} {@var{old_state} =} pause ("on")
@deftypefnx {} {@var{old_state} =} pause ("off")
@deftypefnx {} {@var{old_state} =} pause ("query")
Suspend the execution of the program or change the state of the pause function.

If invoked without an input arguments then the program is suspended until a
character is typed.  If argument @var{n} is a positive real value, it indicates
the number of seconds the program shall be suspended, for example:

@example
@group
tic; pause (0.05); toc
     @print{} Elapsed time is 0.05039 seconds.
@end group
@end example

The following example prints a message and then waits 5 seconds before clearing
the screen.

@example
@group
disp ("wait please...");
pause (5);
clc;
@end group
@end example

If invoked with a string argument @qcode{"on"}, @qcode{"off"}, or
@qcode{"query"}, the state of the pause function is changed or queried.  When
the state is @qcode{"off"}, the pause function returns immediately.  The
optional return value contains the previous state of the pause function.  In
the following example pause is disabled locally:

@example
@group
old_state = pause ("off");
tic; pause (0.05); toc
     @print{} Elapsed time is 3.00407e-05 seconds.
pause (old_state);
@end group
@end example

While the program is suspended Octave still handles figures painting and
graphics callbacks execution.

@seealso{kbhit}
@end deftypefn */)
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  if (nargin == 1 && args(0).is_string ())
    {
      bool saved_state = Vpause_enabled;
      std::string state = args(0).string_value ();

      if (state == "on")
        Vpause_enabled = true;
      else if (state == "off")
        Vpause_enabled = false;
      else if (state == "query")
        ;// Do nothing
      else
        error (R"(pause: first argument must be "on", "off", or "query")");

      if (nargout > 0 || state == "query")
        retval.append (saved_state ? "on" : "off");
    }
  else if (Vpause_enabled)
    {
      double dval;

      if (nargin == 0)
        dval = octave_Inf;
      else
        dval = args(0).xdouble_value ("pause: N must be a scalar real value");

      if (math::isnan (dval))
        warning ("pause: NaN is an invalid delay");
      else
        {
          Fdrawnow (interp);

          sleep (dval, true);
        }
    }

  return retval;
}

/*
%!test
%! pause (1);

%!error pause (1, 2)
*/

// FIXME: maybe this should only return 1 if IEEE floating
// point functions really work.

DEFUN (isieee, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isieee ()
Return true if your computer @emph{claims} to conform to the IEEE standard
for floating point calculations.

No actual tests are performed.
@end deftypefn */)
{
  mach_info::float_format flt_fmt = mach_info::native_float_format ();

  return ovl (flt_fmt == mach_info::flt_fmt_ieee_little_endian
              || flt_fmt == mach_info::flt_fmt_ieee_big_endian);
}

/*
%!assert (islogical (isieee ()))
*/

DEFUN (native_float_format, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{fmtstr} =} native_float_format ()
Return the native floating point format as a string.
@end deftypefn */)
{
  mach_info::float_format flt_fmt = mach_info::native_float_format ();

  return ovl (mach_info::float_format_as_string (flt_fmt));
}

/*
%!assert (ischar (native_float_format ()))
*/

DEFUN (tilde_expand, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{newstr} =} tilde_expand (@var{string})
@deftypefnx {} {@var{newcstr} =} tilde_expand (@var{cellstr})
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

  string_vector sv =
    arg.xstring_vector_value ("tilde_expand: argument must be char or cellstr object");

  sv = sys::file_ops::tilde_expand (sv);

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
  return ovl (sys::env::get_home_directory ());
}

/*
%!test
%! if (! ispc ())
%!   assert (get_home_directory (), getenv ("HOME"));
%! endif
*/

DEFUN (__blas_version__, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{verstr} =} __blas_version__ ()
Undocumented internal function.
@end deftypefn */)
{
  return ovl (sys::blas_version ());
}

DEFUN (__lapack_version__, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{verstr} =} __lapack_version__ ()
Undocumented internal function.
@end deftypefn */)
{
  return ovl (sys::lapack_version ());
}

OCTAVE_END_NAMESPACE(octave)
