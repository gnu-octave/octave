////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2010-2023 The Octave Project Developers
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

#if defined (HAVE_FLTK)

#  if defined (WIN32)
#    define WIN32_LEAN_AND_MEAN
#  endif

#  include <string>

#  include <FL/Fl.H>
#  include <FL/Fl_File_Chooser.H>

// FLTK headers may include X11/X.h which defines Complex, and that
// conflicts with Octave's Complex typedef.  We don't need the X11
// Complex definition in this file, so remove it before including Octave
// headers which may require Octave's Complex typedef.
#  undef Complex

#endif

#include "dMatrix.h"
#include "file-ops.h"

#include "Cell.h"
#include "defun-dld.h"
#include "errwarn.h"
#include "ov.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN_DLD (__fltk_uigetfile__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {[@var{fname}, @var{fpath}, @var{fltidx}] =} __fltk_uigetfile__ (@dots{})
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_FLTK)

  // Expected argument list:
  //
  //   args(0) ... FileFilter in fltk format
  //   args(1) ... Title
  //   args(2) ... Default Filename
  //   args(3) ... SelectValue "on"/"off"/"dir"/"create"

  octave_value_list retval (3, octave_value (0));

  std::string file_filter = args(0).string_value ();
  std::string title = args(1).string_value ();
  std::string default_name = args(2).string_value ();

  int multi_type = Fl_File_Chooser::SINGLE;
  std::string flabel = "Filename:";

  std::string multi = args(3).string_value ();
  if (multi == "on")
    multi_type = Fl_File_Chooser::MULTI;
  else if (multi == "dir")
    {
      multi_type = Fl_File_Chooser::DIRECTORY;
      flabel = "Directory:";
    }
  else if (multi == "create")
    multi_type = Fl_File_Chooser::CREATE;

  Fl_File_Chooser::filename_label = flabel.c_str ();

  Fl_File_Chooser fc (default_name.c_str (), file_filter.c_str (),
                      multi_type, title.c_str ());

  fc.preview (0);

  if (multi_type == Fl_File_Chooser::CREATE)
    fc.ok_label ("Save");

  fc.show ();

  while (fc.shown ())
    Fl::wait ();

  if (fc.value ())
    {
      int file_count = fc.count ();
      std::string fname;

      // FLTK uses forward slash even for Windows
      std::string sep = "/";
      std::size_t idx;

      if (file_count == 1 && multi_type != Fl_File_Chooser::DIRECTORY)
        {
          fname = fc.value ();
          idx = fname.find_last_of (sep);
          retval(0) = fname.substr (idx + 1);
        }
      else
        {
          Cell file_cell = Cell (file_count, 1);
          for (octave_idx_type n = 1; n <= file_count; n++)
            {
              fname = fc.value (n);
              idx = fname.find_last_of (sep);
              file_cell(n - 1) = fname.substr (idx + 1);
            }
          retval(0) = file_cell;
        }

      if (multi_type == Fl_File_Chooser::DIRECTORY)
        retval(0) = sys::file_ops::native_separator_path (std::string (fc.value ()));
      else
        {
          retval(1) = sys::file_ops::native_separator_path (std::string (fc.directory ()) + sep);
          retval(2) = fc.filter_value () + 1;
        }
    }

  fc.hide ();
  Fl::flush ();

  return retval;

#else

  octave_unused_parameter (args);

  err_disabled_feature ("__fltk_uigetfile__", "OpenGL and FLTK");

#endif
}

/*
## No test needed for internal helper function.
%!assert (1)
*/

OCTAVE_END_NAMESPACE(octave)
