/*

Copyright (C) 2016 Daniel J. Sebald

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

#if defined (HAVE_QSCINTILLA)

#include "marker.h"


marker::marker (QsciScintilla *area, int original_linenr, editor_markers type,
                int editor_linenr, const QString& condition) : QObject ()
{
  construct (area, original_linenr, type, editor_linenr, condition);
}


marker::marker (QsciScintilla *area, int original_linenr,
                editor_markers type, const QString& condition) : QObject ()
{
  construct (area, original_linenr, type, original_linenr - 1, condition);
}


marker::~marker (void)
{ }


void
marker::construct (QsciScintilla *area, int original_linenr,
                   editor_markers type, int editor_linenr,
                   const QString& condition)
{
  _edit_area = area;
  _original_linenr = original_linenr;
  _marker_type = type;
  _mhandle = _edit_area->markerAdd (editor_linenr, _marker_type);
  _condition = condition;
}


void
marker::handle_remove_via_original_linenr (int linenr)
{
  if (_original_linenr == linenr)
    {
      _edit_area->markerDeleteHandle(_mhandle);
      delete this;
    }
}


void
marker::handle_request_remove_via_editor_linenr (int linenr)
{
  // Get line number from the edit area and if it matches
  // the requested line number, remove.
  if (_edit_area->markerLine (_mhandle) == linenr)
    {
      // Rather than delete editor marker directly, issue command
      // to Octave core.  Octave core should signal back to remove
      // this breakpoint via debugger line number.
      emit request_remove (_original_linenr);
    }
}


void
marker::handle_remove (void)
{
  _edit_area->markerDeleteHandle (_mhandle);
  delete this;
}


void
marker::handle_find_translation (int linenr, int& translation_linenr,
                                 marker *& bp)
{
  if (_original_linenr == linenr)
    {
      translation_linenr = _edit_area->markerLine (_mhandle);
      bp = this;
    }
}


void
marker::handle_find_just_before (int linenr, int& original_linenr,
                                 int& editor_linenr)
{
  if (_original_linenr < linenr && _original_linenr >= original_linenr)
    {
      original_linenr = _original_linenr;
      editor_linenr = _edit_area->markerLine (_mhandle);
    }
}


void
marker::handle_find_just_after (int linenr, int& original_linenr,
                                int& editor_linenr)
{
  if (_original_linenr > linenr && _original_linenr <= original_linenr)
    {
      original_linenr = _original_linenr;
      editor_linenr = _edit_area->markerLine (_mhandle);
    }
}


void
marker::handle_report_editor_linenr (QIntList& lines, QStringList& conditions)
{
  lines << _edit_area->markerLine (_mhandle);
  conditions << _condition;
}


void
marker::handle_marker_line_deleted (int mhandle)
{
  // FUTURE SUPPORT: There really should be a signal in QsciScintilla
  // called markerLineDeleted (int mhandle) because there is no way
  // of knowing this.  QsciScintilla will place the marker at a
  // different line rather than remove it from the margin.  I (DJS) will
  // lobby for such a signal.
  if (_mhandle == mhandle)
    {
      if (_marker_type == breakpoint || _marker_type == debugger_position)
        {
          int editor_linenr = _edit_area->markerLine (_mhandle);
          _edit_area->markerDeleteHandle(_mhandle);
          _marker_type = _marker_type == breakpoint ? unsure_breakpoint
                                                    : unsure_debugger_position;
          _mhandle = _edit_area->markerAdd (editor_linenr, _marker_type);
        }
    }
}


void
marker::handle_marker_line_undeleted (int mhandle)
{
  // FUTURE SUPPORT: There really should be a signal in QsciScintilla
  // called markerLineUndeleted (int mhandle) because there is no way
  // of knowing this.  QsciScintilla will place the marker at a
  // different line rather than remove it from the margin.  I (DJS) will
  // lobby for such a signal.
  if (_mhandle == mhandle)
    {
      if (_marker_type == unsure_breakpoint
          || _marker_type == unsure_debugger_position)
        {
          int editor_linenr = _edit_area->markerLine (_mhandle);
          _edit_area->markerDeleteHandle(_mhandle);
          _marker_type = _marker_type == unsure_breakpoint ? breakpoint
                                                           : debugger_position;
          _mhandle = _edit_area->markerAdd (editor_linenr, _marker_type);
        }
    }
}

#endif

