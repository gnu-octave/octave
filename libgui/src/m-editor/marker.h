/*
Copyright (C) 2015 Daniel J. Sebald

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

#if ! defined (octave_marker_h)
#define octave_marker_h 1

#include <Qsci/qsciscintilla.h>
#include <QObject>

// Defined for purposes of sending QList<int> as part of signal.
#include <QList>
typedef QList<int> QIntList;

// The breakpoint class keeps track of the debug line number that Octave core
// uses and the handle of the marker inside the editor file.  If the editor
// contents is modified, the debug line number and editor line number can be
// out of alignment.  The marker handle can be used to retrieve the editor
// line.

class marker;
class marker : public QObject
{
  Q_OBJECT

public:

  enum editor_markers
    {
      bookmark,
      breakpoint,
      cond_break,
      unsure_breakpoint,
      debugger_position,
      unsure_debugger_position
    };

  marker (QsciScintilla *edit_area, int original_linenr,
          editor_markers marker_type, const QString& condition = "");
  marker (QsciScintilla *edit_area, int original_linenr,
          editor_markers marker_type, int editor_linenr,
          const QString& condition = "");
  ~marker (void);

  const QString& get_cond (void) const { return _condition; }

  void set_cond (const QString& cond) { _condition = cond; }

public slots:
  void handle_remove_via_original_linenr (int original_linenr);
  void handle_request_remove_via_editor_linenr (int editor_linenr);
  void handle_remove (void);
  void handle_find_translation (int original_linenr, int& editor_linenr,
                                marker*& bp);
  void handle_find_just_before (int linenr, int& original_linenr, int& editor_linenr);
  void handle_find_just_after (int linenr, int& original_linenr, int& editor_linenr);
/*  void handle_lines_changed (void);*/
  void handle_marker_line_deleted (int mhandle);
  void handle_marker_line_undeleted (int mhandle);
  void handle_report_editor_linenr (QIntList& lines, QStringList& conditions);

signals:
  void request_remove (int original_linenr);

private:
  void construct (QsciScintilla *edit_area, int original_linenr,
                  editor_markers marker_type, int editor_linenr,
                  const QString& condition);

  QsciScintilla *       _edit_area;
  int                   _original_linenr;
  editor_markers        _marker_type;
  int                   _mhandle;
  QString               _condition;
};

#endif // MARKER_H
