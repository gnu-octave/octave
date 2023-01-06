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

#if ! defined (octave_marker_h)
#define octave_marker_h 1

#include <QObject>
#include <Qsci/qsciscintilla.h>

// Defined for purposes of sending QList<int> as part of signal.
#include <QList>
typedef QList<int> QIntList;

// The breakpoint class keeps track of the debug line number that Octave core
// uses and the handle of the marker inside the editor file.  If the editor
// contents is modified, the debug line number and editor line number can be
// out of alignment.  The marker handle can be used to retrieve the editor
// line.

OCTAVE_BEGIN_NAMESPACE(octave)

class marker : public QObject
{
  Q_OBJECT

public:

  // List of all marker types
  // If multiple markers are on the same line, the first one listed
  // is drawn at the back, so big ones should be first.
  enum editor_markers
    {
      breakpoint,
      cond_break,
      unsure_breakpoint,
      bookmark,
      debugger_position,
      unsure_debugger_position,
      selection
    };

  marker (QsciScintilla *edit_area, int original_linenr,
          editor_markers marker_type, const QString& condition = "");

  marker (QsciScintilla *edit_area, int original_linenr,
          editor_markers marker_type, int editor_linenr,
          const QString& condition = "");

  ~marker (void) = default;

  const QString& get_cond (void) const { return m_condition; }

  void set_cond (const QString& cond) { m_condition = cond; }

signals:

  void request_remove (int original_linenr);

public slots:

  void handle_remove_via_original_linenr (int original_linenr);
  void handle_request_remove_via_editor_linenr (int editor_linenr);
  void handle_remove (void);
  void handle_find_translation (int original_linenr, int& editor_linenr,
                                marker*& bp);
  void handle_find_just_before (int linenr, int& original_linenr,
                                int& editor_linenr);
  void handle_find_just_after (int linenr, int& original_linenr,
                               int& editor_linenr);
  /*  void handle_lines_changed (void);*/
  void handle_marker_line_deleted (int mhandle);
  void handle_marker_line_undeleted (int mhandle);
  void handle_report_editor_linenr (QIntList& lines, QStringList& conditions);

private:

  void construct (QsciScintilla *edit_area, int original_linenr,
                  editor_markers marker_type, int editor_linenr,
                  const QString& condition);

  QsciScintilla *m_edit_area;
  int m_original_linenr;
  editor_markers m_marker_type;
  int m_mhandle;
  QString m_condition;
};

OCTAVE_END_NAMESPACE(octave)

#endif
