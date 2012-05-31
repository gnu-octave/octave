/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid (jacob.dawid@googlemail.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef FILEEDITORMDISUBWINDOW_H
#define FILEEDITORMDISUBWINDOW_H

#include "main-window.h"
#include "file-editor-interface.h"
#include "file-editor-tab.h"

#include <QToolBar>
#include <QAction>
#include <QMenuBar>
#include <QStatusBar>
#include <QCloseEvent>
#include <QTabWidget>
#include <Qsci/qsciapis.h>
// Not available in the Debian repos yet!
// #include <Qsci/qscilexeroctave.h>
#include "lexer-octave-gui.h"

const char UNNAMED_FILE[]     = "<unnamed>";
const char SAVE_FILE_FILTER[] = "Octave Files (*.m);;All Files (*.*)";
enum MARKER
  {
    MARKER_BOOKMARK,
    MARKER_BREAKPOINT
  };

class file_editor : public file_editor_interface
{
Q_OBJECT

public:
  file_editor (QTerminal *terminal, main_window *mainWindow);
  ~file_editor ();
  void loadFile (QString fileName);
  lexer_octave_gui *lexer ();
  QTerminal *terminal ();
  main_window *mainWindow ();

public slots:
  void request_new_file ();
  void request_open_file ();
  void request_open_file (QString fileName);

  void requestUndo ();
  void requestRedo ();
  void requestCopy ();
  void requestCut ();
  void requestPaste ();
  void requestSaveFile ();
  void requestSaveFileAs ();
  void requestRunFile ();
  void requestToggleBookmark ();
  void requestNextBookmark ();
  void requestPreviousBookmark ();
  void requestRemoveBookmark ();
  void requestCommentSelectedText ();
  void requestUncommentSelectedText ();

  void handleFileNameChanged (QString fileName);
  void handleTabCloseRequest (int index);
  void handleTabCloseRequest ();
  void activeTabChanged (int index);
  void handleEditorStateChanged ();

private:
  void construct ();
  void addFileEditorTab(file_editor_tab *fileEditorTab);
  file_editor_tab *activeEditorTab();

  QMenuBar *m_menuBar;
  QToolBar *m_toolBar;
  QAction* m_copyAction;
  QAction* m_cutAction;
  QTabWidget *m_tabWidget;
  int m_markerBookmark;

  lexer_octave_gui *m_lexer;
  QsciAPIs *m_lexerAPI;
};

#endif // FILEEDITORMDISUBWINDOW_H
