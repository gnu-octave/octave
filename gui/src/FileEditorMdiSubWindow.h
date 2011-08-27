/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid
 * jacob.dawid@googlemail.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef FILEEDITORMDISUBWINDOW_H
#define FILEEDITORMDISUBWINDOW_H

#include "MainWindow.h"
#include "TerminalEmulation.h"
#include <QMdiSubWindow>
#include <QToolBar>
#include <QAction>
#include <QMenuBar>
#include <QStatusBar>
#include <QCloseEvent>
#include <Qsci/qsciscintilla.h>
// Not available in the Debian repos yet!
// #include <Qsci/qscilexeroctave.h>
#include "lexer/lexeroctavegui.h"

const char UNNAMED_FILE[]     = "<unnamed>";
const char SAVE_FILE_FILTER[] = "Octave Files (*.m);;All Files (*.*)";
enum MARKER
  {
    MARKER_BOOKMARK,
    MARKER_BREAKPOINT
  };

class FileEditorMdiSubWindow:public QMdiSubWindow
{
Q_OBJECT public:
  FileEditorMdiSubWindow (QWidget * parent = 0);
  ~FileEditorMdiSubWindow ();
  void loadFile (QString fileName);
  void initEditor (TerminalEmulation *terminalEmulation,
                   LexerOctaveGui *lexer,
                   MainWindow *mainWindow);

public slots:

  void newFile ();
  void openFile ();
  void saveFile ();
  void saveFile (QString fileName);
  void saveFileAs ();

  void showToolTipNew ();
  void showToolTipOpen ();
  void showToolTipSave ();
  void showToolTipSaveAs ();
  void showToolTipUndo ();
  void showToolTipRedo ();
  void registerModified (bool modified);

protected:
  void closeEvent(QCloseEvent *event);

private:
  int checkFileModified (QString msg, int cancelButton);
  void construct ();
  void doCommentSelectedText (bool comment);
  QMenuBar *m_menuBar;
  QToolBar *m_toolBar;
  QsciScintilla *m_editor;
  QStatusBar *m_statusBar;
  QString m_fileName;
  TerminalEmulation* m_terminalEmulation;
  QAction* m_copyAction;
  QAction* m_cutAction;
  MainWindow* m_mainWindow;
  int m_markerBookmark;
  bool m_modified;

private slots:
  void handleModificationChanged(bool modified);
  void handleMarginClicked(int line, int margin, Qt::KeyboardModifiers state);
  void handleCopyAvailable(bool enableCopy);
  void runFile();
  void removeBookmark ();
  void toggleBookmark ();
  void nextBookmark();
  void prevBookmark();
  void commentSelectedText();
  void uncommentSelectedText();

};

#endif // FILEEDITORMDISUBWINDOW_H
