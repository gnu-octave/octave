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

#ifndef FILEEDITORTAB_H
#define FILEEDITORTAB_H

#include <Qsci/qsciscintilla.h>
#include <QWidget>
#include <QCloseEvent>
#include <QFileSystemWatcher>

class FileEditor;
class FileEditorTab : public QWidget
{
  Q_OBJECT
public:
  FileEditorTab (FileEditor *fileEditor);
  bool copyAvailable ();

public slots:
  void newTitle(bool modified);
  void handleCopyAvailable(bool enableCopy);
  void handleMarginClicked (int line, int margin, Qt::KeyboardModifiers state);
  void commentSelectedText ();
  void uncommentSelectedText ();
  void removeBookmark ();
  void toggleBookmark ();
  void nextBookmark ();
  void previousBookmark ();
  void cut ();
  void copy ();
  void paste ();
  void undo ();
  void redo ();

  void setModified (bool modified = true);

  bool openFile();
  void loadFile (QString fileName);
  void newFile ();
  bool saveFile ();
  bool saveFile(QString saveFileName);
  bool saveFileAs();
  void runFile ();

  void fileHasChanged (QString fileName);

signals:
  void fileNameChanged (QString fileName);
  void editorStateChanged ();
  void closeRequest ();

protected:
  void closeEvent (QCloseEvent *event);
  void setFileName (QString fileName);

private:
  void updateTrackedFile ();
  int checkFileModified (QString msg, int cancelButton);
  void doCommentSelectedText (bool comment);

  FileEditor *m_fileEditor;
  QsciScintilla *m_editArea;

  QString m_fileName;
  QString m_fileNameShort;

  bool m_longTitle;
  bool m_copyAvailable;

  QFileSystemWatcher m_fileSystemWatcher;
};

#endif // FILEEDITORTAB_H
