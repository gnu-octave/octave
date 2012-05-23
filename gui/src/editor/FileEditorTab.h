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

class FileEditor;
class FileEditorTab : public QWidget
{
  Q_OBJECT
public:
  FileEditorTab (FileEditor *fileEditor);

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

  void setModified (bool modified);

  void openFile ();
  void loadFile (QString fileName);
  void newFile ();
  void saveFile ();
  void saveFile (QString saveFileName);
  void saveFileAs ();
  void runFile ();

signals:
  void fileNameChanged(QString fileName);

protected:
  void closeEvent(QCloseEvent *event);

private:
  int checkFileModified (QString msg, int cancelButton);
  void doCommentSelectedText (bool comment);

  FileEditor *m_fileEditor;
  QsciScintilla *m_editArea;

  QString m_fileName;
  QString m_fileNameShort;

  bool m_modified;
  bool m_longTitle;

  // TODO: Use QFileSystemWatcher to sync with disc.
};

#endif // FILEEDITORTAB_H
