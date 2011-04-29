/* Quint - A graphical user interface for Octave
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

#ifndef FILEEDITORDOCKWIDGET_H
#define FILEEDITORDOCKWIDGET_H

#include <QDockWidget>
#include <QToolBar>
#include <QStatusBar>
#include "SimpleEditor.h"
#include "NumberedCodeEdit.h"

class FileEditorDockWidget : public QDockWidget {
    Q_OBJECT
public:
    FileEditorDockWidget(QWidget *parent = 0);
    void loadFile(QString fileName);

public slots:
    void newFile();
    void saveFile();

    void showToolTipNew();
    void showToolTipSave();
    void showToolTipUndo();
    void showToolTipRedo();

    void registerModified(bool modified);
private:
    void construct();
    QToolBar *m_toolBar;
    SimpleEditor *m_simpleEditor;
    NumberedCodeEdit *m_numberedTextView;
    QStatusBar *m_statusBar;
    QString m_fileName;
    bool m_modified;
};

#endif // FILEEDITORDOCKWIDGET_H
