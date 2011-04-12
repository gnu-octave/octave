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

#include "FileEditorMdiSubWindow.h"
#include <QVBoxLayout>
#include <QApplication>
#include <QFile>
#include <QFileDialog>

FileEditorMdiSubWindow::FileEditorMdiSubWindow(QWidget *parent)
    : QMdiSubWindow(parent) {
    construct();
}

void FileEditorMdiSubWindow::loadFile(QString fileName) {
    m_fileName = fileName;
    setWindowTitle(fileName);
    QFile file(fileName);
    file.open(QFile::ReadOnly);

    m_codeEdit->setPlainText(file.readAll());

    file.close();
}

void FileEditorMdiSubWindow::saveFile() {
    QString saveFileName = QFileDialog::getSaveFileName(this, "Save File", m_fileName);
    QFile file(saveFileName);
    file.open(QFile::WriteOnly);
    file.write(m_codeEdit->toPlainText().toLocal8Bit());
    file.close();
}

void FileEditorMdiSubWindow::showToolTipSave() {
    m_statusBar->showMessage("Save the file.", 2000);
}

void FileEditorMdiSubWindow::showToolTipUndo() {
    m_statusBar->showMessage("Revert previous changes.", 2000);
}

void FileEditorMdiSubWindow::showToolTipRedo() {
    m_statusBar->showMessage("Append previous changes.", 2000);
}

void FileEditorMdiSubWindow::construct() {
    QStyle *style = QApplication::style();
    setWidget(new QWidget());
    m_toolBar = new QToolBar(this);
    m_codeEdit = new CodeEdit(this);
    m_statusBar = new QStatusBar(this);
    m_numberedTextView = new NumberedTextView(this, m_codeEdit);

    m_codeEdit->setFontFamily("Courier");
    m_codeEdit->setLineWrapMode(QTextEdit::NoWrap);

    QAction *newAction = new QAction(style->standardIcon(QStyle::SP_FileIcon),
        "", m_toolBar);
    QAction *saveAction = new QAction(style->standardIcon(QStyle::SP_DriveHDIcon),
        "", m_toolBar);
    QAction *undoAction = new QAction(style->standardIcon(QStyle::SP_ArrowLeft),
        "", m_toolBar);
    QAction *redoAction = new QAction(style->standardIcon(QStyle::SP_ArrowRight),
        "", m_toolBar);

    m_toolBar->addAction(newAction);
    m_toolBar->addAction(saveAction);
    m_toolBar->addAction(undoAction);
    m_toolBar->addAction(redoAction);

    QVBoxLayout *layout = new QVBoxLayout();
    layout->addWidget(m_toolBar);
    layout->addWidget(m_numberedTextView);
    layout->addWidget(m_statusBar);
    layout->setMargin(2);
    widget()->setLayout(layout);

    connect(undoAction, SIGNAL(triggered()), m_codeEdit, SLOT(undo()));
    connect(redoAction, SIGNAL(triggered()), m_codeEdit, SLOT(redo()));
    connect(saveAction, SIGNAL(triggered()), this, SLOT(saveFile()));

    connect(undoAction, SIGNAL(hovered()), this, SLOT(showToolTipUndo()));
    connect(redoAction, SIGNAL(hovered()), this, SLOT(showToolTipRedo()));
    connect(saveAction, SIGNAL(hovered()), this, SLOT(showToolTipSave()));
}
