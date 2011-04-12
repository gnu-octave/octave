#include "FileEditorMdiSubWindow.h"
#include <QVBoxLayout>
#include <QApplication>
#include <QFile>

FileEditorMdiSubWindow::FileEditorMdiSubWindow(QWidget *parent)
    : QMdiSubWindow(parent) {
    construct();
}

void FileEditorMdiSubWindow::loadFile(QString fileName) {
    setWindowTitle(fileName);
    QFile file(fileName);
    file.open(QFile::ReadOnly);

    m_codeEdit->setPlainText(file.readAll());

    file.close();
}

void FileEditorMdiSubWindow::construct() {
    QStyle *style = QApplication::style();
    setWidget(new QWidget());
    m_toolBar = new QToolBar(this);
    m_codeEdit = new CodeEdit(this);
    m_statusBar = new QStatusBar(this);

    m_codeEdit->setFontFamily("Courier");

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
    layout->addWidget(m_codeEdit);
    layout->addWidget(m_statusBar);
    layout->setMargin(2);
    widget()->setLayout(layout);
}
