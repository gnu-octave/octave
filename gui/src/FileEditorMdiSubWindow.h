#ifndef FILEEDITORMDISUBWINDOW_H
#define FILEEDITORMDISUBWINDOW_H

#include <QMdiSubWindow>
#include <QToolBar>
#include <QStatusBar>
#include "CodeEdit.h"

class FileEditorMdiSubWindow : public QMdiSubWindow {
public:
    FileEditorMdiSubWindow(QWidget *parent = 0);
    void loadFile(QString fileName);

private:
    void construct();
    QToolBar *m_toolBar;
    CodeEdit *m_codeEdit;
    QStatusBar *m_statusBar;
};

#endif // FILEEDITORMDISUBWINDOW_H
