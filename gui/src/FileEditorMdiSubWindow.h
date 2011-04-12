#ifndef FILEEDITORMDISUBWINDOW_H
#define FILEEDITORMDISUBWINDOW_H

#include <QMdiSubWindow>
#include <QToolBar>
#include <QStatusBar>
#include "CodeEdit.h"

class FileEditorMdiSubWindow : public QMdiSubWindow {
    Q_OBJECT
public:
    FileEditorMdiSubWindow(QWidget *parent = 0);
    void loadFile(QString fileName);

public slots:
    void saveFile();
    void showToolTipSave();
    void showToolTipUndo();
    void showToolTipRedo();

private:
    void construct();
    QToolBar *m_toolBar;
    CodeEdit *m_codeEdit;
    QStatusBar *m_statusBar;
    QString m_fileName;
};

#endif // FILEEDITORMDISUBWINDOW_H
