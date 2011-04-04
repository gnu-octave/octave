#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QtGui/QMainWindow>
#include <QMdiArea>

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QWidget *parent = 0);
    ~MainWindow();

    void addOctaveTerminal();
    void loadWebPage(QString title, QString url);

private:
    QMdiArea *m_mdiArea;
};

#endif // MAINWINDOW_H
