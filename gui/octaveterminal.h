#ifndef OCTAVETERMINAL_H
#define OCTAVETERMINAL_H

#include <QMdiSubWindow>
#include <QLineEdit>
#include <QTextBrowser>
#include <QAction>
#include <QMenuBar>
#include <QMenu>

#include "client.h"

class OctaveTerminal : public QMdiSubWindow {
    Q_OBJECT
public:
    explicit OctaveTerminal(QWidget *parent = 0);

signals:

public slots:
    void sendCommand();
    void blockUserInput();
    void allowUserInput();

    void assignClient(Client* client);
    void showEnvironment();

protected slots:
    void fetchDataFromClient();
    void fetchErrorFromClient();

private:
    QMenuBar *m_menuBar;
    QTextBrowser *m_octaveOutput;
    QLineEdit *m_commandLine;
    Client *m_client;
};

#endif // OCTAVETERMINAL_H
