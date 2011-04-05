#ifndef OCTAVETERMINAL_H
#define OCTAVETERMINAL_H

#include <QMdiSubWindow>
#include <QLineEdit>
#include <QTextBrowser>
#include <QAction>
#include <QMenuBar>
#include <QMenu>
#include <QToolBar>
#include <QKeyEvent>

#include "client.h"
#include "terminalhighlighter.h"

class TerminalCommandLine : public QLineEdit {
    Q_OBJECT
public:
    TerminalCommandLine(QWidget *parent = 0)
        : QLineEdit(parent) {
    }

signals:
    void claimCommand(QString command);

protected:
    void keyPressEvent(QKeyEvent *keyEvent) {
        switch(keyEvent->key()) {
            case Qt::Key_Return:
                emit claimCommand(text() + "\n");
                setText("");
                break;

            default:
                QLineEdit::keyPressEvent(keyEvent);
                break;
        }
    }
};

class OctaveTerminal : public QMdiSubWindow {
    Q_OBJECT
public:
    explicit OctaveTerminal(QWidget *parent = 0);

signals:

public slots:
    void sendCommand(QString command);
    void blockUserInput();
    void allowUserInput();

    void assignClient(Client* client);
    void showEnvironment();

protected slots:
    void handleDataFromClient(QString data);
    void handleErrorFromClient(QString error);

private:
    QToolBar *m_mainToolBar;
    QTextBrowser *m_octaveOutput;
    TerminalCommandLine *m_commandLine;
    Client *m_client;
    TerminalHighlighter *m_terminalHighlighter;
};

#endif // OCTAVETERMINAL_H
