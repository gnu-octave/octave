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

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QtGui/QMainWindow>
#include <QThread>
#include <QTabWidget>
#include <QMdiArea>
#include <QStatusBar>
#include <QToolBar>
#include <QQueue>
#include "OctaveTerminal.h"
#include "OctaveLink.h"
#include "VariablesDockWidget.h"
#include "HistoryDockWidget.h"
#include "FilesDockWidget.h"
#include "SimpleEditor.h"
#include "BrowserWidget.h"

// Octave includes
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#undef PACKAGE_URL
#include "octave/config.h"

#include "octave/debug.h"
#include "octave/octave.h"
#include "octave/symtab.h"
#include "octave/parse.h"
#include "octave/unwind-prot.h"
#include "octave/toplev.h"
#include "octave/load-path.h"
#include "octave/error.h"
#include "octave/quit.h"
#include "octave/variables.h"
#include "octave/sighandlers.h"
#include "octave/sysdep.h"
#include "octave/str-vec.h"
#include "octave/cmd-hist.h"
#include "octave/cmd-edit.h"
#include "octave/oct-env.h"
#include "octave/symtab.h"
#include "cmd-edit.h"

typedef struct yy_buffer_state *YY_BUFFER_STATE;
extern OCTINTERP_API YY_BUFFER_STATE create_buffer (FILE *f);
extern OCTINTERP_API void switch_to_buffer (YY_BUFFER_STATE buf);
extern OCTINTERP_API FILE *get_input_from_stdin (void);

// System
#include <termios.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <iostream>
#include <vector>
#include "pty.h"

class OctaveMainThread;
class OctaveCallbackThread;

class TabWidgetWithShortcuts : public QTabWidget {
public:
    TabWidgetWithShortcuts(QWidget *parent = 0)
        : QTabWidget(parent),
          m_showingShortcuts(false) {
        setFocusPolicy(Qt::NoFocus);
    }

protected:
    bool eventFilter(QObject *object, QEvent *event) {
        if(event->type() == QEvent::KeyPress) {
            QKeyEvent *keyEvent = static_cast<QKeyEvent *>(event);
            if(keyEvent->modifiers() == Qt::ControlModifier) {
                showShortcuts();
                switch(keyEvent->key()) {
                case Qt::Key_1: setCurrentIndex(0); return true;
                case Qt::Key_2: setCurrentIndex(1); return true;
                case Qt::Key_3: setCurrentIndex(2); return true;
                case Qt::Key_4: setCurrentIndex(3); return true;
                };
            }
        } else if(event->type() == QEvent::KeyRelease) {
            QKeyEvent *keyEvent = static_cast<QKeyEvent *>(event);
            if(keyEvent->modifiers() != Qt::ControlModifier) {
                hideShortcuts();
            }
        }
        return QTabWidget::eventFilter(object, event);
    }

private:
    void showShortcuts() {
        if(m_showingShortcuts)
            return;

        int tabCount = count();
        for(int tabIndex = 0; tabIndex < tabCount; tabIndex++) {
            m_backuppedTabTitles.push_back(tabText(tabIndex));
            setTabText(tabIndex, QString("%1 [%2]").arg(tabText(tabIndex)).arg(tabIndex + 1));
        }

        m_showingShortcuts = true;
    }

    void hideShortcuts() {
        if(!m_showingShortcuts)
            return;
        int tabCount = count();
        for(int tabIndex = 0; tabIndex < tabCount; tabIndex++) {
            setTabText(tabIndex, m_backuppedTabTitles.front());
            m_backuppedTabTitles.pop_front();
        }
        m_showingShortcuts = false;
    }

    bool m_showingShortcuts;
    QQueue<QString> m_backuppedTabTitles;
};

/**
  * \class MainWindow
  *
  * Represents the main window.
  */
class MainWindow : public QMainWindow {
    Q_OBJECT
public:
    MainWindow(QWidget *parent = 0);
    ~MainWindow();

    bool isRunning() { return m_isRunning; }
    OctaveTerminal *octaveTerminal() { return m_octaveTerminal; }
    VariablesDockWidget *variablesDockWidget() { return m_variablesDockWidget; }
    HistoryDockWidget *historyDockWidget() { return m_historyDockWidget; }
    FilesDockWidget *filesDockWidget() { return m_filesDockWidget; }

public slots:
    void handleOpenFileRequest(QString fileName);
    void reportStatusMessage(QString statusMessage);
    void openWebPage(QString url);
    void handleSaveWorkspaceRequest();
    void handleLoadWorkspaceRequest();
    void handleClearWorkspaceRequest();
    void handleCommandDoubleClicked(QString command);

protected:
    void closeEvent(QCloseEvent *closeEvent);
    void readSettings();
    void writeSettings();

private:
    void construct();
    void establishOctaveLink();
    OctaveTerminal *m_octaveTerminal;
    VariablesDockWidget *m_variablesDockWidget;
    HistoryDockWidget *m_historyDockWidget;
    FilesDockWidget *m_filesDockWidget;
    QMdiArea *m_openedFiles;
    TabWidgetWithShortcuts *m_centralTabWidget;
    QStatusBar *m_statusBar;
    QToolBar *m_generalPurposeToolbar;
    BrowserWidget *m_browserWidget;
    BrowserWidget *m_serviceWidget;
    QString m_settingsFile;

    // Threads for running octave and managing the data interaction.
    OctaveMainThread *m_octaveMainThread;
    OctaveCallbackThread *m_octaveCallbackThread;
    bool m_isRunning;
};

class OctaveMainThread : public QThread {
    Q_OBJECT
public:
    OctaveMainThread(QObject *parent)
        : QThread(parent) {
    }
protected:
    void run() {
        int argc = 3;
        const char* argv[] = {"octave", "--interactive", "--line-editing"};
        octave_main(argc, (char**)argv, 1);
        main_loop();
        clean_up_and_exit(0);
    }
};

class OctaveCallbackThread : public QThread {
    Q_OBJECT
public:
    OctaveCallbackThread(QObject *parent, MainWindow *mainWindow)
        : QThread(parent),
          m_mainWindow(mainWindow) {
    }

protected:
    void run() {
        while(m_mainWindow->isRunning()) {

        // Get a full variable list.
        QList<SymbolRecord> symbolTable = OctaveLink::instance()->currentSymbolTable();
        if(symbolTable.size()) {
            m_mainWindow->variablesDockWidget()->setVariablesList(symbolTable);
        }

        // Collect history list.
        string_vector historyList = OctaveLink::instance()->currentHistory();
        if(historyList.length()) {
            m_mainWindow->historyDockWidget()->updateHistory(historyList);
        }

            usleep(100000);
        }
    }
private:
    MainWindow *m_mainWindow;
};

#endif // MAINWINDOW_H
