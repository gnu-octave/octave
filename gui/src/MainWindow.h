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
#include "OctaveTerminal.h"
#include "OctaveLink.h"
#include "VariablesDockWidget.h"
#include "HistoryDockWidget.h"
#include "FilesDockWidget.h"
#include "CodeEdit.h"
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

protected:
    void closeEvent(QCloseEvent *closeEvent);
    void readSettings();
    void writeSettings();

private:
    void constructWindow();
    void establishOctaveLink();
    OctaveTerminal *m_octaveTerminal;
    VariablesDockWidget *m_variablesDockWidget;
    HistoryDockWidget *m_historyDockWidget;
    FilesDockWidget *m_filesDockWidget;
    QMdiArea *m_openedFiles;
    QTabWidget *m_centralTabWidget;
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
        octave_main(argc, (char**)argv,1);
        switch_to_buffer(create_buffer(get_input_from_stdin()));
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
        QList<OctaveLink::VariableMetaData> variables
                = OctaveLink::instance()->variableInfoList();
        if(variables.size()) {
            m_mainWindow->variablesDockWidget()->setVariablesList(variables);
        }

        // Check whether any requested variables have been returned.
        QList<OctaveLink::RequestedVariable> reqVars
                = OctaveLink::instance()->requestedVariables();

        //for(std::vector<OctaveLink::RequestedVariable>::iterator it = reqVars.begin();
        //   it != reqVars.end(); it++ ) {
            // TODO: Process requested variables.
        //}

        // Collect history list.
        string_vector historyList = OctaveLink::instance()->getHistoryList();
        if(historyList.length()) {
            m_mainWindow->historyDockWidget()->updateHistory(historyList);
        }

        // Put a marker in each buffer at the proper location.
        int status = 0;
        std::vector<OctaveLink::BreakPoint> breakPoints = OctaveLink::instance()->breakPointList(status);
        if(status==0) {
            //MEditor::GetInstance()->process_breakpoint_list (bps);
        }

        // Find out if a breakpoint is hit
        static bool lineNumber = -1;
        bool hitBreakPoint = OctaveLink::instance()->isBreakpointReached(status);
        if((status==0) && hitBreakPoint) {
            std::vector<OctaveLink::BreakPoint> hit_breakpoint = OctaveLink::instance()->reachedBreakpoint();

            if(hit_breakpoint.size() > 0 && (hit_breakpoint[0].lineNumber != lineNumber)) {
                //MEditor::GetInstance()->remove_hit_breakpoint_marker ();
                //MEditor::GetInstance()->add_breakpoint_marker(hit_breakpoint[0], BP_MARKER_TYPE_HIT);
                lineNumber = hit_breakpoint[0].lineNumber;
            }
        }
        else if((status==0) && lineNumber>0) {
            //MEditor::GetInstance()->remove_hit_breakpoint_marker ();
            lineNumber = -1;
        }

            usleep(100000);
        }
    }
private:
    MainWindow *m_mainWindow;
};

#endif // MAINWINDOW_H
