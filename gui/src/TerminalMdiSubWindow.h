#ifndef TERMINALMDISUBWINDOW_H
#define TERMINALMDISUBWINDOW_H

#include <QMdiSubWindow>
#include <QTreeView>
#include <QListView>
#include <QStatusBar>
#include "QTerminalWidget.h"
#include "OctaveLink.h"


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

class TerminalMdiSubWindow : public QMdiSubWindow {
    Q_OBJECT
public:
    /**
     * Stops the monitor thread.
     */
    void stopMonitorRunning(void) { this->isRunning = false; }

    /**
     * Checks if the monitor thread is currently running.
     */
    bool isMonitorRunning() { return this->isRunning; }

    void updateHistory(string_vector historyEntries);
    static void* octave_monitor(void *octaveUI);
    TerminalMdiSubWindow(QWidget *parent = 0);
    ~TerminalMdiSubWindow();

private slots:

private:
    void establishOctaveLink();
    void constructWindow();
    QTerminalWidget *m_terminalWidget;
    QTreeView *m_variableView;
    QListView *m_commandHistoryView;
    QStatusBar *m_statusBar;
    OctaveLink *m_octaveLink;

    // Threads for running octave and managing the data interaction
    pthread_t octave_thread;
    pthread_t octave_monitor_thread;
    bool isRunning;
};
#endif // TERMINALMDISUBWINDOW_H
