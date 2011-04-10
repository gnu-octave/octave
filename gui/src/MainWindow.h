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
#include "OctaveTerminal.h"
#include "OctaveLink.h"
#include "VariablesDockWidget.h"
#include "HistoryDockWidget.h"

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
/**
  * \class MainWindow
  *
  * Represents the main window.
  */
class MainWindow : public QMainWindow
{
    Q_OBJECT
public:
    static void* octaveMainWrapper(void *widget);
    static void* octaveCallback(void *widget);

    MainWindow(QWidget *parent = 0);
    ~MainWindow();

public slots:
private:
    void constructWindow();
    void establishOctaveLink();
    OctaveTerminal *m_octaveTerminal;
    VariablesDockWidget *m_variablesDockWidget;
    HistoryDockWidget *m_historyDockWidget;

    // Threads for running octave and managing the data interaction.
    OctaveLink *m_octaveLink;
    pthread_t m_octaveThread;
    pthread_t m_octaveCallbackThread;
    bool isRunning;
};

#endif // MAINWINDOW_H
