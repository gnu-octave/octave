#include "TerminalMdiSubWindow.h"
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QStringListModel>
#include <QStringList>

void * octave_main_wrapper(void *ptr)
{
  //MainWindow *mainWindow = (MainWindow*)ptr;

  int argc = 3;
  const char* argv[] = {"octave", "--interactive", "--line-editing"};
  octave_main(argc,(char**)argv,1);
  switch_to_buffer (create_buffer (get_input_from_stdin ()));

  main_loop();
  clean_up_and_exit(0);
  return 0;
}

TerminalMdiSubWindow::TerminalMdiSubWindow(QWidget *parent)
    : QMdiSubWindow(parent),
      m_terminalWidget(0),
      isRunning(true) {
    constructWindow();
    establishOctaveLink();
}

TerminalMdiSubWindow::~TerminalMdiSubWindow() {
    delete m_octaveLink;
    isRunning = false;
}

void TerminalMdiSubWindow::establishOctaveLink() {
    m_octaveLink = new OctaveLink();
    pthread_create(&octave_thread, NULL, octave_main_wrapper, (void*)this);
    pthread_create(&octave_monitor_thread, 0, TerminalMdiSubWindow::octaveCallback, this);
    command_editor::add_event_hook(server_rl_event_hook_function);

    int fdm, fds;
    if(openpty(&fdm, &fds, 0, 0, 0) < 0) {
        fprintf (stderr, "oops!\n");
    }
    dup2 (fds, 0);
    dup2 (fds, 1);
    dup2 (fds, 2);
    m_terminalWidget->openTeletype(fdm);
}

void TerminalMdiSubWindow::constructWindow() {
    setWindowTitle("Octave Session");
    resize(900, 600);
    setWidget(new QWidget(this));

    QVBoxLayout *vBoxLayout = new QVBoxLayout();

        QWidget *hWidget = new QWidget();
        QHBoxLayout *hBoxLayout = new QHBoxLayout();

        m_terminalWidget = new QTerminalWidget(0, hWidget);
        m_terminalWidget->setScrollBarPosition(QTerminalWidget::ScrollBarRight);
        m_terminalWidget->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        m_terminalWidget->setColorScheme(QTerminalWidget::BlackOnLightYellow);

            QWidget *hvWidget = new QWidget();
            QVBoxLayout *hvBoxLayout = new QVBoxLayout();
            m_variableView = new QTreeView(hWidget);
            m_commandHistoryView = new QListView(hWidget);
            m_commandHistoryView->setModel(new QStringListModel());
            hvWidget->setMaximumWidth(300);
            hvBoxLayout->addWidget(new QLabel("Variables", hWidget));
            hvBoxLayout->addWidget(m_variableView);
            hvBoxLayout->addWidget(new QLabel("Command History", hWidget));
            hvBoxLayout->addWidget(m_commandHistoryView);
            hvBoxLayout->setMargin(1);
            hvWidget->setLayout(hvBoxLayout);

        hBoxLayout->addWidget(m_terminalWidget);
        hBoxLayout->addWidget(hvWidget);
        hBoxLayout->setMargin(2);
        hWidget->setLayout(hBoxLayout);

        m_statusBar = new QStatusBar();

    vBoxLayout->addWidget(hWidget);
    vBoxLayout->addWidget(m_statusBar);
    vBoxLayout->setMargin(2);
    widget()->setLayout(vBoxLayout);

    m_statusBar->showMessage("Ready.");
}

void TerminalMdiSubWindow::updateHistory(string_vector historyEntries) {
    QStringListModel * model = dynamic_cast<QStringListModel*>(m_commandHistoryView->model());
    if(!model)
        return;

    QStringList stringList = model->stringList();
    for(size_t i = 0; i < historyEntries.length(); i++)
        stringList.append(QString(historyEntries[i].c_str()));

    model->setStringList(stringList);
}

void* TerminalMdiSubWindow::octaveCallback(void *window) {
    TerminalMdiSubWindow* terminalWindow = (TerminalMdiSubWindow*)window;

    while(terminalWindow->isRunning) {

    // Get a full variable list.
    std::vector<OctaveLink::VariableMetaData> variables = oct_octave_server.variableInfoList();
    if(variables.size()) {
        // TODO: Update variable list model data.
    }

    // Check whether any requested variables have been returned.
    std::vector<OctaveLink::RequestedVariable> reqVars = oct_octave_server.requestedVariables();
    for(std::vector<OctaveLink::RequestedVariable>::iterator it = reqVars.begin();
        it != reqVars.end(); it++ ) {
        // TODO: Process requested variables.
    }

    // Collect history list.
    string_vector historyList = oct_octave_server.getHistoryList();
    if(historyList.length()) {
        terminalWindow->updateHistory(historyList);
    }

    // Put a marker in each buffer at the proper location.
    int status = 0;
    std::vector<OctaveLink::BreakPoint> breakPoints = oct_octave_server.breakPointList(status);
    if(status==0) {
        //MEditor::GetInstance()->process_breakpoint_list (bps);
    }

    // Find out if a breakpoint is hit
    static bool lineNumber = -1;
    bool hitBreakPoint = oct_octave_server.isBreakpointReached(status);
    if((status==0) && hitBreakPoint) {
        std::vector<OctaveLink::BreakPoint> hit_breakpoint = oct_octave_server.reachedBreakpoint();

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

    return 0;
}
