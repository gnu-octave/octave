#include "OctaveCallbackThread.h"

OctaveCallbackThread::OctaveCallbackThread (QObject * parent,
                      MainWindow * mainWindow):QThread (parent),
  m_mainWindow (mainWindow)
{
}

void
OctaveCallbackThread::run ()
{
  while (m_mainWindow->isRunning ())
    {
      OctaveLink::instance ()->fetchSymbolTable ();

      // Get a full variable list.
      QList < SymbolRecord > symbolTable = OctaveLink::instance ()->copyCurrentSymbolTable ();
      if (symbolTable.size ())
        {
          m_mainWindow->variablesDockWidget ()->setVariablesList (symbolTable);
        }

      OctaveLink::instance ()->updateHistoryModel ();
      usleep (500000);
    }
}
