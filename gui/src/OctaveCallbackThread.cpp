#include "OctaveCallbackThread.h"
#include "MainWindow.h"

OctaveCallbackThread::OctaveCallbackThread (QObject * parent):QThread (parent)
{
}

void
OctaveCallbackThread::run ()
{
  while (1)
    {
      OctaveLink::instance ()->fetchSymbolTable ();
      OctaveLink::instance ()->updateHistoryModel ();
      usleep (500000);
    }
}
