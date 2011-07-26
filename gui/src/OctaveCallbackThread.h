#ifndef OCTAVECALLBACKTHREAD_H
#define OCTAVECALLBACKTHREAD_H

#include <QThread>
#include "MainWindow.h"

class OctaveCallbackThread:public QThread
{
  Q_OBJECT
public:
  OctaveCallbackThread (QObject * parent, MainWindow * mainWindow);

protected:
  void run ();
private:
  MainWindow * m_mainWindow;
};

#endif // OCTAVECALLBACKTHREAD_H
