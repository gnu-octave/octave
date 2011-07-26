#ifndef OCTAVECALLBACKTHREAD_H
#define OCTAVECALLBACKTHREAD_H

#include <QThread>

class OctaveCallbackThread:public QThread
{
  Q_OBJECT
public:
  OctaveCallbackThread (QObject * parent);
protected:
  void run ();

};

#endif // OCTAVECALLBACKTHREAD_H
