#ifndef READLINEADAPTER_H
#define READLINEADAPTER_H

#include "octave/config.h"
#include "octave/cmd-edit.h"
#include <QObject>

class ReadlineAdapter : public QObject, public command_editor
{
  Q_OBJECT
public:
  explicit ReadlineAdapter (QObject *parent = 0);

signals:

public slots:

};

#endif // READLINEADAPTER_H
