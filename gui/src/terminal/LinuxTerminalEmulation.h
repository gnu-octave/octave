#ifndef LINUXTERMINALEMULATION_H
#define LINUXTERMINALEMULATION_H

#include "TerminalEmulation.h"
#include "Pty.h"

#include "pty.h"
#include "unistd.h"
#include <assert.h>
#include <cstdio>

class LinuxTerminalEmulation : public TerminalEmulation
{
  Q_OBJECT
public:
  LinuxTerminalEmulation ();
  ~LinuxTerminalEmulation ();

  void processKeyEvent (QKeyEvent *keyEvent);
  void transmitText (const QString &text);

private slots:
  void handleReceivedData (const QByteArray& data);

private:
  Pty *m_pty;
};

#endif // LINUXTERMINALEMULATION_H
