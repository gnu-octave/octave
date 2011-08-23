#ifndef LINUXTERMINALEMULATION_H
#define LINUXTERMINALEMULATION_H

#include "TerminalEmulation.h"
#include "pty.h"
#include "unistd.h"
#include <assert.h>
#include <cstdio>
#include "kptydevice.h"

class LinuxTerminalEmulation : public TerminalEmulation
{
  Q_OBJECT
public:
  LinuxTerminalEmulation ();
  ~LinuxTerminalEmulation ();

  void processKeyEvent (QKeyEvent *keyEvent);
  void transmitText (const QString &text);

private slots:
  void handleReadyRead ();

private:
  KPtyDevice *m_pty;
};

#endif // LINUXTERMINALEMULATION_H
