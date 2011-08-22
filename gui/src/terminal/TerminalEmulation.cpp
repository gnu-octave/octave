#include "TerminalEmulation.h"

#ifdef Q_OS_UNIX
  #include "LinuxTerminalEmulation.h"
#endif

TerminalEmulation *TerminalEmulation::newTerminalEmulation (Terminal *terminal)
{
#ifdef Q_OS_UNIX
  TerminalEmulation *terminalEmulation = new LinuxTerminalEmulation ();
  terminalEmulation->m_terminal = terminal;
  return terminalEmulation;
#endif
}

TerminalEmulation::TerminalEmulation ()
  : QObject ()
{
}
