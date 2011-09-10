#ifndef TERMINALEMULATION_H
#define TERMINALEMULATION_H

#include <QObject>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QTextCursor>

class Terminal
{
  public:
    virtual QTextCursor textCursor () = 0;
    virtual void setTextCursor (const QTextCursor& cursor) = 0;

    virtual void bell () = 0;
};

class TerminalEmulation : public QObject
{
  Q_OBJECT
public:
  static TerminalEmulation *newTerminalEmulation (Terminal *terminal);
  TerminalEmulation ();
  virtual ~TerminalEmulation () {}

  virtual void processKeyEvent (QKeyEvent *keyEvent) = 0;
  virtual void transmitText (const QString& text) = 0;
protected:
  Terminal *m_terminal;
};

#endif // TERMINALEMULATION_H
