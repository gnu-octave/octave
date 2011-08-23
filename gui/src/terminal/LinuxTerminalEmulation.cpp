#include "LinuxTerminalEmulation.h"

LinuxTerminalEmulation::LinuxTerminalEmulation ()
  : TerminalEmulation ()
{
  int fdm, fds;
  if (openpty (&fdm, &fds, 0, 0, 0) < 0)
    {
      assert (0);
    }
  dup2 (fds, STDIN_FILENO);
  dup2 (fds, STDOUT_FILENO);
  dup2 (fds, STDERR_FILENO);

  m_pty = new KPtyDevice ();
  m_pty->open (fdm);
  connect (m_pty, SIGNAL(readyRead ()),
           this, SLOT (handleReadyRead ()));
}

LinuxTerminalEmulation::~LinuxTerminalEmulation ()
{
  m_pty->close ();
}

void LinuxTerminalEmulation::processKeyEvent (QKeyEvent *keyEvent)
{
  switch (keyEvent->key ())
    {
      case Qt::Key_PageUp:
      //if (verticalScrollBar ())
      //  verticalScrollBar ()->setValue (verticalScrollBar ()->value () - 10);
      return;
      case Qt::Key_PageDown:
      //if (verticalScrollBar ())
      //  verticalScrollBar ()->setValue (verticalScrollBar ()->value () + 10);
      return;

      case Qt::Key_Up:
      m_pty->write ("\033OA");
      break;

      case Qt::Key_Down:
      m_pty->write ("\033OB");
      break;

      case Qt::Key_Right:
      m_pty->write ("\033OC");
      break;

      case Qt::Key_Left:
      m_pty->write ("\033OF");
      break;

      //case Qt::Key_Backspace:
      //m_pty->sendData ("\03308");
      //break;

      default:
      m_pty->write (keyEvent->text ().toAscii ());
      break;
    }
  keyEvent->accept ();
}

void
LinuxTerminalEmulation::transmitText (const QString &text)
{
  m_pty->write (text.toLocal8Bit ());
}

void
LinuxTerminalEmulation::handleReadyRead ()
{
  QByteArray data = m_pty->readAll ();

  int position;
  QTextCursor tc = m_terminal->textCursor ();
  tc.movePosition (QTextCursor::End);

  // Decode data into cursor actions.
  foreach(QChar character, data)
    {
      unsigned short unicode = character.unicode ();
      switch (unicode)
        {
        case 0: // Null (NUL)
          qDebug ("NUL");
          break;
        case 1: // Start Of Heading (SOH)
          qDebug ("SOH");
          break;
        case 2: // Start Of Text (STX)
          qDebug ("STX");
          break;
        case 3: // End Of Text (ETX)
          qDebug ("ETX");
          break;
        case 4: // End Of Transmission (EOT)
          qDebug ("EOT");
          break;
        case 5: // Enquiry (ENQ)
          qDebug ("ENQ");
          break;
        case 6: // Acknowledgement (ACK)
          qDebug ("ACK");
          break;
        case 7: // Bell (BEL)
          m_terminal->bell ();
          break;
        case 8: // Backspace (BS)
          tc.deletePreviousChar ();
          break;
        case 9: // Horizontal Tab (HT)
          qDebug ("HT");
          break;
        case 10: // Line Feed (LF)
          position = tc.position ();
          tc.movePosition (QTextCursor::EndOfLine);
          tc.insertText ("\n");
          tc.setPosition (position);
          tc.movePosition (QTextCursor::Down);
          break;
        case 11: // Vertical Tab (VT)
          qDebug ("VT");
          break;
        case 12: // Form Feed (FF)
          qDebug ("FF");
          break;
        case 13: // Carriage Return (CR)
          tc.movePosition (QTextCursor::StartOfLine, QTextCursor::KeepAnchor);
          break;
        case 14: // Shift Out (SO)
          qDebug ("SO");
          break;
        case 15: // Shift In (SI)
          qDebug ("SI");
          break;
        case 16: // Data Link Escape (DLE)
          qDebug ("DLE");
          break;
        case 17: // Device Control 1 (DC1, XON)
          qDebug ("DC1");
          break;
        case 18: // Device Control 2 (DC2)
          qDebug ("DC2");
          break;
        case 19: // Device Control 3 (DC3, XOFF)
          qDebug ("DC3");
          break;
        case 20: // Device Control 4 (DC4)
          qDebug ("DC4");
          break;
        case 21: // Negative Acknowledgement (NAK)
          qDebug ("NAK");
          break;
        case 22: // Synchronous Idle (SYN)
          qDebug ("SYN");
          break;
        case 23: // End Of Transmission Block (ETB)
          qDebug ("ETB");
          break;
        case 24: // Cancel (CAN)
          qDebug ("CAN");
          break;
        case 25: // End of Medium (EM)
          qDebug ("EM");
          break;
        case 26: // Substitute (SUB)
          qDebug ("SUB");
          break;
        case 27: // Escape (ESC)
          qDebug ("ESC");
          break;
        case 28: // File Separator (FS)
          qDebug ("FS");
          break;
        case 29: // Group Separator (GS)
          qDebug ("GS");
          break;
        case 30: // Record Separator (RS)
          qDebug ("RS");
          break;
        case 31: // Unit Separator (US)
          qDebug ("US");
          break;
        case 127: // Delete (DEL)
          break;
        default:
          tc.insertText (character);
          break;
        }
    }
  m_terminal->setTextCursor (tc);
}
