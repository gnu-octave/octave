/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid
 * jacob.dawid@googlemail.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "OctaveTerminal.h"
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QStringListModel>
#include <QStringList>
#include <QScrollBar>

#include "pty.h"
#include "unistd.h"
#include <assert.h>

#include <cstdio>

OctaveTerminal::OctaveTerminal (QWidget * parent)
  : QPlainTextEdit (parent)
{
  construct ();
}

OctaveTerminal::~OctaveTerminal ()
{
}

void
OctaveTerminal::construct ()
{
  setSizePolicy (QSizePolicy::Expanding, QSizePolicy::Expanding);
}

void
OctaveTerminal::openTerminal ()
{
  int fdm, fds;
  if (openpty (&fdm, &fds, 0, 0, 0) < 0)
    {
      assert (0);
    }
  dup2 (fds, 0);
  dup2 (fds, 1);
  dup2 (fds, 2);

  m_shellProcess = new Pty (fdm);
  connect (m_shellProcess, SIGNAL(receivedData(QByteArray)),
           this, SLOT(handleReceivedData(QByteArray)));
}

void
OctaveTerminal::keyPressEvent (QKeyEvent * keyEvent)
{
  switch (keyEvent->key ())
    {
      case Qt::Key_PageUp:
      if (verticalScrollBar ())
        verticalScrollBar ()->setValue (verticalScrollBar ()->value () - 10);
      return;
      case Qt::Key_PageDown:
      if (verticalScrollBar ())
        verticalScrollBar ()->setValue (verticalScrollBar ()->value () + 10);
      return;

      case Qt::Key_Up:
      m_shellProcess->sendData ("\EOA");
      break;

      case Qt::Key_Down:
      m_shellProcess->sendData ("\EOB");
      break;

      case Qt::Key_Right:
      m_shellProcess->sendData ("\EOC");
      break;

      case Qt::Key_Left:
      m_shellProcess->sendData ("\EOF");
      break;
    }

  m_shellProcess->sendData (keyEvent->text ().toLocal8Bit ());

  /*
  bool emitKeyPressSignal = true;

  if (event->modifiers () == Qt::ControlModifier)
    {
      switch (event->key ())
    {
    case Qt::Key_C:
      copyClipboard ();
      break;
    case Qt::Key_V:
      pasteClipboard ();
      break;
    };
    }
  else if (event->modifiers () == Qt::ShiftModifier)
    {
      bool update = true;


      else
    update = false;

    }


  Qt::KeyboardModifiers modifiers = keyEvent->modifiers ();
  KeyboardTranslator::States states = KeyboardTranslator::NoState;

  // get current states
  if (getMode (MODE_NewLine))
    states |= KeyboardTranslator::NewLineState;
  if (getMode (MODE_Ansi))
    states |= KeyboardTranslator::AnsiState;
  if (getMode (MODE_AppCuKeys))
    states |= KeyboardTranslator::CursorKeysState;
  if (getMode (MODE_AppScreen))
    states |= KeyboardTranslator::AlternateScreenState;
  if (getMode (MODE_AppKeyPad) && (modifiers & Qt::KeypadModifier))
    states |= KeyboardTranslator::ApplicationKeypadState;

  // check flow control state
  if (modifiers & Qt::ControlModifier)
    {
      if (event->key () == Qt::Key_S)
    emit flowControlKeyPressed (true);
      else
    if (event->key () == Qt::Key_Q)
      emit flowControlKeyPressed (false);
    }

  // lookup key binding
  if (_keyTranslator)
    {
      KeyboardTranslator::Entry entry =
    _keyTranslator->findEntry (event->key (), modifiers, states);

      // send result to terminal
      QByteArray textToSend;

      // special handling for the Alt (aka. Meta) modifier.  pressing
      // Alt+[Character] results in Esc+[Character] being sent
      // (unless there is an entry defined for this particular combination
      //  in the keyboard modifier)
      bool wantsAltModifier =
    entry.modifiers () & entry.modifierMask () & Qt::AltModifier;
      bool wantsAnyModifier =
    entry.state () & entry.
    stateMask () & KeyboardTranslator::AnyModifierState;

      if (modifiers & Qt::AltModifier
      && !(wantsAltModifier || wantsAnyModifier)
      && !event->text ().isEmpty ())
    {
      textToSend.prepend ("\033");
    }

      if (entry.command () != KeyboardTranslator::NoCommand)
    {
      if (entry.command () & KeyboardTranslator::EraseCommand)
        textToSend += eraseChar ();

      // TODO command handling
    }
      else if (!entry.text ().isEmpty ())
    {
      textToSend += _codec->fromUnicode (entry.text (true, modifiers));
    }
      else
    textToSend += _codec->fromUnicode (event->text ());

      sendData (textToSend.constData (), textToSend.length ());
    }
  else
    {
      // print an error message to the terminal if no key translator has been
      // set
      QString translatorError = QString ("No keyboard translator available.  "
                     "The information needed to convert key presses "
                     "into characters to send to the terminal "
                     "is missing.");
      reset ();
      receiveData (translatorError.toAscii ().constData (),
           translatorError.count ());
    }*/
  keyEvent->accept ();
}

void OctaveTerminal::handleReceivedData (const QByteArray& data)
{
  int position;
  QTextCursor tc = textCursor ();
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
          emit bell ();
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
  setTextCursor (tc);

  if (verticalScrollBar ())
    {
      verticalScrollBar ()->setValue (verticalScrollBar ()->maximum ());
    }
}
