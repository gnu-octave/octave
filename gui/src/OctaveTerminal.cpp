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
  m_session = new Session ();
  m_session->setTitle (Session::NameRole, "QTerminalWidget");
  m_session->setProgram ("/bin/bash");
  m_session->setArguments (QStringList ());
  m_session->setAutoClose (true);
  m_session->setFlowControlEnabled (true);
  m_session->setDarkBackground (true);

  connect (m_session, SIGNAL(receivedData(QByteArray)), this, SLOT(handleReceivedData(QByteArray)));
  int fdm, fds;
  if (openpty (&fdm, &fds, 0, 0, 0) < 0)
    {
      assert (0);
    }
  dup2 (fds, 0);
  dup2 (fds, 1);
  dup2 (fds, 2);

  m_session->openTeletype (fdm);
}

void
OctaveTerminal::keyPressEvent (QKeyEvent * keyEvent)
{
  //QByteArray textToSend;

  //textToSend += QString::fromUtf8());
  m_session->sendText(keyEvent->text ());

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

      if (event->key () == Qt::Key_PageUp)
    {
      _screenWindow->scrollBy (ScreenWindow::ScrollPages, -1);
    }
      else if (event->key () == Qt::Key_PageDown)
    {
      _screenWindow->scrollBy (ScreenWindow::ScrollPages, 1);
    }
      else if (event->key () == Qt::Key_Up)
    {
      _screenWindow->scrollBy (ScreenWindow::ScrollLines, -1);
    }
      else if (event->key () == Qt::Key_Down)
    {
      _screenWindow->scrollBy (ScreenWindow::ScrollLines, 1);
    }
      else
    update = false;

    }

  _actSel = 0;          // Key stroke implies a screen update, so TerminalDisplay won't
  // know where the current selection is.

  if (_hasBlinkingCursor)
    {
      _blinkCursorTimer->start (QApplication::cursorFlashTime () / 2);
      if (_cursorBlinking)
    blinkCursorEvent ();
      else
    _cursorBlinking = false;
    }

  if (emitKeyPressSignal)
    emit keyPressedSignal (event);

  event->accept ();
  _scrollBar->setValue (_scrollBar->maximum ());
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
  QTextCursor tc = textCursor ();
  tc.movePosition (QTextCursor::End);
  tc.insertText (data);

  if (verticalScrollBar ())
    {
      verticalScrollBar ()->setValue (verticalScrollBar ()->maximum ());
    }
}
