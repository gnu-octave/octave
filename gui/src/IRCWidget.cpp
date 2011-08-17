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

#include "ResourceManager.h"
#include "IRCWidget.h"
#include <QMessageBox>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QLabel>
#include <QSettings>
#include <QInputDialog>
#include <QKeyEvent>
#include <QScrollBar>
#include <QApplication>
#include "IRCClientImpl.h"

ChatMessageTextEdit::ChatMessageTextEdit (QWidget *parent)
  : QPlainTextEdit (parent), m_completer (0)
{
  setMaximumHeight (50);
  setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Minimum);
}

ChatMessageTextEdit::~ChatMessageTextEdit ()
{
}

void
ChatMessageTextEdit::setCompleter (QCompleter *completer)
{
  if (m_completer)
    QObject::disconnect (m_completer, 0, this, 0);

  m_completer = completer;

  if (!m_completer)
    return;

  m_completer->setWidget (this);
  m_completer->setCompletionMode (QCompleter::InlineCompletion);
  m_completer->setCaseSensitivity (Qt::CaseInsensitive);
  QObject::connect (m_completer, SIGNAL (activated (QString)),
                    this, SLOT (insertCompletion (QString)));
}

QCompleter *
ChatMessageTextEdit::completer () const
{
  return m_completer;
}

void
ChatMessageTextEdit::insertCompletion(const QString& completion)
{

  if (m_completer->widget() != this)
    return;
  QTextCursor tc = textCursor();
  int extra = completion.length() - m_completer->completionPrefix().length();
  tc.movePosition(QTextCursor::Left);
  tc.movePosition(QTextCursor::EndOfWord);
  tc.insertText(completion.right(extra));
  setTextCursor(tc);
}

QString
ChatMessageTextEdit::textUnderCursor () const
{
  QTextCursor tc = textCursor ();
  tc.select (QTextCursor::WordUnderCursor);
  return tc.selectedText ();
}

void
ChatMessageTextEdit::focusInEvent (QFocusEvent *e)
{
  if (m_completer)
    m_completer->setWidget (this);
  QPlainTextEdit::focusInEvent (e);
}

void
ChatMessageTextEdit::keyPressEvent (QKeyEvent *keyPressEvent)
{
  if (m_completer) {
    switch (keyPressEvent->key ()) {
    case Qt::Key_Enter:
    case Qt::Key_Return:
      if (! (keyPressEvent->modifiers () & Qt::ShiftModifier))
        {
          emit sendMessage (document ()->toPlainText ());
          document ()->setPlainText ("");
        }
      else
        {
          QPlainTextEdit::keyPressEvent (keyPressEvent);
        }
      break;
    case Qt::Key_Escape:
    case Qt::Key_Tab:
    case Qt::Key_Backtab:
      keyPressEvent->ignore ();
      return;
    default:
      QPlainTextEdit::keyPressEvent(keyPressEvent);
      break;
      }

    QString completionPrefix = textUnderCursor ();
    if (completionPrefix != m_completer->completionPrefix ())
      m_completer->setCompletionPrefix(completionPrefix);
    m_completer->complete ();
   }
}

IRCWidget::IRCWidget (QWidget * parent):
QWidget (parent)
{
  QSettings *settings = ResourceManager::instance ()->settings ();
  bool connectOnStartup = settings->value ("connectOnStartup").toBool ();
  m_autoIdentification = settings->value ("autoIdentification").toBool ();
  m_nickServPassword = settings->value ("nickServPassword").toString ();

  m_initialNick = settings->value ("IRCNick").toString ();

  if (m_initialNick.isEmpty ())
    m_initialNick = "OctaveGUI-User";

  QVBoxLayout *layout = new QVBoxLayout ();

  m_chatWindow = new QTextEdit (this);
  m_chatWindow->setReadOnly (true);
  m_chatWindow->setEnabled (false);
  QWidget *bottomWidget = new QWidget (this);

  layout->addWidget (m_chatWindow);
  layout->addWidget (bottomWidget);
  setLayout (layout);

  QHBoxLayout *bottomLayout = new QHBoxLayout ();
  m_nickButton = new QPushButton (bottomWidget);
  m_nickButton->setStatusTip (tr ((char *) "Click here to change your nick."));
  m_nickButton->setText (m_initialNick);
  m_chatMessageTextEdit = new ChatMessageTextEdit (bottomWidget);
  m_chatMessageTextEdit->setStatusTip (tr ((char *) "Enter your message here."));

  bottomLayout->addWidget (m_nickButton);
  bottomLayout->addWidget (new QLabel (":", this));
  bottomLayout->addWidget (m_chatMessageTextEdit);
  bottomLayout->setMargin (0);
  bottomWidget->setLayout (bottomLayout);

  m_nickButton->setEnabled (false);
  m_chatMessageTextEdit->setEnabled (false);

  //setFocusProxy (m_chatMessageTextEdit);
  m_nickButton->setFocusProxy (m_chatMessageTextEdit);

  QFont font;
  font.setFamily ("Courier");
  font.setPointSize (11);
  m_chatWindow->setFont (font);
  m_ircClientInterface = new IRCClientImpl (this);
  m_octaveChannel = m_ircClientInterface->ircChannelProxy ("#octave");

  connect (m_ircClientInterface, SIGNAL (connected (QString)),
           this, SLOT (handleConnected (QString)));
  connect (m_ircClientInterface, SIGNAL(loggedIn(QString)),
           this, SLOT (joinOctaveChannel (QString)));
  connect (m_ircClientInterface, SIGNAL (error (QString)),
           this, SLOT (showErrorMessage (QString)));
  connect (m_ircClientInterface, SIGNAL (debugMessage (QString)),
           this, SLOT (showStatusMessage (QString)));
  connect (m_ircClientInterface, SIGNAL (message (QString, QString, QString)),
           this, SLOT (showMessage (QString, QString, QString )));
  connect (m_ircClientInterface, SIGNAL (nicknameChanged (QString,QString)),
           this, SLOT (handleNickChange (QString,QString)));
  connect (m_ircClientInterface, SIGNAL (notification (QString,QString)),
           this, SLOT (showNotification (QString,QString)));
  connect (m_ircClientInterface, SIGNAL (loggedIn (QString)),
           this, SLOT (handleLoggedIn(QString)));
  connect (m_ircClientInterface, SIGNAL (userNicknameChanged (QString)),
           this, SLOT (handleUserNicknameChanged (QString)));

  connect (m_nickButton, SIGNAL (clicked ()), this, SLOT (showChangeUserNickPopup ()));
  connect (m_chatMessageTextEdit, SIGNAL (sendMessage (QString)),
           this, SLOT (sendMessage (QString)));

  m_chatMessageTextEdit->setCompleter
      (new QCompleter (m_ircClientInterface->ircChannelProxy ("#octave")->userListModel (), this));

  if (connectOnStartup)
    connectToServer ();
}

void
IRCWidget::connectToServer ()
{
  showStatusMessage ("Looking up irc.freenode.net.");
  QHostInfo hostInfo = QHostInfo::fromName ("irc.freenode.net");
  QList<QHostAddress> hostAddresses = hostInfo.addresses();
  if (hostAddresses.isEmpty ())
    {
      showStatusMessage ("Failed to lookup irc.freenode.net.");
    }
  else
    {
      showStatusMessage (QString ("Attempting to connect to %1.")
                         .arg (hostAddresses.at (0).toString ()));
      m_ircClientInterface->connectToHost(hostAddresses.at (0), 6667, m_initialNick);
    }
}

void
IRCWidget::showStatusMessage (const QString& message)
{
  m_chatWindow->append (QString ("<i>%1</i>").arg (message));
}

void
IRCWidget::showErrorMessage (const QString& message)
{
  m_chatWindow->append (QString ("<i>Error: %1</i>").arg (message));
}

void
IRCWidget::handleConnected (const QString &host)
{
  showStatusMessage (QString ("Connected to server %1.").arg (host));
}

void
IRCWidget::joinOctaveChannel (const QString& nick)
{
  Q_UNUSED (nick);
  showStatusMessage (QString ("Joining channel #octave."));
  m_octaveChannel->sendJoinRequest ();
}

void
IRCWidget::showMessage (const QString& channel, const QString& sender, const QString& message)
{
  Q_UNUSED (channel);
  if (!hasFocus()
      && !m_chatMessageTextEdit->hasFocus ()
      && !m_nickButton->hasFocus ()
      && !m_chatWindow->hasFocus ())
    {
      emit unreadMessages (true);
    }

  QString output;
  QString htmlMessage = message;
  htmlMessage.replace ("<", "&lt;");
  htmlMessage.replace (">", "&gt;");
  htmlMessage.replace ("\n", "<br>");
  if (message.contains (m_ircClientInterface->nickname ()))
    {
      output =
        QString ("<font color=\"#990000\"><b>%1:</b> %2</font>").arg (sender).
        arg (htmlMessage);

      QApplication::alert (this);
    }
  else
    {
      output =
        QString ("<b>%1:</b> %2").arg (sender).
        arg (htmlMessage);
    }
  m_chatWindow->append (output);
}

void
IRCWidget::showNotification (const QString& sender, const QString& message)
{
  Q_UNUSED (sender);
  m_chatWindow->append (QString ("<font color=\"#007700\">%1</font>").arg (message));
}

void
IRCWidget::showChangeUserNickPopup ()
{
  bool ok;
  QString newNick =
    QInputDialog::getText (this, QString ("Nickname"),
			   QString ("Type in your nickname:"),
                           QLineEdit::Normal, m_ircClientInterface->nickname (), &ok);
  if (ok)
    {
      m_ircClientInterface->sendNicknameChangeRequest (newNick);
    }
}

void
IRCWidget::sendMessage (QString message)
{
  // Do not send empty messages.
  if (message.isEmpty ())
    return;

  // Remove trailing spaces.
  while (message.at (0).isSpace ())
    message.remove (0, 1);
  if (message.startsWith ("/"))
    {
      QStringList line =
	message.split (QRegExp ("\\s+"), QString::SkipEmptyParts);
      if (line.at (0) == "/join")
	{
          IRCChannelProxyInterface *ircChannel = m_ircClientInterface->ircChannelProxy (line.at (1));
          ircChannel->sendJoinRequest ();
	}
      else if (line.at (0) == "/nick")
	{
          m_ircClientInterface->sendNicknameChangeRequest (line.at (1));
	}
      else if (line.at (0) == "/msg")
	{
	  QString recipient = line.at (1);
	  // Since we splitted the message before, we have to glue it together again.
	  QString pmsg = "";
	  for (int i = 2; i < line.length (); i++)
	    {
	      pmsg += line.at (i);
	      pmsg += " ";
	    }
          m_ircClientInterface->sendPrivateMessage(recipient, pmsg);
	}
    }
  else
    {
      m_octaveChannel->sendMessage (message);
      message.replace ("<", "&lt;");
      message.replace (">", "&gt;");
      message.replace ("\n", "<br>");
      m_chatWindow->append (QString ("<b>%1:</b> %2").
                            arg (m_ircClientInterface->nickname ()).arg (message));
    }
}

void
IRCWidget::focusInEvent (QFocusEvent *focusEvent)
{
  Q_UNUSED (focusEvent);
  emit unreadMessages (false);
  QWidget::focusInEvent (focusEvent);

  m_chatMessageTextEdit->setFocus ();
}

void
IRCWidget::handleLoggedIn (const QString &nick)
{
  m_chatWindow->
    append (QString
            ("<i><font color=\"#00AA00\"><b>Successfully logged in as %1.</b></font></i>").
            arg (nick));
  m_nickButton->setEnabled (true);
  m_chatMessageTextEdit->setEnabled (true);
  m_chatWindow->setEnabled (true);
  m_chatMessageTextEdit->setFocus ();


  if (m_autoIdentification)
    {
      m_ircClientInterface->sendPrivateMessage("NickServ", QString ("identify %1").
                                          arg (m_nickServPassword));
    }
}

void
IRCWidget::handleNickChange (const QString &oldNick, const QString &newNick)
{
  m_chatWindow->append (QString ("%1 is now known as %2.").arg (oldNick).arg (newNick));
}

void
IRCWidget::handleUserJoined (const QString &nick, const QString &channel)
{
  m_chatWindow->append (QString ("<i>%1 has joined %2.</i>").arg (nick).arg (channel));
}

void
IRCWidget::handleUserQuit (const QString &nick, const QString &reason)
{
  m_chatWindow->append (QString ("<i>%1 has quit.(%2).</i>").arg (nick).arg (reason));
}

void
IRCWidget::handleUserNicknameChanged (const QString &nick)
{
  m_nickButton->setText (nick);
  QSettings *settings = ResourceManager::instance ()->settings ();
  settings->setValue ("IRCNick", nick);
}
