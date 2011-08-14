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
#include "IRCClientImpl.h"

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
  m_inputLine = new QLineEdit (bottomWidget);
  m_inputLine->setStatusTip (tr ((char *) "Enter your message here."));

  bottomLayout->addWidget (m_nickButton);
  bottomLayout->addWidget (new QLabel (":", this));
  bottomLayout->addWidget (m_inputLine);
  bottomLayout->setMargin (0);
  bottomWidget->setLayout (bottomLayout);

  m_nickButton->setEnabled (false);
  m_inputLine->setEnabled (false);

  //m_chatWindow->setFocusProxy (m_inputLine);
  this->setFocusProxy (m_inputLine);
  m_nickButton->setFocusProxy (m_inputLine);

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
  connect (m_ircClientInterface, SIGNAL (loggedIn(QString)),
           this, SLOT (handleLoggedIn(QString)));
  connect (m_nickButton, SIGNAL (clicked ()), this, SLOT (nickPopup ()));
  connect (m_inputLine, SIGNAL (returnPressed ()), this,
	   SLOT (sendInputLine ()));

  if (connectOnStartup)
    connectToServer ();
}

void
IRCWidget::connectToServer ()
{
  showStatusMessage ("<font color=\"#990000\"><b>IMPORTANT: THE BACKEND FOR THE IRC CHAT HAS BEEN REWRITTEN COMPLETELY TO MAKE IT PLATFORM-INDEPENDENT.</b></font>");
  showStatusMessage ("<font color=\"#990000\"><b>IT WILL PROBABLY NOT WORK AS IT SHOULD UNTIL ALL BUGS HAVE BEEN FIXED.</b></font>");
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
  QString output;
  if (message.contains (m_ircClientInterface->nickname ()))
    {
      output =
        QString ("<font color=\"#990000\"><b>%1:</b> %2</font>").arg (sender).
        arg (message);
    }
  else
    {
      output =
        QString ("<b>%1:</b> %2").arg (sender).
        arg (message);
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
IRCWidget::nickPopup ()
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
      m_chatWindow->append (QString ("<b>%1:</b> %2").
                            arg (m_ircClientInterface->nickname ()).arg (message));
    }
}

void
IRCWidget::sendInputLine ()
{
  sendMessage (m_inputLine->text ());
  m_inputLine->setText ("");
}

void
IRCWidget::handleLoggedIn (const QString &nick)
{
  m_chatWindow->
    append (QString
            ("<i><font color=\"#00AA00\"><b>Successfully logged in as %1.</b></font></i>").
            arg (nick));
  m_nickButton->setEnabled (true);
  m_inputLine->setEnabled (true);
  m_chatWindow->setEnabled (true);
  m_inputLine->setFocus ();


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
  m_nickList.removeAll (QString (oldNick));
  m_nickList.append (QString (newNick));
  updateNickCompleter ();

  //m_nickButton->setText (m_ircClient->nickInUse ());
  //QSettings *settings = ResourceManager::instance ()->settings ();
  //settings->setValue ("IRCNick", m_ircClient->nickInUse ());
}

void
IRCWidget::handleUserJoined (const QString &nick, const QString &channel)
{
  m_chatWindow->append (QString ("<i>%1 has joined %2.</i>").arg (nick).
                        arg (channel));
  m_nickList.append (QString (nick));
  updateNickCompleter ();
}

void
IRCWidget::handleUserQuit (const QString &nick, const QString &reason)
{
  m_chatWindow->append (QString ("<i>%1 has quit.(%2).</i>").arg (nick).
                        arg (reason));
  m_nickList.removeAll (QString (nick));
  updateNickCompleter ();
}

void
IRCWidget::updateNickCompleter ()
{
  QCompleter *completer = new QCompleter (m_nickList, this);
  completer->setCompletionMode (QCompleter::InlineCompletion);
  m_inputLine->setCompleter (completer);
}
