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

#include "IRCWidget.h"
#include <QMessageBox>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QLabel>
#include <QSettings>
#include <QInputDialog>

IRCWidget::IRCWidget (QWidget * parent, QString settingsFile):
QWidget (parent)
{
  m_settingsFile = settingsFile;
  m_alternatingColor = false;
  QSettings settings (m_settingsFile, QSettings::IniFormat);
  bool connectOnStartup = settings.value ("connectOnStartup").toBool ();
  m_autoIdentification = settings.value ("autoIdentification").toBool ();
  m_nickServPassword = settings.value ("nickServPassword").toString ();

  m_initialNick = settings.value ("IRCNick").toString ();

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
  m_nickButton->
    setStatusTip (tr ((char *) "Click here to change your nick."));
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
  m_ircClient = new IRCClient ();

  connect (m_nickButton, SIGNAL (clicked ()), this, SLOT (nickPopup ()));
  connect (m_inputLine, SIGNAL (returnPressed ()), this,
	   SLOT (sendInputLine ()));

  connect (m_ircClient, SIGNAL (nickInUseChanged ()), this,
	   SLOT (handleNickInUseChanged ()));
  connect (m_ircClient, SIGNAL (connectionStatus (const char *)), this,
	   SLOT (showStatusMessage (const char *)));
  connect (m_ircClient, SIGNAL (error (const char *)), this,
	   SLOT (showStatusMessage (const char *)));
  connect (m_ircClient, SIGNAL (completedLogin (const char *)), this,
	   SLOT (loginSuccessful (const char *)));
  connect (m_ircClient, SIGNAL (completedLogin (const char *)), this,
	   SLOT (joinOctaveChannel (const char *)));
  connect (m_ircClient,
	   SIGNAL (topic (const char *, const char *, const char *)), this,
	   SLOT (showTopic (const char *, const char *, const char *)));
  connect (m_ircClient, SIGNAL (join (const char *, const char *)), this,
	   SLOT (showJoin (const char *, const char *)));
  connect (m_ircClient, SIGNAL (quit (const char *, const char *)), this,
	   SLOT (showQuit (const char *, const char *)));
  connect (m_ircClient,
	   SIGNAL (privateMessage (const char *, const char *, const char *)),
	   this,
	   SLOT (showPrivateMessage
		 (const char *, const char *, const char *)));
  connect (m_ircClient,
	   SIGNAL (notice (const char *, const char *, const char *)), this,
	   SLOT (showNotice (const char *, const char *, const char *)));
  connect (m_ircClient, SIGNAL (nick (const char *, const char *)), this,
	   SLOT (showNickChange (const char *, const char *)));
  connect (m_ircClient, SIGNAL (replyCode (IRCEvent *)), this,
	   SLOT (handleReplyCode (IRCEvent *)));

  if (connectOnStartup)
    connectToServer ();
}

void
IRCWidget::connectToServer ()
{
  m_ircClient->connectToServer ("irc.freenode.net", 6667,
				m_initialNick.toStdString ().c_str (),
				m_initialNick.toStdString ().c_str (),
				"Unknown", "Unknown", 0, 0);
}

void
IRCWidget::showStatusMessage (const char *message)
{
  m_chatWindow->append (QString ("<i>%1</i>").arg (message));
}

void
IRCWidget::joinOctaveChannel (const char *)
{
  m_ircClient->joinChannel ("#octave");
}

void
IRCWidget::loginSuccessful (const char *nick)
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
    m_ircClient->sendCommand (2, COMMAND_PRIVMSG,
			      "NickServ",
			      QString ("identify %1").
			      arg (m_nickServPassword).toStdString ().
			      c_str ());
}

void
IRCWidget::showPrivateMessage (const char *nick, const char *destination,
			       const char *message)
{
  Q_UNUSED (destination);
  QString msg (message);
  msg.replace ("<", "&lt;");
  msg.replace (">", "&gt;");
  if (msg.contains (m_ircClient->nickInUse ()))
    {
      msg =
	QString ("<font color=\"#990000\"><b>%1:</b> %2</font>").arg (nick).
	arg (msg);
    }
  else
    {
      msg =
	QString ("<font color=\"%3\"><b>%1:</b> %2</font>").arg (nick).
	arg (msg).arg (getAlternatingColor ());
    }
  m_chatWindow->append (msg);
}

void
IRCWidget::showNotice (const char *nick, const char *destination,
		       const char *message)
{
  Q_UNUSED (nick);
  Q_UNUSED (destination);
  m_chatWindow->append (QString ("<font color=\"#007700\">%1</font>").
			arg (message));
}

void
IRCWidget::showTopic (const char *nick, const char *channel,
		      const char *message)
{
  QString msg (message);
  msg.replace ("<", "&lt;");
  msg.replace (">", "&gt;");
  m_chatWindow->append (QString ("Topic for %2 was set by %1: %3").arg (nick).
                        arg (channel).arg (msg));
}

void
IRCWidget::showNickChange (const char *oldNick, const char *newNick)
{
  m_chatWindow->append (QString ("%1 is now known as %2.").arg (oldNick).
			arg (newNick));
  m_nickList.removeAll (QString (oldNick));
  m_nickList.append (QString (newNick));
  updateNickCompleter ();
}

void
IRCWidget::nickPopup ()
{
  bool ok;
  QString newNick =
    QInputDialog::getText (this, QString ("Nickname"),
			   QString ("Type in your nickname:"),
			   QLineEdit::Normal, m_ircClient->nickInUse (), &ok);
  if (ok)
    {
      m_ircClient->sendNickChange (newNick);
    }
}

void
IRCWidget::showJoin (const char *nick, const char *channel)
{
  m_chatWindow->append (QString ("<i>%1 has joined %2.</i>").arg (nick).
			arg (channel));
  m_nickList.append (QString (nick));
  updateNickCompleter ();
}

void
IRCWidget::showQuit (const char *nick, const char *reason)
{
  m_chatWindow->append (QString ("<i>%1 has quit.(%2).</i>").arg (nick).
			arg (reason));
  m_nickList.removeAll (QString (nick));
  updateNickCompleter ();
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
	  m_ircClient->joinChannel (line.at (1));
	}
      else if (line.at (0) == "/nick")
	{
	  m_ircClient->sendNickChange (line.at (1));
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
	  m_ircClient->sendCommand (2, COMMAND_PRIVMSG,
				    recipient.toStdString ().c_str (),
				    pmsg.toStdString ().c_str ());
	}
    }
  else
    {
      m_ircClient->sendPublicMessage (message);
      message.replace ("<", "&lt;");
      message.replace (">", "&gt;");
      m_chatWindow->append (QString ("<b>%1:</b> %2").
                            arg (m_ircClient->nickInUse ()).arg (message));
    }
}

void
IRCWidget::sendInputLine ()
{
  sendMessage (m_inputLine->text ());
  m_inputLine->setText ("");
}

void
IRCWidget::handleNickInUseChanged ()
{
  m_nickButton->setText (m_ircClient->nickInUse ());
  QSettings settings (m_settingsFile, QSettings::IniFormat);
  settings.setValue ("IRCNick", m_ircClient->nickInUse ());
}

void
IRCWidget::handleReplyCode (IRCEvent * event)
{
  QSettings settings (m_settingsFile, QSettings::IniFormat);

  switch (event->getNumeric ())
    {
    case RPL_MOTDSTART:
    case RPL_MOTD:
    case ERR_NOMOTD:
    case RPL_ENDOFMOTD:
      if (settings.value ("showMessageOfTheDay").toBool ())
	m_chatWindow->append (QString ("<font color=\"#777777\">%1</font>").
			      arg (event->getParam (1)));
      break;
    case RPL_NOTOPIC:
    case RPL_TOPIC:
      if (settings.value ("showTopic").toBool ())
	m_chatWindow->
	  append (QString ("<font color=\"#000088\"><b>%1</b></font>").
		  arg (event->getParam (2)));
      break;
    case RPL_NAMREPLY:
      m_chatWindow->
	append (QString ("<font color=\"#000088\">Users online: %1</font>").
		arg (event->getParam (3)));
      m_nickList =
	event->getParam (3).split (QRegExp ("\\s+"), QString::SkipEmptyParts);
      updateNickCompleter ();
      break;
    case ERR_NICKNAMEINUSE:
    case ERR_NICKCOLLISION:
      m_chatWindow->
	append (QString ("<font color=\"#AA0000\">Nickname in use.</font>"));
      break;
    };
}


void
IRCWidget::updateNickCompleter ()
{
  QCompleter *completer = new QCompleter (m_nickList, this);
  completer->setCompletionMode (QCompleter::InlineCompletion);
  m_inputLine->setCompleter (completer);
}
