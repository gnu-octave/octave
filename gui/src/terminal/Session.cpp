/*
    This file is part of Konsole

    Copyright 2006-2008 by Robert Knight <robertknight@gmail.com>
    Copyright 1997,1998 by Lars Doelle <lars.doelle@on-line.de>
    Copyright 2009 by Thomas Dreibholz <dreibh@iem.uni-due.de>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
    02110-1301  USA.
*/

// Own
#include "Session.h"

// Standard
#include <assert.h>
#include <stdlib.h>
#include <signal.h>

// Qt
#include <QtGui/QApplication>
#include <QtCore/QByteRef>
#include <QtCore/QDir>
#include <QtCore/QFile>
#include <QtCore/QRegExp>
#include <QtCore/QStringList>
#include <QtCore/QDate>
#include <QTimer>
#include <QTextStream>

#include "kprocess.h"
#include "kptydevice.h"

#include "Pty.h"
#include "ShellCommand.h"

int
  Session::lastSessionId = 0;

// HACK This is copied out of QUuid::createUuid with reseeding forced.
// Required because color schemes repeatedly seed the RNG...
// ...with a constant.
QUuid
createUuid ()
{
  static const int
    intbits = sizeof (int) * 8;
  static int
    randbits = 0;
  if (!randbits)
    {
      int
	max = RAND_MAX;
      do
	{
	  ++randbits;
	}
      while ((max = max >> 1));
    }

  qsrand (uint (QDateTime::currentDateTime ().toTime_t ()));
  qrand ();			// Skip first

  QUuid
    result;
  uint *
    data = &(result.data1);
  int
    chunks = 16 / sizeof (uint);
  while (chunks--)
    {
      uint
	randNumber = 0;
      for (int filled = 0; filled < intbits; filled += randbits)
	randNumber |= qrand () << filled;
      *(data + chunks) = randNumber;
    }

  result.data4[0] = (result.data4[0] & 0x3F) | 0x80;	// UV_DCE
  result.data3 = (result.data3 & 0x0FFF) | 0x4000;	// UV_Random

  return result;
}

Session::Session (QObject * parent):
QObject (parent), _shellProcess (0),/* _emulation (0),*/ _monitorActivity (false),
_monitorSilence (false), _notifiedActivity (false), _autoClose (true),
_wantedClose (false), _silenceSeconds (10),
_flowControl (true), _fullScripting (false), _sessionId (0),
_hasDarkBackground (false)
{
  _uniqueIdentifier = createUuid ();

  //prepare DBus communication
  //new SessionAdaptor(this);
  _sessionId = ++lastSessionId;

  // JPS: commented out for lack of DBUS support by default on OSX
  //QDBusConnection::sessionBus().registerObject(QLatin1String("/Sessions/")+QString::number(_sessionId), this);

  //create emulation backend
  //_emulation = new Vt102Emulation ();
  /*
  connect (_emulation, SIGNAL (titleChanged (int, const QString &)),
	   this, SLOT (setUserTitle (int, const QString &)));
  connect (_emulation, SIGNAL (stateSet (int)),
	   this, SLOT (activityStateSet (int)));
  connect (_emulation, SIGNAL (changeTabTextColorRequest (int)),
	   this, SIGNAL (changeTabTextColorRequest (int)));
  connect (_emulation,
	   SIGNAL (profileChangeCommandReceived (const QString &)), this,
	   SIGNAL (profileChangeCommandReceived (const QString &)));
  connect (_emulation, SIGNAL (flowControlKeyPressed (bool)), this,
	   SLOT (updateFlowControlState (bool)));
*/
  //create new teletype for I/O with shell process
  openTeletype (-1);

  //setup timer for monitoring session activity
  _monitorTimer = new QTimer (this);
  _monitorTimer->setSingleShot (true);
  connect (_monitorTimer, SIGNAL (timeout ()), this,
	   SLOT (monitorTimerDone ()));
}

void
Session::openTeletype (int fd)
{
  delete _shellProcess;

  if (fd < 0)
    _shellProcess = new Pty ();
  else
    _shellProcess = new Pty (fd);

  connect (_shellProcess, SIGNAL (receivedData (const char *, int)), this,
           SLOT (onReceiveBlock (const char *, int)));
  connect (_shellProcess, SIGNAL (finished (int, QProcess::ExitStatus)), this,
           SLOT (done (int)));
}

void
Session::setDarkBackground (bool darkBackground)
{
  _hasDarkBackground = darkBackground;
}

bool
Session::hasDarkBackground () const
{
  return _hasDarkBackground;
}

bool
Session::isRunning () const
{
  return _shellProcess->state () == QProcess::Running;
}

void
Session::setProgram (const QString & program)
{
  _program = ShellCommand::expand (program);
}

void
Session::setInitialWorkingDirectory (const QString & dir)
{
  //_initialWorkingDir = KShell::tildeExpand(ShellCommand::expand(dir));
  _initialWorkingDir = ShellCommand::expand (dir);
}

void
Session::setArguments (const QStringList & arguments)
{
  _arguments = ShellCommand::expand (arguments);
}

QString
Session::checkProgram (const QString & program) const
{
  // Upon a KPty error, there is no description on what that error was...
  // Check to see if the given program is executable.
  QString exec = QFile::encodeName (program);

  if (exec.isEmpty ())
    return QString ();

  // if 'exec' is not specified, fall back to default shell.  if that 
  // is not set then fall back to /bin/sh
  if (exec.isEmpty ())
    exec = qgetenv ("SHELL");
  if (exec.isEmpty ())
    exec = "/bin/sh";
  return program;
}

void
Session::run ()
{
  if (_uniqueIdentifier.isNull ())
    {
      _uniqueIdentifier = createUuid ();
    }

  const int CHOICE_COUNT = 3;
  QString programs[CHOICE_COUNT] = { _program, qgetenv ("SHELL"), "/bin/sh" };
  QString exec;
  int choice = 0;
  while (choice < CHOICE_COUNT)
    {
      exec = checkProgram (programs[choice]);
      if (exec.isEmpty ())
	choice++;
      else
	break;
    }

  // if no arguments are specified, fall back to program name
  QStringList arguments = _arguments.join (QChar (' ')).isEmpty ()?
    QStringList () << exec : _arguments;

  if (!_initialWorkingDir.isEmpty ())
    _shellProcess->setWorkingDirectory (_initialWorkingDir);
  else
    _shellProcess->setWorkingDirectory (QDir::homePath ());

  _shellProcess->setFlowControlEnabled (_flowControl);
  //_shellProcess->setErase (_emulation->eraseChar ());

  // this is not strictly accurate use of the COLORFGBG variable.  This does not
  // tell the terminal exactly which colors are being used, but instead approximates
  // the color scheme as "black on white" or "white on black" depending on whether
  // the background color is deemed dark or not
  QString backgroundColorHint =
    _hasDarkBackground ? "COLORFGBG=15;0" : "COLORFGBG=0;15";
  _environment << backgroundColorHint;

  _shellProcess->start (exec, arguments);
  _shellProcess->setWriteable (false);
  emit started ();
}

void
Session::setUserTitle (int what, const QString & caption)
{
  //set to true if anything is actually changed (eg. old _nameTitle != new _nameTitle )
  bool modified = false;

  if ((what == IconNameAndWindowTitle) || (what == WindowTitle))
    {
      if (_userTitle != caption)
	{
	  _userTitle = caption;
	  modified = true;
	}
    }

  if ((what == IconNameAndWindowTitle) || (what == IconName))
    {
      if (_iconText != caption)
	{
	  _iconText = caption;
	  modified = true;
	}
    }

  if (what == TextColor || what == BackgroundColor)
    {
      QString colorString = caption.section (';', 0, 0);
      QColor color = QColor (colorString);
      if (color.isValid ())
	{
	  if (what == TextColor)
	    emit changeForegroundColorRequest (color);
	  else
	  emit changeBackgroundColorRequest (color);
	}
    }

  if (what == SessionName)
    {
      if (_nameTitle != caption)
	{
	  setTitle (Session::NameRole, caption);
	  return;
	}
    }

  if (what == 31)
    {
      QString cwd = caption;
      cwd = cwd.replace (QRegExp ("^~"), QDir::homePath ());
      //emit openUrlRequest (cwd);
    }

  // change icon via \033]32;Icon\007
  if (what == 32)
    {
      if (_iconName != caption)
	{
	  _iconName = caption;

	  modified = true;
	}
    }

  if (what == ProfileChange)
    {
      emit profileChangeCommandReceived (caption);
      return;
    }

  if (modified)
    emit titleChanged ();
}

QString
Session::userTitle () const
{
  return _userTitle;
}

void
Session::monitorTimerDone ()
{
  //FIXME: The idea here is that the notification popup will appear to tell the user than output from
  //the terminal has stopped and the popup will disappear when the user activates the session.
  //
  //This breaks with the addition of multiple views of a session.  The popup should disappear
  //when any of the views of the session becomes active


  //FIXME: Make message text for this notification and the activity notification more descriptive.    
  if (_monitorSilence)
    {
      //KNotification::event("Silence", i18n("Silence in session '%1'", _nameTitle)propagateSize, QPixmap(),
      //                QApplication::activeWindow(),
      //                KNotification::CloseWhenWidgetActivated);
      //emit stateChanged (NOTIFYSILENCE);
    }
  else
    {
      //emit stateChanged (NOTIFYNORMAL);
    }

  _notifiedActivity = false;
}

void
Session::updateFlowControlState (bool suspended)
{
  /*
  if (suspended)
    {
      if (flowControlEnabled ())
	{
	  foreach (TerminalDisplay * display, _views)
	  {
	    if (display->flowControlWarningEnabled ())
	      display->outputSuspended (true);
	  }
	}
    }
  else
    {
      foreach (TerminalDisplay * display, _views)
	display->outputSuspended (false);
    }*/
}

void
Session::activityStateSet (int state)
{/*
  if (state == NOTIFYBELL)
    {
      // empty
    }
  else if (state == NOTIFYACTIVITY)
    {
      if (_monitorSilence)
	{
	  _monitorTimer->start (_silenceSeconds * 1000);
	}

      if (_monitorActivity)
	{
	  //FIXME:  See comments in Session::monitorTimerDone()
	  if (!_notifiedActivity)
	    {
	      //KNotification::event("Activity", i18n("Activity in session '%1'", _nameTitle), QPixmap(),
	      //                QApplication::activeWindow(),
	      //KNotification::CloseWhenWidgetActivated);
	      _notifiedActivity = true;
	    }
	}
    }

  if (state == NOTIFYACTIVITY && !_monitorActivity)
    state = NOTIFYNORMAL;
  if (state == NOTIFYSILENCE && !_monitorSilence)
    state = NOTIFYNORMAL;

  emit stateChanged (state);
  */
}

void
Session::refresh ()
{
  const QSize existingSize = _shellProcess->windowSize ();
  _shellProcess->setWindowSize (existingSize.height (),
				existingSize.width () + 1);
  _shellProcess->setWindowSize (existingSize.height (),
				existingSize.width ());
}

bool
Session::kill (int signal)
{
  int result =::kill (_shellProcess->pid (), signal);

  if (result == 0)
    {
      _shellProcess->waitForFinished ();
      return true;
    }
  else
    return false;
}

void
Session::close ()
{
  _autoClose = true;
  _wantedClose = true;

  if (!isRunning () || !kill (SIGHUP))
    {
      if (isRunning ())
	{
	  _shellProcess->pty ()->close ();
	  if (_shellProcess->waitForFinished (3000))
            return;
        }
      QTimer::singleShot (1, this, SIGNAL (finished ()));
    }
}

void
Session::sendText (const QString & text) const
{
  _shellProcess->sendData (text.toStdString().c_str(), text.length ());
}

void
Session::sendMouseEvent (int buttons, int column, int line, int eventType)
{
  //_emulation->sendMouseEvent (buttons, column, line, eventType);
}

Session::~Session ()
{
  delete _shellProcess;
}

void
Session::done (int exitStatus)
{
  if (!_autoClose)
    {
      _userTitle = QString ("@info:shell This session is done");
      emit titleChanged ();
      return;
    }

  QString message;
  QTextStream msgStream (&message);
  if (!_wantedClose || exitStatus != 0)
    {
      if (_shellProcess->exitStatus () == QProcess::NormalExit)
	{
          msgStream << "Program '" << _program << "' exited with status " <<
	    exitStatus << ".";
	}
      else
	{
	  msgStream << "Program '" << _program << "' crashed.";
	}
      }
  emit finished ();
}

QStringList
Session::environment () const
{
  return _environment;
}

void
Session::setEnvironment (const QStringList & environment)
{
  _environment = environment;
}

void
Session::setTitle (TitleRole role, const QString & newTitle)
{
  if (title (role) != newTitle)
    {
      if (role == NameRole)
	_nameTitle = newTitle;
      else if (role == DisplayedTitleRole)
	_displayTitle = newTitle;

      emit titleChanged ();
    }
}

QString
Session::title (TitleRole role) const
{
  if (role == NameRole)
    return _nameTitle;
  else if (role == DisplayedTitleRole)
    return _displayTitle;
  else
    return QString ();
}

void
Session::setIconName (const QString & iconName)
{
  if (iconName != _iconName)
    {
      _iconName = iconName;
      emit titleChanged ();
    }
}

void
Session::setIconText (const QString & iconText)
{
  _iconText = iconText;
}

QString
Session::iconName () const
{
  return _iconName;
}

QString
Session::iconText () const
{
  return _iconText;
}

QStringList
Session::arguments () const
{
  return _arguments;
}

QString
Session::program () const
{
  return _program;
}

void
Session::setFlowControlEnabled (bool enabled)
{
  _flowControl = enabled;

  if (_shellProcess)
    _shellProcess->setFlowControlEnabled (_flowControl);
  emit flowControlEnabledChanged (enabled);
}

bool
Session::flowControlEnabled () const
{
  if (_shellProcess)
    return _shellProcess->flowControlEnabled ();
  else
    return _flowControl;
}

void
Session::onReceiveBlock (const char *buf, int len)
{
  emit receivedData (QByteArray (buf, len));
}

void
Session::setTitle (int role, const QString & title)
{
  switch (role)
    {
    case (0):
      this->setTitle (Session::NameRole, title);
      break;
    case (1):
      this->setTitle (Session::DisplayedTitleRole, title);
      break;
    }
}

QString
Session::title (int role) const
{
  switch (role)
    {
    case (0):
      return this->title (Session::NameRole);
    case (1):
      return this->title (Session::DisplayedTitleRole);
    default:
      return QString ();
    }
}

SessionGroup::SessionGroup (QObject * parent):QObject (parent), _masterMode (0)
{
}

SessionGroup::~SessionGroup ()
{
}

int
SessionGroup::masterMode () const
{
  return _masterMode;
}

QList < Session * >SessionGroup::sessions () const
{
  return _sessions.keys ();
}

bool
SessionGroup::masterStatus (Session * session) const
{
  return _sessions[session];
}

void
SessionGroup::addSession (Session * session)
{
  connect (session, SIGNAL (finished ()), this, SLOT (sessionFinished ()));
  _sessions.insert (session, false);
}

void
SessionGroup::removeSession (Session * session)
{
  disconnect (session, SIGNAL (finished ()), this, SLOT (sessionFinished ()));
  setMasterStatus (session, false);
  _sessions.remove (session);
}

void
SessionGroup::sessionFinished ()
{
  Session *session = qobject_cast < Session * >(sender ());
  Q_ASSERT (session);
  removeSession (session);
}

void
SessionGroup::setMasterMode (int mode)
{
  _masterMode = mode;
}

QList < Session * >SessionGroup::masters () const
{
  return _sessions.keys (true);
}

void
SessionGroup::setMasterStatus (Session * session, bool master)
{
  const bool wasMaster = _sessions[session];

  if (wasMaster == master)
    {
      // No status change -> nothing to do.
      return;
    }
  _sessions[session] = master;

  if (master)
    {
      //connect (session->emulation (), SIGNAL (sendData (const char *, int)),
        //       this, SLOT (forwardData (const char *, int)));
    }
  else
    {
      //disconnect (session->emulation (),
        //	  SIGNAL (sendData (const char *, int)), this,
        //	  SLOT (forwardData (const char *, int)));
    }
}

void
SessionGroup::forwardData (const char *data, int size)
{
  static bool _inForwardData = false;
  if (_inForwardData)
    {				// Avoid recursive calls among session groups!
      // A recursive call happens when a master in group A calls forwardData()
      // in group B. If one of the destination sessions in group B is also a
      // master of a group including the master session of group A, this would
      // again call forwardData() in group A, and so on.
      return;
    }

  _inForwardData = true;
  QListIterator < Session * >iter (_sessions.keys ());
  while (iter.hasNext ())
    {
      Session *other = iter.next ();
      if (!_sessions[other])
	{
          //other->emulation ()->sendString (data, size);
	}
    }
  _inForwardData = false;
}
