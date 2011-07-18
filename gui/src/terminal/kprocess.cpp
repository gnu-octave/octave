/*
    This file is part of the KDE libraries

    Copyright (C) 2007 Oswald Buddenhagen <ossi@kde.org>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public License
    along with this library; see the file COPYING.LIB.  If not, write to
    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301, USA.
*/

#include "kprocess_p.h"

#include <qfile.h>

#ifdef Q_OS_WIN
#include <windows.h>
#else
#include <unistd.h>
#include <errno.h>
#endif

#ifndef Q_OS_WIN
#define STD_OUTPUT_HANDLE 1
#define STD_ERROR_HANDLE 2
#endif

#ifdef _WIN32_WCE
#include <stdio.h>
#endif

void
KProcessPrivate::writeAll (const QByteArray & buf, int fd)
{
#ifdef Q_OS_WIN
#ifndef _WIN32_WCE
  HANDLE h = GetStdHandle (fd);
  if (h)
    {
      DWORD wr;
      WriteFile (h, buf.data (), buf.size (), &wr, 0);
    }
#else
  fwrite (buf.data (), 1, buf.size (), (FILE *) fd);
#endif
#else
  int off = 0;
  do
    {
      int ret =::write (fd, buf.data () + off, buf.size () - off);
      if (ret < 0)
	{
	  if (errno != EINTR)
	    return;
	}
      else
	{
	  off += ret;
	}
    }
  while (off < buf.size ());
#endif
}

void
KProcessPrivate::forwardStd (KProcess::ProcessChannel good, int fd)
{
  Q_Q (KProcess);

  QProcess::ProcessChannel oc = q->readChannel ();
  q->setReadChannel (good);
  writeAll (q->readAll (), fd);
  q->setReadChannel (oc);
}

void
KProcessPrivate::_k_forwardStdout ()
{
#ifndef _WIN32_WCE
  forwardStd (KProcess::StandardOutput, STD_OUTPUT_HANDLE);
#else
  forwardStd (KProcess::StandardOutput, (int) stdout);
#endif
}

void
KProcessPrivate::_k_forwardStderr ()
{
#ifndef _WIN32_WCE
  forwardStd (KProcess::StandardError, STD_ERROR_HANDLE);
#else
  forwardStd (KProcess::StandardError, (int) stderr);
#endif
}

/////////////////////////////
// public member functions //
/////////////////////////////

KProcess::KProcess (QObject * parent):
QProcess (parent), d_ptr (new KProcessPrivate)
{
  d_ptr->q_ptr = this;
  setOutputChannelMode (ForwardedChannels);
}

KProcess::KProcess (KProcessPrivate * d, QObject * parent):
QProcess (parent), d_ptr (d)
{
  d_ptr->q_ptr = this;
  setOutputChannelMode (ForwardedChannels);
}

KProcess::~KProcess ()
{
  delete d_ptr;
}

void
KProcess::setOutputChannelMode (OutputChannelMode mode)
{
  Q_D (KProcess);

  d->outputChannelMode = mode;
  disconnect (this, SIGNAL (readyReadStandardOutput ()));
  disconnect (this, SIGNAL (readyReadStandardError ()));
  switch (mode)
    {
    case OnlyStdoutChannel:
      connect (this, SIGNAL (readyReadStandardError ()),
	       SLOT (_k_forwardStderr ()));
      break;
    case OnlyStderrChannel:
      connect (this, SIGNAL (readyReadStandardOutput ()),
	       SLOT (_k_forwardStdout ()));
      break;
    default:
      QProcess::setProcessChannelMode ((ProcessChannelMode) mode);
      return;
    }
  QProcess::setProcessChannelMode (QProcess::SeparateChannels);
}

KProcess::OutputChannelMode KProcess::outputChannelMode () const
{
  Q_D (const KProcess);

  return d->outputChannelMode;
}

#define DUMMYENV "_KPROCESS_DUMMY_="

void
KProcess::setEnv (const QString & name, const QString & value, bool overwrite)
{
  QStringList env = environment ();
  if (env.isEmpty ())
    {
      env = systemEnvironment ();
      env.removeAll (QString::fromLatin1 (DUMMYENV));
    }
  QString fname (name);
  fname.append (QLatin1Char ('='));
  for (QStringList::Iterator it = env.begin (); it != env.end (); ++it)
    if ((*it).startsWith (fname))
      {
	if (overwrite)
	  {
	    *it = fname.append (value);
	    setEnvironment (env);
	  }
	return;
      }
  env.append (fname.append (value));
  setEnvironment (env);
}

void
KProcess::unsetEnv (const QString & name)
{
  QStringList env = environment ();
  if (env.isEmpty ())
    {
      env = systemEnvironment ();
      env.removeAll (QString::fromLatin1 (DUMMYENV));
    }
  QString fname (name);
  fname.append (QLatin1Char ('='));
  for (QStringList::Iterator it = env.begin (); it != env.end (); ++it)
    if ((*it).startsWith (fname))
      {
	env.erase (it);
	if (env.isEmpty ())
	  env.append (QString::fromLatin1 (DUMMYENV));
	setEnvironment (env);
	return;
      }
}

void
KProcess::setProgram (const QString & exe, const QStringList & args)
{
  Q_D (KProcess);

  d->prog = exe;
  d->args = args;
#ifdef Q_OS_WIN
  setNativeArguments (QString ());
#endif
}

void
KProcess::setProgram (const QStringList & argv)
{
  Q_D (KProcess);

  Q_ASSERT (!argv.isEmpty ());
  d->args = argv;
  d->prog = d->args.takeFirst ();
#ifdef Q_OS_WIN
  setNativeArguments (QString ());
#endif
}

KProcess & KProcess::operator<< (const QString & arg)
{
  Q_D (KProcess);

  if (d->prog.isEmpty ())
    d->prog = arg;
  else
    d->args << arg;
  return *this;
}

KProcess & KProcess::operator<< (const QStringList & args)
{
  Q_D (KProcess);

  if (d->prog.isEmpty ())
    setProgram (args);
  else
    d->args << args;
  return *this;
}

void
KProcess::clearProgram ()
{
  Q_D (KProcess);

  d->prog.clear ();
  d->args.clear ();
#ifdef Q_OS_WIN
  setNativeArguments (QString ());
#endif
}

QStringList
KProcess::program () const
{
  Q_D (const KProcess);

  QStringList argv = d->args;
  argv.prepend (d->prog);
  return argv;
}

void
KProcess::start ()
{
  Q_D (KProcess);

  QProcess::start (d->prog, d->args, d->openMode);
}

int
KProcess::execute (int msecs)
{
  start ();
  if (!waitForFinished (msecs))
    {
      kill ();
      waitForFinished (-1);
      return -2;
    }
  return (exitStatus () == QProcess::NormalExit) ? exitCode () : -1;
}

// static
int
KProcess::execute (const QString & exe, const QStringList & args, int msecs)
{
  KProcess p;
  p.setProgram (exe, args);
  return p.execute (msecs);
}

// static
int
KProcess::execute (const QStringList & argv, int msecs)
{
  KProcess p;
  p.setProgram (argv);
  return p.execute (msecs);
}

int
KProcess::startDetached ()
{
  Q_D (KProcess);

  qint64 pid;
  if (!QProcess::startDetached (d->prog, d->args, workingDirectory (), &pid))
    return 0;
  return (int) pid;
}

// static
int
KProcess::startDetached (const QString & exe, const QStringList & args)
{
  qint64 pid;
  if (!QProcess::startDetached (exe, args, QString (), &pid))
    return 0;
  return (int) pid;
}

// static
int
KProcess::startDetached (const QStringList & argv)
{
  QStringList args = argv;
  QString prog = args.takeFirst ();
  return startDetached (prog, args);
}

int
KProcess::pid () const
{
#ifdef Q_OS_UNIX
  return (int) QProcess::pid ();
#else
  return QProcess::pid ()? QProcess::pid ()->dwProcessId : 0;
#endif
}
