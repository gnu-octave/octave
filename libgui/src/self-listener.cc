////////////////////////////////////////////////////////////////////////
//
// self-listener: redirection of output streams
//
// Copyright (C) 2024 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
//
// TODO:
//    * We might want to filter output so that terminal escape sequences
//      are processed (see the Unix version of the old terminal widget)
//    * Maybe the self_listener could use a QString object instead
//      of a character array + length buffer?
//    * See also https://github.com/dmikushin/stdcapture and the stack
//      overflow discussion linked there.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstring>

#if defined (OCTAVE_USE_WINDOWS_API)
#  include <winsock2.h>
#else
#  include <sys/select.h>
#endif

#include <QMessageBox>

#include "self-listener.h"

#include "oct-syscalls.h"

self_listener::self_listener (const std::vector<int>& fds,
                              const QString& caller,
                              QObject *parent)
  : QThread (parent),
    m_caller (caller),
    m_redir_streams ()
{
  m_num_fds = fds.size ();

  // Get all fd that should be redirected
  m_redir_streams.resize (m_num_fds);
  for (int i = 0; i < m_num_fds; i++)
    m_redir_streams.at (i).fd = fds.at (i);

  int pipe_fd[2];

  // Initialize all required data for redirection with pipes and select
  for (auto& rs : m_redir_streams)
    {
      if (! (rs.stream = fdopen (rs.fd, "wb")))
        {
          error_msg (
              QString (tr ("Can not open redirected stream with fd = %1."))
                      .arg (rs.fd));
          return;
        }
      else
        {
          // No buffering.
          if (::setvbuf (rs.stream, nullptr, _IONBF, 0))
            {
              error_msg (
                  QString (tr ("Can not disable buffering of stream with fd = %1."))
                          .arg (rs.fd));
              return;
            }
        }

      // m_old_fds should be used to restore the original output stream
      // when the command widget and the self listener are destroyed.
      rs.old_fd = ::dup (rs.fd);
      if (rs.old_fd == -1)
        {
          error_msg (
              QString (tr ("Can not dup redirected stream with fd = %1."))
                      .arg (rs.fd));
          return;
        }

      // Hook up the existing streams to a pipe that we can read from.
      std::string error_str;
      if (octave::sys::pipe (pipe_fd, error_str) < 0)
        {
          error_msg (
              QString (tr ("Cannot create pipe for redirecting stream with fd = %1:"))
                      .arg (rs.fd), error_str);
          return;
        }
      if (octave::sys::dup2 (pipe_fd[1], rs.fd, error_str) < 0)
        {
          error_msg (
              QString (tr ("Cannot dup2 redirected stream with fd = %1\n"
                           "to pipe with fd = %2: %3"))
                .arg (rs.fd).arg (pipe_fd[1]), error_str);
          return;
        }

      rs.pipe_fd = pipe_fd[0];
    }

  // Make sure that the thread is deleted after usage
  // but restores the streams before
  connect (this, &self_listener::finished,
           this, &self_listener::restore_streams);

  connect (this, &self_listener::finished,
           this, &self_listener::deleteLater);
}

self_listener::~self_listener (void)
{
  restore_streams ();
}

void self_listener::run ()
{
  // Initialize structure required by select
  fd_set redir_fds;
  FD_ZERO (&redir_fds);

  // Required variables
  char buf[4096 + 1];
  int len = 0;
  int pipes_with_data;
  bool running = true;

  // Value of highest fd tracked by select (+1, see man select)
  int fdmax = 0;
  for (auto& rs : m_redir_streams)
    fdmax = (rs.pipe_fd > fdmax) ? rs.pipe_fd : fdmax;
  fdmax++;

  // The main loop for tracking the pipes
  while (running)
    {
      // Set the fds that should be scanned by select
      for (auto& rs : m_redir_streams)
        FD_SET (rs.pipe_fd, &redir_fds);

      // Pipes with data ready for being read. No timeout, wait
      // until a file descriptor is ready.
      pipes_with_data = select (fdmax, &redir_fds, nullptr, nullptr, nullptr);

      if (pipes_with_data == 0)
        continue;  // timeout reached (not used here)

      if (pipes_with_data < 0)
        {
          error_msg (QString (tr ("Error while listening to redirected streams")));
          running = false;
          continue;
        }

        for (auto& rs : m_redir_streams)
          {
            if (FD_ISSET (rs.pipe_fd, &redir_fds))
              {
                if ((len = ::read (rs.pipe_fd, buf, 4096)) > 0)
                  {
                    buf[len] = 0;  // Just in case.
                    Q_EMIT receive_data (buf, len, rs.fd);
                  }
                else if  (len < 0)
                  {
                    error_msg (
                      QString (tr ("Error reading from redirected strem fd = %1."))
                      .arg (rs.fd));
                    running = false;
                    break;
                  }
              }
          }

    }
}

void
self_listener::restore_streams (void)
{
  for (auto& rs : m_redir_streams)
    octave::sys::dup2 (rs.old_fd, rs.fd);
}

void
self_listener::error_msg (const QString& msg, const std::string& err_str)
{
  QString title = QString ("Octave");
  QString info = QString ();

  if (! m_caller.isEmpty ())
    {
      title = title + QString (" ") + m_caller;
      info = QString (tr ("\nOutput redirection in ")) +
             m_caller + QString (tr (" won't work."));
    }

  std::string err_msg = err_str;
  if (err_msg.empty ())
    err_msg = std::strerror (errno);
      
  QString error_message = msg + info +
                          QString (tr ("\nError: ")) +
                          QString::fromStdString (err_msg);

  QMessageBox msg_box (QMessageBox::Critical, QString (tr ("Octave")),
                       error_message, QMessageBox::Ok);
  msg_box.exec ();
}
