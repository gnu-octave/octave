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

#if ! defined (octave_self_listener_h)
#define octave_self_listener_h 1

#include <QThread>
#include <unistd.h>

class self_listener : public QThread
{
  Q_OBJECT

public:

  explicit self_listener (const std::vector<int>& fds,
                          const QString &caller = QString (),
                          QObject *parent = nullptr);

  ~self_listener (void);

Q_SIGNALS:

  void receive_data (const char *buf_stdout, int len_stdout, int fd);

public Q_SLOTS:

private Q_SLOTS:

  void restore_streams (void);

protected:

  void run ();

private:

  void error_msg (const QString& msg, const std::string& err_str = std::string ());

  QString m_caller;

  struct redir_stream
  {
    FILE *stream;   // Stream to be redirected
    int fd;         // File descriptor of stream
    int old_fd;     // Store file descriptor for restore later
    int pipe_fd;    // file descriptor of pipe used for redirection
  };

  std::vector<redir_stream> m_redir_streams;
  int m_num_fds;
};

#endif
