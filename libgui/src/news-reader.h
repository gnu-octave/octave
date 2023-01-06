////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2023 The Octave Project Developers
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

#if ! defined (octave_news_reader_h)
#define octave_news_reader_h 1

#include <QObject>
#include <QString>

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;

class news_reader : public QObject
{
  Q_OBJECT

public:

  news_reader (base_qobject& oct_qobj, const QString& base_url,
               const QString& page, int serial = -1,
               bool connect_to_web = false)
    : QObject (), m_octave_qobj (oct_qobj), m_base_url (base_url),
      m_page (page), m_serial (serial), m_connect_to_web (connect_to_web)
  { }

signals:

  void display_news_signal (const QString& news);

  void finished (void);

public slots:

  void process (void);

private:

  base_qobject& m_octave_qobj;

  QString m_base_url;
  QString m_page;
  int m_serial;
  bool m_connect_to_web;
};

OCTAVE_END_NAMESPACE(octave)

#endif
