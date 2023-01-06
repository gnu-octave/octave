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

#if ! defined (octave_community_news_h)
#define octave_community_news_h 1

#include <QString>
#include <QWidget>

class QTextBrowser;

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;

class community_news : public QWidget
{
  Q_OBJECT

public:

  community_news (base_qobject& oct_qobj, int serial);

  community_news (base_qobject& oct_qobj, QWidget *parent = nullptr,
                  const QString& base_url = "https://octave.org",
                  const QString& page = "community-news.html",
                  int serial = -1);

  ~community_news (void) = default;

public slots:

  void set_news (const QString& news);

  void display (void);

private:

  void construct (base_qobject& oct_qobj, const QString& base_url,
                  const QString& page, int serial);

  QTextBrowser *m_browser;
};

OCTAVE_END_NAMESPACE(octave)

#endif
