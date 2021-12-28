////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2022 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QDesktopWidget>
#include <QLayout>
#include <QTextBrowser>
#include <QThread>

#include "community-news.h"
#include "gui-preferences-nr.h"
#include "news-reader.h"
#include "octave-qobject.h"

namespace octave
{
  community_news::community_news (base_qobject& oct_qobj, int serial)
    : QWidget (nullptr), m_browser (nullptr)
  {
    construct (oct_qobj, "https://octave.org", "community-news.html", serial);
  }

  community_news::community_news (base_qobject& oct_qobj, QWidget *parent,
                                  const QString& base_url, const QString& page,
                                  int serial)
    : QWidget (parent), m_browser (nullptr)
  {
    construct (oct_qobj, base_url, page, serial);
  }

  void community_news::construct (base_qobject& oct_qobj,
                                  const QString& base_url, const QString& page,
                                  int serial)
  {
    m_browser = new QTextBrowser (this);

    m_browser->setObjectName ("OctaveNews");
    m_browser->setOpenExternalLinks (true);

    QVBoxLayout *vlayout = new QVBoxLayout;

    vlayout->addWidget (m_browser);

    setLayout (vlayout);
    setWindowTitle (tr ("Octave Community News"));

    int win_x, win_y;
    get_screen_geometry (win_x, win_y);

    resize (win_x/2, win_y/2);
    move ((win_x - width ())/2, (win_y - height ())/2);

    resource_manager& rmgr = oct_qobj.get_resource_manager ();
    gui_settings *settings = rmgr.get_settings ();

    // FIXME: should be configurable...  See also the icon for the
    // release notes window.
    QString icon = ":/actions/icons/logo.png";
    setWindowIcon (QIcon (icon));

    // FIXME: This is a news reader preference, so shouldn't it be used
    // in the news_reader object?

    bool connect_to_web
      = (settings
         ? settings->value (nr_allow_connection).toBool ()
         : true);

    QThread *worker_thread = new QThread;

    news_reader *reader = new news_reader (oct_qobj, base_url, page,
                                           serial, connect_to_web);

    reader->moveToThread (worker_thread);

    connect (reader, &news_reader::display_news_signal,
             this, &community_news::set_news);

    connect (worker_thread, &QThread::started,
             reader, &news_reader::process);

    connect (reader, &news_reader::finished, worker_thread, &QThread::quit);

    connect (reader, &news_reader::finished, reader, &news_reader::deleteLater);

    connect (worker_thread, &QThread::finished,
             worker_thread, &QThread::deleteLater);

    worker_thread->start ();
  }

  void community_news::set_news (const QString& news)
  {
    m_browser->setHtml (news);
  }

  void community_news::display (void)
  {
    if (! isVisible ())
      show ();
    else if (isMinimized ())
      showNormal ();

    raise ();
    activateWindow ();
  }

  // FIXME: This function is duplicated in main_window.cc.  Maybe it
  // should be a utility function?

  void community_news::get_screen_geometry (int& width, int& height)
  {
    QRect screen_geometry = QApplication::desktop ()->availableGeometry (this);

    width = screen_geometry.width ();
    height = screen_geometry.height ();
  }
}
