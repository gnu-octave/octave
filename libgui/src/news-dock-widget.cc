/*

Copyright (C) 2013 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include <QVBoxLayout>
#include <QThread>

#include "news-dock-widget.h"

#include "Array.h"
#include "str-vec.h"
#include "url-transfer.h"

#include "version.h"

news_dock_widget::news_dock_widget (QWidget *p)
  : octave_dock_widget (p), news_browser (new QWebView (p))
{
  news_browser->setObjectName ("OctaveNews");

  setObjectName ("NewsDockWidget");
  setWindowIcon (QIcon (":/icons/logo.png"));
  set_title (tr ("Community News"));

  setWidget (news_browser);

  load_news ();
}

void
news_dock_widget::load_news (void)
{
  QString base_url = "http://octave.org";
  QString page = "community-news.html";

  QThread *worker_thread = new QThread;

  news_reader *reader = new news_reader (base_url, page);

  reader->moveToThread (worker_thread);

  connect (reader, SIGNAL (display_news_signal (const QString&, const QUrl&)),
           this, SLOT (display_news (const QString&, const QUrl&)));

  connect (worker_thread, SIGNAL (started (void)), reader, SLOT (process ()));

  connect (reader, SIGNAL (finished (void)), worker_thread, SLOT (quit ()));

  connect (reader, SIGNAL (finished (void)), reader, SLOT (deleteLater ()));

  connect (worker_thread, SIGNAL (finished (void)),
           worker_thread, SLOT (deleteLater ()));

  worker_thread->start ();
}

static const char fixed_news[] = "<html>\n\
<body>\n\
<p>\n\
This window will be used to inform you about Octave community events.\n\
Octave may show it to you even if you've chosen hide the window by\n\
default.  We'll try not to bother you too much, but we do want to keep\n\
you up to date with the latest information about important bug fixes,\n\
new releases, or any other news that all Octave users should be aware of.\n\
</p>\n\
<p>\n\
Currently, Octave's community news source seems to be unavailable.\n\
For the latest news, please check\n\
<a href=\"http://octave.org/community-news.html\">http://octave.org/community-news.html</a>\n\
when you have a connection to the web.\n\
</p>\n\
<p>\n\
<small><em>&mdash; The Octave Developers, " OCTAVE_RELEASE_DATE "</em></small>\n\
</body>\n\
</html>\n";

void
news_dock_widget::display_news (const QString& news, const QUrl& base_url)
{
  if (news.contains ("this-is-the-gnu-octave-community-news-page"))
    {
      news_browser->setHtml (news, base_url);

      if (news.contains ("critical-news-event") && ! isVisible ())
        setVisible (true);
    }
  else
    news_browser->setHtml (fixed_news);
}

void
news_reader::process (void)
{
  // Run this part in a separate thread so Octave can continue to run
  // while we wait for the page to load.  Then emit the signal to
  // display it when we have the page contents.

  QString url = base_url + "/" + page;
  std::ostringstream buf;
  url_transfer octave_dot_org (url.toStdString (), buf);

  Array<std::string> param;
  octave_dot_org.http_get (param);

  QString html_text;

  if (octave_dot_org.good ())
    html_text = QString::fromStdString (buf.str ());

  emit display_news_signal (html_text, QUrl (base_url));

  emit finished ();
}
