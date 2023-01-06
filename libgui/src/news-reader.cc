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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <string>

#include <QDateTime>
#include <QString>

#include "news-reader.h"
#include "octave-qobject.h"
#include "gui-preferences-nr.h"

#include "url-transfer.h"
#include "version.h"

OCTAVE_BEGIN_NAMESPACE(octave)

void news_reader::process (void)
{
  QString html_text;

  if (m_connect_to_web)
    {
      // Run this part in a separate thread so Octave can continue to
      // run while we wait for the page to load.  Then emit the signal
      // to display it when we have the page contents.

      QString url = m_base_url + '/' + m_page;
      std::ostringstream buf;
      url_transfer octave_dot_org (url.toStdString (), buf);

      if (octave_dot_org.is_valid ())
        {
          Array<std::string> param;
          octave_dot_org.http_get (param);

          if (octave_dot_org.good ())
            html_text = QString::fromStdString (buf.str ());
        }

      if (html_text.contains ("this-is-the-gnu-octave-community-news-page"))
        {
          if (m_serial >= 0)
            {
              resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
              gui_settings *settings = rmgr.get_settings ();

              if (settings)
                {
                  settings->setValue (nr_last_time.key,
                                      QDateTime::currentDateTime ());

                  settings->sync ();
                }

              QString tag ("community-news-page-serial=");

              int b = html_text.indexOf (tag);

              if (b)
                {
                  b += tag.length ();

                  int e = html_text.indexOf ("\n", b);

                  QString tmp = html_text.mid (b, e-b);

                  int curr_page_serial = tmp.toInt ();

                  if (curr_page_serial > m_serial)
                    {
                      if (settings)
                        {
                          settings->setValue (nr_last_news.key,
                                              curr_page_serial);

                          settings->sync ();
                        }
                    }
                  else
                    return;
                }
              else
                return;
            }
        }
      else
        html_text = QString
          (tr ("<html>\n"
               "<body>\n"
               "<p>\n"
               "Octave's community news source seems to be unavailable.\n"
               "</p>\n"
               "<p>\n"
               "For the latest news, please check\n"
               "<a href=\"https://octave.org/community-news.html\">https://octave.org/community-news.html</a>\n"
               "when you have a connection to the web (link opens in an external browser).\n"
               "</p>\n"
               "<p>\n"
               "<small><em>&mdash; The Octave Developers, ") + OCTAVE_RELEASE_DATE + "</em></small>\n"
           "</p>\n"
           "</body>\n"
           "</html>\n");
    }
  else
    html_text = QString
      (tr ("<html>\n"
           "<body>\n"
           "<p>\n"
           "Connecting to the web to display the latest Octave Community news has been disabled.\n"
           "</p>\n"
           "<p>\n"
           "For the latest news, please check\n"
           "<a href=\"https://octave.org/community-news.html\">https://octave.org/community-news.html</a>\n"
           "when you have a connection to the web (link opens in an external browser)\n"
           "or enable web connections for news in Octave's network settings dialog.\n"
           "</p>\n"
           "<p>\n"
           "<small><em>&mdash; The Octave Developers, ") + OCTAVE_RELEASE_DATE + "</em></small>\n"
       "</p>\n"
       "</body>\n"
       "</html>\n");

  emit display_news_signal (html_text);

  emit finished ();
}

OCTAVE_END_NAMESPACE(octave)
