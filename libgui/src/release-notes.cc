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

#include <QApplication>
#include <QFile>
#include <QIcon>
#include <QLayout>
#include <QScreen>
#include <QTextBrowser>
#include <QTextStream>
#include <QThread>

#include "release-notes.h"
#include "gui-utils.h"
#include "gui-preferences-dw.h"
#include "gui-preferences-nr.h"
#include "news-reader.h"
#include "octave-qobject.h"

#include "defaults.h"

OCTAVE_BEGIN_NAMESPACE(octave)

release_notes::release_notes (base_qobject& oct_qobj)
: QWidget (nullptr), m_browser (nullptr)
{

  resource_manager& rmgr = oct_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  // The icon
  QString icon_set = settings->value (dw_icon_set).toString ();
  if (icon_set != "NONE")
    m_release_notes_icon = dw_icon_set_names[icon_set]
      + "ReleaseWidget.png";
  else
    m_release_notes_icon = dw_icon_set_names[icon_set];

  std::string news_file = config::oct_etc_dir () + "/NEWS";

  QString news;

  QFile *file = new QFile (QString::fromStdString (news_file));
  if (file->open (QFile::ReadOnly))
    {
      QTextStream *stream = new QTextStream (file);
      news = stream->readAll ();
      if (! news.isEmpty ())
        {
          // Convert '<', '>' which would be interpreted as HTML
          news.replace ("<", "&lt;");
          news.replace (">", "&gt;");
          // Add HTML tags for pre-formatted text
          news.prepend ("<pre>");
          news.append ("</pre>");
        }
      else
        news = (tr ("The release notes file '%1' is empty.")
                . arg (QString::fromStdString (news_file)));
    }
  else
    news = (tr ("The release notes file '%1' cannot be read.")
            . arg (QString::fromStdString (news_file)));

  m_browser = new QTextBrowser (this);
  m_browser->setText (news);

  QVBoxLayout *vlayout = new QVBoxLayout;
  vlayout->addWidget (m_browser);

  setLayout (vlayout);
  setWindowTitle (tr ("Octave Release Notes"));

  m_browser->document ()->adjustSize ();

  int win_x, win_y;
  get_screen_geometry (win_x, win_y);

  resize (win_x*2/5, win_y*2/3);
  move (20, 20);  // move to the top left corner
}

void release_notes::display (void)
{
  if (! isVisible ())
    show ();
  else if (isMinimized ())
    showNormal ();

  setWindowIcon (QIcon (m_release_notes_icon));

  raise ();
  activateWindow ();
}

OCTAVE_END_NAMESPACE(octave)
