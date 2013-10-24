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

#ifndef NEWSDOCKWIDGET_H
#define NEWSDOCKWIDGET_H

#include <QTextBrowser>

#include "octave-dock-widget.h"

class news_dock_widget : public octave_dock_widget
{
  Q_OBJECT

public:

  news_dock_widget (QWidget *parent = 0);

  void load_news (void);

protected slots:

  void display_news (const QString& news);

  /* from octave_dock_widget */
  void copyClipboard ();

private:

  QTextBrowser *browser;
};

class news_reader : public QObject
{
  Q_OBJECT
 
public:

  news_reader (const QString& xbase_url, const QString& xpage)
    : QObject (), base_url (xbase_url), page (xpage) { }
 
public slots:

    void process (void);
 
signals:

  void display_news_signal (const QString& news);

  void finished (void);
 
private:

  QString base_url;
  QString page;
};

#endif // NEWSDOCKWIDGET_H
