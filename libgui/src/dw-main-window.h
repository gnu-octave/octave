/*

Copyright (C) 2013-2018 Torsten <mttl@mailbox.org>

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (dw_main_window_h)
#define dw_main_window_h 1

#include <QMainWindow>
#include <QSettings>


namespace octave
{

  class dw_main_window : public QMainWindow
  {
    Q_OBJECT

  public:

    dw_main_window (QWidget *parent = nullptr);

    ~dw_main_window (void) = default;

    // No copying!

    dw_main_window (const dw_main_window&) = delete;

    dw_main_window& operator = (const dw_main_window&) = delete;

  public slots:

    void notice_settings (const QSettings*);

  protected slots:

    virtual QMenu* createPopupMenu ();

  private slots:

    void request_close_file ();
    void request_close_all_files ();
    void request_close_other_files ();

  signals:

  protected:

  private:

    QAction *add_action (QMenu *menu, const QIcon& icon, const QString& text,
                         const char *member, QWidget *receiver);

    QAction *m_close_action;
    QAction *m_close_all_action;
    QAction *m_close_others_action;

  };

}

#endif
