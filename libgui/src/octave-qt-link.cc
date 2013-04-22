/*

Copyright (C) 2013 John W. Eaton
Copyright (C) 2011-2012 Jacob Dawid
Copyright (C) 2011-2012 John P. Swensen

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

#include <QStringList>

#include "str-vec.h"

#include "dialog.h"
#include "error.h"
#include "workspace-element.h"

#include "octave-qt-link.h"

octave_qt_link::octave_qt_link (octave_main_thread *mt)
  : octave_link (), main_thread (mt)
{ }

octave_qt_link::~octave_qt_link (void) { }

void
octave_qt_link::execute_interpreter (void)
{
  main_thread->execute_interpreter ();
}

bool
octave_qt_link::do_exit (int status)
{
  emit exit_signal (status);

  return true;
}

bool
octave_qt_link::do_edit_file (const std::string& file)
{
  emit edit_file_signal (QString::fromStdString (file));

  return true;
}

int
octave_qt_link::do_message_dialog (const std::string& dlg,
                                   const std::string& msg,
                                   const std::string& title)
{
  uiwidget_creator.signal_dialog (QString::fromStdString (msg),
                                  QString::fromStdString (title),
                                  QString::fromStdString (dlg),
                                  QStringList (), QString (),
                                  QStringList ());

  // Wait while the user is responding to message box.
  uiwidget_creator.wait ();

  // The GUI has sent a signal and the process has been awakened.
  return uiwidget_creator.get_dialog_result ();
}

std::string
octave_qt_link::do_question_dialog (const std::string& msg,
                                    const std::string& title,
                                    const std::string& btn1,
                                    const std::string& btn2,
                                    const std::string& btn3,
                                    const std::string& btndef)
{
  QStringList btn;
  QStringList role;
  role << "AcceptRole" << "AcceptRole" << "AcceptRole";
  btn << QString::fromStdString (btn1);
  if (btn2 == "")
    role.removeAt (0);
  else
    btn << QString::fromStdString (btn2);
  btn << QString::fromStdString (btn3);

  uiwidget_creator.signal_dialog (QString::fromStdString (msg),
                                  QString::fromStdString (title),
                                  "quest",
                                  btn,
                                  QString::fromStdString (btndef),
                                  role);

  // Wait while the user is responding to message box.
  uiwidget_creator.wait ();

  // The GUI has sent a signal and the process has been awakened.
  return uiwidget_creator.get_dialog_button ()->toStdString ();
}

static QStringList
make_qstring_list (const std::list<std::string>& lst)
{
  QStringList retval;

  for (std::list<std::string>::const_iterator it = lst.begin ();
       it != lst.end (); it++)
    {
      retval.append (QString::fromStdString (*it));
    }

  return retval;
}


std::pair<std::list<int>, int>
octave_qt_link::do_list_dialog (const std::list<std::string>& list,
                                const std::string& mode,
                                int width, int height,
                                const std::list<int>& initial,
                                const std::string& name,
                                const std::list<std::string>& prompt,
                                const std::string& ok_string,
                                const std::string& cancel_string)
{
  uiwidget_creator.signal_listview (make_qstring_list (list),
                                    QString::fromStdString (mode),
                                    width, height,
                                    QList<int>::fromStdList (initial),
                                    QString::fromStdString (name),
                                    make_qstring_list (prompt),
                                    QString::fromStdString (ok_string),
                                    QString::fromStdString (cancel_string));

  // Wait while the user is responding to message box.
  uiwidget_creator.wait ();

  // The GUI has sent a signal and the process has been awakened.
  const QIntList *selected = uiwidget_creator.get_list_index ();
  int ok = uiwidget_creator.get_dialog_result ();

  return std::pair<std::list<int>, int> (selected->toStdList (), ok);
}

std::list<std::string>
octave_qt_link::do_input_dialog (const std::list<std::string>& prompt,
                                 const std::string& title,
                                 const std::list<float>& nr,
                                 const std::list<float>& nc,
                                 const std::list<std::string>& defaults)
{
  std::list<std::string> retval;

  uiwidget_creator.signal_inputlayout (make_qstring_list (prompt),
                                       QString::fromStdString (title),
                                       QFloatList::fromStdList (nr),
                                       QFloatList::fromStdList (nc),
                                       make_qstring_list (defaults));

  // Wait while the user is responding to message box.
  uiwidget_creator.wait ();

  // The GUI has sent a signal and the process has been awakened.
  const QStringList *inputLine = uiwidget_creator.get_string_list ();

  for (QStringList::const_iterator it = inputLine->begin ();
       it != inputLine->end (); it++)
    {
      retval.push_back (it->toStdString ());
    }

  return retval;
}

int
octave_qt_link::do_debug_cd_or_addpath_error (const std::string& file,
                                              const std::string& dir,
                                              bool addpath_option)
{
  uiwidget_creator.signal_debug_cd_or_addpath (QString::fromStdString (file),
                                               QString::fromStdString (dir),
                                               addpath_option);

  uiwidget_creator.wait ();

  return uiwidget_creator.get_dialog_result ();
}

void
octave_qt_link::do_change_directory (const std::string& dir)
{
  emit change_directory_signal (QString::fromStdString (dir));
}

void
octave_qt_link::do_set_workspace (bool top_level,
                                  const std::list<workspace_element>& ws)
{
  QString scopes;
  QStringList symbols;
  QStringList class_names;
  QStringList dimensions;
  QStringList values;

  for (std::list<workspace_element>::const_iterator it = ws.begin ();
       it != ws.end (); it++)
    {
      scopes.append (it->scope ());
      symbols.append (QString::fromStdString (it->symbol ()));
      class_names.append (QString::fromStdString (it->class_name ()));
      dimensions.append (QString::fromStdString (it->dimension ()));
      values.append (QString::fromStdString (it->value ()));
    }

  emit set_workspace_signal (top_level, scopes, symbols, class_names,
                             dimensions, values);
}

void
octave_qt_link::do_clear_workspace (void)
{
  emit clear_workspace_signal ();
}

void
octave_qt_link::do_set_history (const string_vector& hist)
{
  QStringList qt_hist;

  for (octave_idx_type i = 0; i < hist.length (); i++)
    qt_hist.append (QString::fromStdString (hist[i]));

  emit set_history_signal (qt_hist);
}

void
octave_qt_link::do_append_history (const std::string& hist_entry)
{
  emit append_history_signal (QString::fromStdString (hist_entry));
}

void
octave_qt_link::do_clear_history (void)
{
  emit clear_history_signal ();
}

void
octave_qt_link::do_pre_input_event (void)
{
}

void
octave_qt_link::do_post_input_event (void)
{
}

void
octave_qt_link::do_enter_debugger_event (const std::string& file, int line)
{
  do_insert_debugger_pointer (file, line);

  emit enter_debugger_signal ();
}

void
octave_qt_link::do_execute_in_debugger_event (const std::string& file, int line)
{
  do_delete_debugger_pointer (file, line);
}

void
octave_qt_link::do_exit_debugger_event (void)
{
  emit exit_debugger_signal ();
}

void
octave_qt_link::do_update_breakpoint (bool insert,
                                      const std::string& file, int line)
{
  emit update_breakpoint_marker_signal (insert, QString::fromStdString (file), line);
}

void
octave_qt_link::do_set_default_prompts (std::string& ps1, std::string& ps2,
                                        std::string& ps4)
{
  ps1 = ">> ";
  ps2 = "";
  ps4 = "";
}


void
octave_qt_link::do_insert_debugger_pointer (const std::string& file, int line)
{
  emit insert_debugger_pointer_signal (QString::fromStdString (file), line);
}

void
octave_qt_link::do_delete_debugger_pointer (const std::string& file, int line)
{
  emit delete_debugger_pointer_signal (QString::fromStdString (file), line);
}
