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
#include "builtin-defun-decls.h"
#include "load-path.h"
#include "oct-env.h"
#include "utils.h"

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
  return uiwidget_creator.get_dialog_button().toStdString ();
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

static QStringList
make_filter_list (const octave_link::filter_list& lst)
{
  QStringList retval;

  // We have pairs of data, first being the list of extensions
  // exta;exb;extc etc second the name to use as filter name
  // (optional).  Qt wants a a list of filters in the format of
  // 'FilterName (space separated exts)'.

  for (octave_link::filter_list::const_iterator it = lst.begin ();
       it != lst.end (); it++)
    {
      QString ext = QString::fromStdString (it->first);
      QString name = QString::fromStdString (it->second);

      // Strip out extensions from name and replace ';' with spaces in
      // list.

      name.replace (QRegExp ("\\(.*\\)"), "");
      ext.replace (";", " ");

      if (name.length() == 0)
        {
          // No name field.  Build one from the extensions.
          name = ext.toUpper() + " Files";
        }

      retval.append (name + " (" + ext + ")");
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

std::list<std::string>
octave_qt_link::do_file_dialog (const filter_list& filter,
                                const std::string& title,
                                const std::string& filename,
                                const std::string& dirname,
                                const std::string& multimode)
{
  std::list<std::string> retval;

  uiwidget_creator.signal_filedialog (make_filter_list (filter),
                                      QString::fromStdString (title),
                                      QString::fromStdString (filename),
                                      QString::fromStdString (dirname),
                                      QString::fromStdString (multimode));

  // Wait while the user is responding to dialog.
  uiwidget_creator.wait ();

  // Add all the file dialog results to a string list.
  const QStringList *inputLine = uiwidget_creator.get_string_list ();

  for (QStringList::const_iterator it = inputLine->begin ();
       it != inputLine->end (); it++)
    retval.push_back (it->toStdString ());

  retval.push_back (uiwidget_creator.get_dialog_path ()->toStdString ());
  retval.push_back ((QString ("%1").arg (uiwidget_creator.get_dialog_result ())).toStdString ());

  return retval;
}

int
octave_qt_link::do_debug_cd_or_addpath_error (const std::string& file,
                                              const std::string& dir,
                                              bool addpath_option)
{
  int retval = -1;

  QString qdir = QString::fromStdString (dir);
  QString qfile = QString::fromStdString (file);

  QString msg
    = (addpath_option
       ? tr ("The file %1 does not exist in the load path.  To debug the function you are editing, you must either change to the directory %2 or add that directory to the load path.").arg(qfile).arg(qdir)
       : tr ("The file %1 is shadowed by a file with the same name in the load path.  To debug the function you are editing, change to the directory %2.").arg(qfile).arg(qdir));

  QString title = tr ("Change Directory or Add Directory to Load Path");

  QString cd_txt = tr ("Change Directory");
  QString addpath_txt = tr ("Add Directory to Load Path");
  QString cancel_txt = tr ("Cancel");

  QStringList btn;
  QStringList role;
  btn << cd_txt;
  role << "AcceptRole";
  if (addpath_option)
    {
      btn << addpath_txt;
      role << "AcceptRole";
    }
  btn << cancel_txt;
  role << "AcceptRole";

  uiwidget_creator.signal_dialog (msg, title, "quest", btn, cancel_txt, role);

  // Wait while the user is responding to message box.
  uiwidget_creator.wait ();

  QString result = uiwidget_creator.get_dialog_button ();

  if (result == cd_txt)
    retval = 1;
  else if (result == addpath_txt)
    retval = 2;

  return retval;
}

void
octave_qt_link::do_change_directory (const std::string& dir)
{
  emit change_directory_signal (QString::fromStdString (dir));
}

void
octave_qt_link::do_execute_command_in_terminal (const std::string& command)
{
  emit execute_command_in_terminal_signal (QString::fromStdString (command));
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


bool
octave_qt_link::file_in_path (const std::string& file, const std::string& dir)
{

  bool ok = false;
  bool addpath_option = true;

  std::string curr_dir = octave_env::get_current_directory ();

  if (same_file (curr_dir, dir))
    ok = true;
  else
    {
      bool dir_in_load_path = load_path::contains_canonical (dir);

      std::string base_file = octave_env::base_pathname (file);
      std::string lp_file = load_path::find_file (base_file);

      if (dir_in_load_path)
        {
          if (same_file (lp_file, file))
            ok = true;
        }
      else
        {
          // File directory is not in path.  Is the file in the path in
          // the current directory?  If so, then changing the current
          // directory will be needed.  Adding directory to path is
          // not enough because the file in the current directory would
          // still be found.

          if (same_file (lp_file, base_file))
            {
              if (same_file (curr_dir, dir))
                ok = true;
              else
                addpath_option = false;
            }
        }
    }

  if (! ok)
    {
      int action = debug_cd_or_addpath_error (file, dir, addpath_option);
      switch (action)
        {
        case 1:
          Fcd (ovl (dir));
          ok = true;
          break;

        case 2:
          load_path::prepend (dir);
          ok = true;
          break;

        default:
          break;
        }
    }

  return ok;
}
