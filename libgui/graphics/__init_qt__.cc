/*

Copyright (C) 2011-2018 Michael Goffioul

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QApplication>
#include <QDir>
#include <QFileDialog>
#include <QMetaType>
#include <QPalette>
#include <QRegExp>

#include "defun-dld.h"
#include "graphics.h"
#include "gtk-manager.h"
#include "interpreter.h"
#include "symtab.h"

#include "Backend.h"
#include "QtHandlesUtils.h"
#include "__init_qt__.h"

// PKG_ADD: if (__have_feature__ ("QT") && __have_feature__ ("OPENGL") && have_window_system () && isguirunning ()) register_graphics_toolkit ("qt"); endif

namespace QtHandles
{

  static bool qtHandlesInitialized = false;

  bool
  __init__ (octave::interpreter& interp)
  {
    if (! qtHandlesInitialized)
      {
        if (qApp)
          {
            gh_manager::auto_lock lock;

            qRegisterMetaType<graphics_object> ("graphics_object");

            gh_manager::enable_event_processing (true);

            octave::gtk_manager& gtk_mgr = interp.get_gtk_manager ();

            graphics_toolkit tk (new Backend ());
            gtk_mgr.load_toolkit (tk);

            octave::interpreter::add_atexit_function ("__shutdown_qt__");

            qtHandlesInitialized = true;

            return true;
          }
        else
          error ("__init_qt__: QApplication object must exist.");
      }

    return false;
  }

  bool
  __shutdown__ (void)
  {
    if (qtHandlesInitialized)
      {
        gh_manager::auto_lock lock;

        gh_manager::enable_event_processing (false);

        qtHandlesInitialized = false;

        return true;
      }

    return false;
  }
}

DEFMETHOD_DLD (__init_qt__, interp, , , "")
{
  QtHandles::__init__ (interp);

  return octave_value ();
}

DEFUN_DLD (__shutdown_qt__, , , "")
{
  QtHandles::__shutdown__ ();

  return octave_value ();
}

void
install___init_qt___functions (octave::symbol_table& symtab)
{
  symtab.install_built_in_function
    ("__init_qt__", octave_value (new octave_builtin
                                  (F__init_qt__, "__init_qt__",
                                   "__init_qt__.cc", "")));

  symtab.install_built_in_function
    ("__shutdown_qt__", octave_value (new octave_builtin
                                      (F__shutdown_qt__, "__shutdown_qt__",
                                       "__init_qt__.cc", "")));
}

#if 0

static QStringList
makeFilterSpecs (const Cell& filters)
{
  using namespace QtHandles::Utils;

  QStringList filterSpecs;
  QRegExp parenRe (" ?\\(.*\\)\\s*$");

  for (int i = 0; i < filters.rows (); i++)
    {
      QStringList extList =
        fromStdString (filters(i, 0).string_value ()).split (";");
      QString desc = fromStdString (filters(i, 1).string_value ()).trimmed ();
      QString specItem;

      if (desc.contains (parenRe))
        {
          // We need to strip any existing parenthesis and recreate it.
          // In case the format specified in the () section is not correct,
          // the filters won't work as expected.
          desc.remove (parenRe);
        }

      specItem = QString ("%1 (%2)").arg (desc).arg (extList.join (" "));

      filterSpecs.append (specItem);
    }

  return filterSpecs;
}

static QString
appendDirSep (const QString& d)
{
  if (! d.endsWith ("/") && ! d.endsWith (QDir::separator ()))
    return (d + '/');
  return d;
}

DEFUN_DLD (__uigetfile_qt__, args, , "")
{
  using namespace QtHandles::Utils;

  // Expected arguments:
  //   args(0) : File filter as a cell array {ext1, name1; ext2, name2; ...}
  //   args(1) : Dialog title
  //   args(2) : Default filename
  //   args(3) : Dialog position [ignored]
  //   args(4) : Multiselection "on"/"off"
  //   args(5) : Default directory

  octave_value_list retval (3);

  QString caption = fromStdString (args(1).string_value ());
  QString defaultDirectory = fromStdString (args(5).string_value ());
  QString defaultFileName = fromStdString (args(2).string_value ());
  bool isMultiSelect = (args(4).string_value () == "on");

  if (isMultiSelect)
    retval(0) = Cell ();
  else
    retval(0) = "";
  retval(1) = "";
  retval(2) = 0.0;

  if (defaultFileName.isEmpty ())
    defaultFileName = defaultDirectory;
  else
    defaultFileName = defaultDirectory + '/' + defaultFileName;

  QStringList filterSpecs = makeFilterSpecs (args(0).cell_value ());

  if (isMultiSelect)
    {
      QString filter;
      QStringList files =
        QFileDialog::getOpenFileNames (0, caption, defaultFileName,
                                       filterSpecs.join (";;"), &filter, 0);

      if (! files.isEmpty ())
        {
          Cell cFiles (1, files.length ());
          QString dirName;
          int i = 0;

          foreach (const QString& s, files)
            {
              QFileInfo fi (s);

              if (dirName.isEmpty ())
                dirName = appendDirSep (fi.canonicalPath ());
              cFiles(i++) = toStdString (fi.fileName ());
            }

          retval(0) = cFiles;
          retval(1) = toStdString (dirName);
          if (! filter.isEmpty ())
            retval(2) = static_cast<double> (filterSpecs.indexOf (filter) + 1);
        }
    }
  else
    {
      QString filter;
      QString fileName =
        QFileDialog::getOpenFileName (0, caption, defaultFileName,
                                      filterSpecs.join (";;"), &filter, 0);

      if (! fileName.isNull ())
        {
          QFileInfo fi (fileName);

          retval(0) = toStdString (fi.fileName ());
          retval(1) = toStdString (appendDirSep (fi.canonicalPath ()));
          if (! filter.isEmpty ())
            retval(2) = static_cast<double> (filterSpecs.indexOf (filter) + 1);
        }
    }

  return retval;
}

DEFUN_DLD (__uiputfile_qt__, args, , "")
{
  using namespace QtHandles::Utils;

  // Expected arguments:
  //   args(0) : File filter as a cell array {ext1, name1; ext2, name2; ...}
  //   args(1) : Dialog title
  //   args(2) : Default filename
  //   args(3) : Dialog position [ignored]
  //   args(4) : Tag [ignored]
  //   args(5) : Default directory

  octave_value_list retval (3);

  QString caption = fromStdString (args(1).string_value ());
  QString defaultDirectory = fromStdString (args(5).string_value ());
  QString defaultFileName = fromStdString (args(2).string_value ());

  retval(0) = "";
  retval(1) = "";
  retval(2) = 0.0;

  if (defaultFileName.isEmpty ())
    defaultFileName = defaultDirectory;
  else
    defaultFileName = defaultDirectory + '/' + defaultFileName;

  QStringList filterSpecs = makeFilterSpecs (args(0).cell_value ());

  QString filter;
  QString fileName =
    QFileDialog::getSaveFileName (0, caption, defaultFileName,
                                  filterSpecs.join (";;"), &filter, 0);

  if (! fileName.isNull ())
    {
      QFileInfo fi (fileName);

      retval(0) = toStdString (fi.fileName ());
      if (fi.exists ())
        retval(1) = toStdString (appendDirSep (fi.canonicalPath ()));
      else
        retval(1) = toStdString (appendDirSep (fi.absolutePath ()));
      if (! filter.isEmpty ())
        retval(2) = static_cast<double> (filterSpecs.indexOf (filter) + 1);
    }

  return retval;
}

DEFUN_DLD (__uigetdir_qt__, args, , "")
{
  using namespace QtHandles::Utils;

  // Expected arguments:
  //   args(0) : Start directory
  //   args(1) : Dialog title

  octave_value retval ("");

  QString caption = fromStdString (args(1).string_value ());
  QString defaultDirectory = fromStdString (args(0).string_value ());

  QString dirName = QFileDialog::getExistingDirectory (0, caption,
                    defaultDirectory);

  if (! dirName.isNull ())
    retval = toStdString (dirName);

  return retval;
}

#endif
