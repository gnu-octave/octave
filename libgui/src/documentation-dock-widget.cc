/*

Copyright (C) 2011-2018 Jacob Dawid

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

#include "documentation-dock-widget.h"

#include "help.h"
#include "interpreter-private.h"

namespace octave
{
  documentation_dock_widget::documentation_dock_widget (QWidget *p)
    : octave_dock_widget (p)
  {
    setObjectName ("DocumentationDockWidget");
    setWindowIcon (QIcon (":/actions/icons/logo.png"));
    set_title (tr ("Documentation"));
    setStatusTip (tr ("See the documentation for help."));

    m_docs = new octave::documentation (this);
    setWidget (m_docs);

    connect (p, SIGNAL (show_doc_signal (const QString&)),
             this, SLOT (showDoc (const QString&)));

    connect (p, SIGNAL (register_doc_signal (const QString&)),
             this, SLOT (registerDoc (const QString&)));

    connect (p, SIGNAL (unregister_doc_signal (const QString&)),
             this, SLOT (unregisterDoc (const QString&)));
  }

  documentation_dock_widget::~documentation_dock_widget (void)
  {
    if (m_docs)
      delete m_docs;
  }

  void documentation_dock_widget::notice_settings (const QSettings *settings)
  {
    m_docs->notice_settings (settings);
  }

  void documentation_dock_widget::copyClipboard (void)
  {
    m_docs->copyClipboard ();
  }

  void documentation_dock_widget::pasteClipboard (void)
  {
    m_docs->pasteClipboard ();
  }

  void documentation_dock_widget::selectAll (void)
  {
    m_docs->selectAll ();
  }

  void documentation_dock_widget::showDoc (const QString& name)
  {
    // show the doc pane without focus for carrying on typing in the console
    if (! isVisible ())
      setVisible (true);

    raise ();

    m_docs->load_ref (name);
  }

  void documentation_dock_widget::registerDoc (const QString& name)
  {
    m_docs->registerDoc (name);
  }

  void documentation_dock_widget::unregisterDoc (const QString& name)
  {
    m_docs->unregisterDoc (name);
  }
}
