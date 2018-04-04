/*

Copyright (C) 2018 Torsten <mttl@maibox.org>

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

#include "defaults.h"
#include "file-ops.h"
#include "oct-env.h"

#include <QApplication>
#include <QCompleter>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QHelpContentWidget>
#include <QHelpIndexWidget>
#include <QHelpSearchEngine>
#include <QHelpSearchQueryWidget>
#include <QHelpSearchResultWidget>
#include <QLabel>
#include <QLineEdit>
#include <QMessageBox>
#include <QShortcut>
#include <QTabWidget>
#include <QToolButton>
#include <QVBoxLayout>

#include "documentation.h"
#include "resource-manager.h"

namespace octave
{
  // The documentation splitter, which is the main widget
  // of the doc dock widget
  documentation::documentation (QWidget *p)
    : QSplitter (Qt::Horizontal, p)
  {
    // Get original collection
    QString collection = getenv ("OCTAVE_QTHELP_COLLECTION");
    if (collection.isEmpty ())
      collection = QString::fromStdString (octave::config::oct_doc_dir ()
                                           + octave::sys::file_ops::dir_sep_str ()
                                           + "octave_interpreter.qhc");

    // Setup the help engine with the original collection, use a writable copy
    // of the original collection and load the help data
    m_help_engine = new QHelpEngine (collection, this);

    QString tmpdir = QDir::tempPath();
    m_collection
      = QString::fromStdString (octave::sys::tempnam (tmpdir.toStdString (),
                                                      "oct-qhelp-"));

    if (m_help_engine->copyCollectionFile (m_collection))
      m_help_engine->setCollectionFile (m_collection);
    else
      QMessageBox::warning (this, tr ("Octave Documentation"),
                            tr ("Could not copy help collection to temporary\n"
                                "file. Search capabilities may be affected.\n"
                                "%1").arg (m_help_engine->error ()));

    connect(m_help_engine, SIGNAL(setupFinished()),
            m_help_engine->searchEngine(), SLOT(indexDocumentation()));

    if (! m_help_engine->setupData())
      {
        QMessageBox::warning (this, tr ("Octave Documentation"),
                              tr ("Could not setup the data required for the\n"
                                  "documentation viewer. Only help texts in\n"
                                  "the Console Widget will be available."));
        if (m_help_engine)
          delete m_help_engine;
        m_help_engine = 0;
        return;
      }

    // The browser
    QWidget *browser_find = new QWidget (this);
    m_doc_browser = new documentation_browser (m_help_engine, browser_find);

    QWidget *find_footer = new QWidget (browser_find);
    QLabel *find_label = new QLabel (tr ("Find:"), find_footer);
    m_find_line_edit = new QLineEdit (find_footer);
    connect (m_find_line_edit, SIGNAL (returnPressed (void)),
             this, SLOT(find_forward (void)));
    QToolButton *forward_button = new QToolButton (find_footer);
    forward_button->setText (tr ("Search forward"));
    forward_button->setToolTip (tr ("Search forward"));
    forward_button->setIcon (resource_manager::icon ("go-down"));
    connect (forward_button, SIGNAL (pressed (void)),
             this, SLOT(find_forward (void)));
    QToolButton *backward_button = new QToolButton (find_footer);
    backward_button->setText (tr ("Search backward"));
    backward_button->setToolTip (tr ("Search backward"));
    backward_button->setIcon (resource_manager::icon ("go-up"));
    connect (backward_button, SIGNAL (pressed (void)),
             this, SLOT(find_backward (void)));
    QHBoxLayout *h_box_find_footer = new QHBoxLayout (find_footer);
    h_box_find_footer->addWidget (find_label);
    h_box_find_footer->addWidget (m_find_line_edit);
    h_box_find_footer->addWidget (forward_button);
    h_box_find_footer->addWidget (backward_button);
    h_box_find_footer->setMargin (2);
    find_footer->setLayout (h_box_find_footer);

    QVBoxLayout *v_box_browser_find = new QVBoxLayout (browser_find);
    v_box_browser_find->addWidget (m_doc_browser);
    v_box_browser_find->addWidget (find_footer);
    browser_find->setLayout (v_box_browser_find);

    QShortcut *shortcut = new QShortcut(QKeySequence(tr("Ctrl+F")), browser_find);
    shortcut->setContext(Qt::WidgetWithChildrenShortcut);
    connect (shortcut, SIGNAL (activated (void)),
             this, SLOT(toggle_hidden_find (void)));
    find_footer->hide ();

    // Layout contents, index and search
    QTabWidget *navi = new QTabWidget (this);
    navi->setTabsClosable (false);
    navi->setMovable (true);

    // Contents
    QHelpContentWidget *content = m_help_engine->contentWidget ();
    navi->addTab (content, tr ("Contents"));

    connect(m_help_engine->contentWidget (),
            SIGNAL (linkActivated (const QUrl&)),
            m_doc_browser, SLOT(handle_index_clicked (const QUrl&)));

    // Index
    QHelpIndexWidget *index = m_help_engine->indexWidget ();

    m_filter = new QComboBox (this);
    m_filter->setToolTip (tr ("Enter text to search the indices"));
    m_filter->setEditable (true);
    m_filter->setInsertPolicy (QComboBox::NoInsert);
    m_filter->setMaxCount (10);
    m_filter->setMaxVisibleItems (10);
    m_filter->setSizeAdjustPolicy (
      QComboBox::AdjustToMinimumContentsLengthWithIcon);
    QSizePolicy sizePol (QSizePolicy::Expanding, QSizePolicy::Preferred);
    m_filter->setSizePolicy (sizePol);
    m_filter->completer ()->setCaseSensitivity (Qt::CaseSensitive);
    QLabel *filter_label = new QLabel (tr ("Search"));

    QWidget *filter_all = new QWidget (navi);
    QHBoxLayout *h_box_index = new QHBoxLayout (filter_all);
    h_box_index->addWidget (filter_label);
    h_box_index->addWidget (m_filter);
    h_box_index->setMargin (2);
    filter_all->setLayout (h_box_index);

    QWidget *index_all = new QWidget (navi);
    QVBoxLayout *v_box_index = new QVBoxLayout (index_all);
    v_box_index->addWidget (filter_all);
    v_box_index->addWidget (index);
    index_all->setLayout (v_box_index);

    navi->addTab (index_all, tr ("Index"));

    connect(m_help_engine->indexWidget (),
            SIGNAL (linkActivated (const QUrl&, const QString&)),
            m_doc_browser, SLOT(handle_index_clicked (const QUrl&,
                                                      const QString&)));

    connect (m_filter, SIGNAL (editTextChanged (const QString&)),
             this, SLOT(filter_update (const QString&)));

    connect (m_filter->lineEdit (), SIGNAL (editingFinished (void)),
             this, SLOT(filter_update_history (void)));

    // Search
    QHelpSearchEngine *search_engine = m_help_engine->searchEngine ();
    QHelpSearchQueryWidget *search = search_engine->queryWidget ();
    QHelpSearchResultWidget *result = search_engine->resultWidget ();
    QWidget *search_all = new QWidget (navi);
    QVBoxLayout *v_box_search = new QVBoxLayout (search_all);
    v_box_search->addWidget (search);
    v_box_search->addWidget (result);
    search_all->setLayout (v_box_search);
    navi->addTab (search_all, tr ("Search"));

    connect (search, SIGNAL (search (void)),
             this, SLOT(global_search (void)));

    connect (search_engine, SIGNAL (searchingStarted (void)),
             this, SLOT(global_search_started (void)));
    connect (search_engine, SIGNAL (searchingFinished (int)),
             this, SLOT(global_search_finished (int)));

    connect (search_engine->resultWidget (),
             SIGNAL (requestShowLink (const QUrl&)),
             m_doc_browser,
             SLOT(handle_index_clicked (const QUrl&)));

    // Fill the splitter
    insertWidget (0, navi);
    insertWidget (1, browser_find);
    setStretchFactor (1, 1);

    // Initial view: Contents
    m_doc_browser->setSource (QUrl (
        "qthelp://org.octave.interpreter-1.0/doc/octave.html/index.html"));
  }

  documentation::~documentation (void)
  {
    if (m_help_engine)
      delete m_help_engine;

    // Cleanup temporary file and directory
    QFile file (m_collection);
    if (file.exists ())
      {
        QFileInfo finfo (file);
        QString bname = finfo.fileName ();
        QDir dir = finfo.absoluteDir ();
        dir.setFilter (QDir::Dirs | QDir::NoDotAndDotDot | QDir::Hidden);
        QStringList namefilter;
        namefilter.append ("*" + bname + "*");
        foreach (QFileInfo fi, dir.entryInfoList (namefilter))
          {
            std::string file_name = fi.absoluteFilePath ().toStdString ();
            sys::recursive_rmdir (file_name);
          }

        file.remove();
      }
  }

  void documentation::global_search (void)
  {
#if defined (HAVE_QHELPSEARCHQUERYWIDGET_SEARCHINPUT)
    QString queries
      = m_help_engine->searchEngine ()->queryWidget ()->searchInput ();
#else
    QList<QHelpSearchQuery> queries
      = m_help_engine->searchEngine ()->queryWidget ()->query ();
#endif

    m_help_engine->searchEngine ()->search (queries);
  }

  void documentation::global_search_started (void)
  {
    qApp->setOverrideCursor(QCursor(Qt::WaitCursor));
  }

  void documentation::global_search_finished (int)
  {
    qApp->restoreOverrideCursor();
  }

  void documentation::notice_settings (const QSettings *) { }

  void documentation::copyClipboard (void) { }

  void documentation::pasteClipboard (void) { }

  void documentation::selectAll (void) { }

  void documentation::load_ref (const QString& ref_name)
  {
    if (m_help_engine)
      {
        QMap<QString, QUrl> found_links
          = m_help_engine->linksForIdentifier (ref_name);
        if (found_links.count() > 0)
          m_doc_browser->setSource (found_links.constBegin().value());
      }
  }

  void documentation::filter_update (const QString& expression)
  {
    if (! m_help_engine)
      return;

    QString wildcard;
    if (expression.contains (QLatin1Char('*')))
      wildcard = expression;

    m_help_engine->indexWidget ()->filterIndices(expression, wildcard);
  }

  void documentation::filter_update_history (void)
  {
    QString text = m_filter->currentText ();   // get current text
    int index = m_filter->findText (text);     // and its actual index

    if (index > -1)
      m_filter->removeItem (index);            // remove if already existing

    m_filter->insertItem (0, text);            // (re)insert at beginning
    m_filter->setCurrentIndex (0);
  }

  void documentation::find_forward (void)
  {
    if (! m_help_engine)
      return;

    m_doc_browser->find (m_find_line_edit->text ());
  }

  void documentation::find_backward (void)
  {
    if (! m_help_engine)
      return;

    m_doc_browser->find (m_find_line_edit->text (), QTextDocument::FindBackward);
  }

  void documentation::toggle_hidden_find (void)
  {
    if (! m_help_engine)
      return;

    m_find_line_edit->parentWidget ()->
        setHidden (! m_find_line_edit->parentWidget ()->isHidden ());
    if (m_find_line_edit->parentWidget ()->isHidden ())
      m_doc_browser->setFocus ();
    else
      m_find_line_edit->setFocus ();
  }

  void documentation::registerDoc (const QString& qch)
  {
    if (m_help_engine)
      {
        QString ns = m_help_engine->namespaceName (qch);
        bool do_setup = true;
        if (m_help_engine->registeredDocumentations ().contains (ns))
          {
            if (m_help_engine->documentationFileName (ns) == qch)
              do_setup = false;
            else
              {
                m_help_engine->unregisterDocumentation (ns);
                m_help_engine->registerDocumentation (qch);
              }
          }
        else if (! m_help_engine->registerDocumentation (qch))
          {
            QMessageBox::warning (this, tr ("Octave Documentation"),
                                  tr ("Unable to register help file %1.").
                                  arg (qch));
            do_setup = false;
            return;
          }

        if (do_setup)
          m_help_engine->setupData();
      }
  }

  void documentation::unregisterDoc (const QString& qch)
  {
    QString ns = m_help_engine->namespaceName (qch);
    if (m_help_engine
        && m_help_engine->registeredDocumentations ().contains (ns)
        && m_help_engine->documentationFileName (ns) == qch)
      {
        m_help_engine->unregisterDocumentation (ns);
        m_help_engine->setupData ();
      }
  }


  // The documentation browser
  documentation_browser::documentation_browser (QHelpEngine *he, QWidget *p)
    : QTextBrowser (p), m_help_engine (he)
  { }

  documentation_browser::~documentation_browser (void)
  { }

  void documentation_browser::handle_index_clicked (const QUrl& url,
                                                    const QString&)
  {
    setSource (url);
  }

  void documentation_browser::notice_settings (const QSettings *)
  { }

  QVariant documentation_browser::loadResource (int type, const QUrl &url)
  {
    if (url.scheme () == "qthelp")
      return QVariant (m_help_engine->fileData(url));
    else
      return QTextBrowser::loadResource(type, url);
  }

}
