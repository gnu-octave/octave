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

#include <QAction>
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
#include <QTabWidget>
#include <QVBoxLayout>

#include "documentation.h"
#include "resource-manager.h"
#include "shortcut-manager.h"
#include "gui-preferences.h"

namespace octave
{
  // The documentation splitter, which is the main widget
  // of the doc dock widget
  documentation::documentation (QWidget *p)
    : QSplitter (Qt::Horizontal, p),
      m_doc_widget (p),
      m_tool_bar (new QToolBar (p)),
      m_prev_pages_menu (new QMenu (p)),
      m_next_pages_menu (new QMenu (p)),
      m_prev_pages_count (0),
      m_next_pages_count (0),
      m_findnext_shortcut (new QShortcut (p)),
      m_findprev_shortcut (new QShortcut (p))
  {
    // Get original collection
    QString collection = getenv ("OCTAVE_QTHELP_COLLECTION");
    if (collection.isEmpty ())
      collection = QString::fromStdString (config::oct_doc_dir ()
                                           + sys::file_ops::dir_sep_str ()
                                           + "octave_interpreter.qhc");

    // Setup the help engine with the original collection, use a writable copy
    // of the original collection and load the help data
    m_help_engine = new QHelpEngine (collection, this);

    QString tmpdir = QDir::tempPath();
    m_collection
      = QString::fromStdString (sys::tempnam (tmpdir.toStdString (),
                                              "oct-qhelp-"));

    if (m_help_engine->copyCollectionFile (m_collection))
      m_help_engine->setCollectionFile (m_collection);
    else
#ifdef ENABLE_DOCS
      // FIXME: Perhaps a better way to do this would be to keep a count
      // in the GUI preferences file.  After issuing this warning 3 times
      // it would be disabled.  The count would need to be reset when a new
      // version of Octave is installed.
      QMessageBox::warning (this, tr ("Octave Documentation"),
                            tr ("Could not copy help collection to temporary\n"
                                "file. Search capabilities may be affected.\n"
                                "%1").arg (m_help_engine->error ()));
#endif

    connect(m_help_engine, SIGNAL(setupFinished()),
            m_help_engine->searchEngine(), SLOT(indexDocumentation()));

    if (! m_help_engine->setupData())
      {
#ifdef ENABLE_DOCS
        QMessageBox::warning (this, tr ("Octave Documentation"),
                              tr ("Could not setup the data required for the\n"
                                  "documentation viewer. Only help texts in\n"
                                  "the Command Window will be available."));
#endif
        if (m_help_engine)
          delete m_help_engine;
        m_help_engine = 0;
        return;
      }

    // The browser
    QWidget *browser_find = new QWidget (this);
    m_doc_browser = new documentation_browser (m_help_engine, browser_find);
    connect (m_doc_browser, SIGNAL (cursorPositionChanged (void)),
             this, SLOT(handle_cursor_position_change (void)));

    // Tool bar
    construct_tool_bar ();

    // Find bar
    QWidget *find_footer = new QWidget (browser_find);
    QLabel *find_label = new QLabel (tr ("Find:"), find_footer);
    m_find_line_edit = new QLineEdit (find_footer);
    connect (m_find_line_edit, SIGNAL (returnPressed (void)),
             this, SLOT(find_forward (void)));
    connect (m_find_line_edit, SIGNAL (textEdited (const QString&)),
             this, SLOT(find_forward_from_anchor (const QString&)));
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
    v_box_browser_find->addWidget (m_tool_bar);
    v_box_browser_find->addWidget (m_doc_browser);
    v_box_browser_find->addWidget (find_footer);
    browser_find->setLayout (v_box_browser_find);

    notice_settings (resource_manager::get_settings ());

    m_findnext_shortcut->setContext (Qt::WidgetWithChildrenShortcut);
    connect (m_findnext_shortcut, SIGNAL (activated (void)),
             this, SLOT(find_forward (void)));
    m_findprev_shortcut->setContext (Qt::WidgetWithChildrenShortcut);
    connect (m_findprev_shortcut, SIGNAL (activated (void)),
             this, SLOT(find_backward (void)));

    find_footer->hide ();
    m_search_anchor_position = 0;

    // Layout contents, index and search
    QTabWidget *navi = new QTabWidget (this);
    navi->setTabsClosable (false);
    navi->setMovable (true);

    // Contents
    QHelpContentWidget *content = m_help_engine->contentWidget ();
    content->setObjectName ("documentation_tab_contents");
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
    index_all->setObjectName ("documentation_tab_index");
    QVBoxLayout *v_box_index = new QVBoxLayout (index_all);
    v_box_index->addWidget (filter_all);
    v_box_index->addWidget (index);
    index_all->setLayout (v_box_index);

    navi->addTab (index_all, tr ("Function Index"));

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
    search_all->setObjectName ("documentation_tab_search");
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

  QAction * documentation::add_action (const QIcon& icon, const QString& text,
                                       const char *member, QWidget *receiver,
                                       QToolBar *tool_bar)
  {
    QAction *a;
    QWidget *r = this;
    if (receiver != nullptr)
      r = receiver;

    a = new QAction (icon, text, this);
    connect (a, SIGNAL (triggered ()), r, member);

    if (tool_bar)
      tool_bar->addAction (a);

    m_doc_widget->addAction (a);  // important for shortcut context
    a->setShortcutContext (Qt::WidgetWithChildrenShortcut);

    return a;
  }

  void documentation::construct_tool_bar (void)
  {
    // Home, Previous, Next
    m_action_go_home = add_action (resource_manager::icon ("go-home"),
                                   tr ("Go home"), SLOT (home (void)),
                                   m_doc_browser, m_tool_bar);
    m_action_go_prev = add_action (resource_manager::icon ("go-previous"),
                                   tr ("Go back"), SLOT (backward (void)),
                                   m_doc_browser, m_tool_bar);
    m_action_go_prev->setEnabled (false);

    // popdown menu with prev pages files
    QToolButton *popdown_button_prev_pages = new QToolButton ();
    popdown_button_prev_pages->setToolTip (tr ("Previous Pages"));
    popdown_button_prev_pages->setMenu (m_prev_pages_menu);
    popdown_button_prev_pages->setPopupMode (QToolButton::InstantPopup);
    popdown_button_prev_pages->setToolButtonStyle (Qt::ToolButtonTextOnly);
    popdown_button_prev_pages->setCheckable (false);
    popdown_button_prev_pages->setArrowType(Qt::DownArrow);
    m_tool_bar->addWidget (popdown_button_prev_pages);
    m_action_go_next = add_action (resource_manager::icon ("go-next"),
                                   tr ("Go forward"), SLOT (forward (void)),
                                   m_doc_browser, m_tool_bar);
    m_action_go_next->setEnabled (false);

    // popdown menu with prev pages files
    QToolButton *popdown_button_next_pages = new QToolButton ();
    popdown_button_next_pages->setToolTip (tr ("Next Pages"));
    popdown_button_next_pages->setMenu (m_next_pages_menu);
    popdown_button_next_pages->setPopupMode (QToolButton::InstantPopup);
    popdown_button_next_pages->setToolButtonStyle (Qt::ToolButtonTextOnly);
    popdown_button_next_pages->setArrowType(Qt::DownArrow);
    m_tool_bar->addWidget (popdown_button_next_pages);

    connect (m_doc_browser, SIGNAL (backwardAvailable (bool)),
             m_action_go_prev, SLOT (setEnabled (bool)));
    connect (m_doc_browser, SIGNAL (backwardAvailable (bool)),
             popdown_button_prev_pages, SLOT (setEnabled (bool)));
    connect (m_doc_browser, SIGNAL (forwardAvailable (bool)),
             m_action_go_next, SLOT (setEnabled (bool)));
    connect (m_doc_browser, SIGNAL (forwardAvailable (bool)),
             popdown_button_next_pages, SLOT (setEnabled (bool)));
    connect (m_doc_browser, SIGNAL (historyChanged (void)),
             this, SLOT (update_history_menus (void)));

    // Init prev/next menus
    for (int i = 0; i < max_history_entries; ++i)
      {
        m_prev_pages_actions[i] = new QAction (this);
        m_prev_pages_actions[i]->setVisible (false);
        m_next_pages_actions[i] = new QAction (this);
        m_next_pages_actions[i]->setVisible (false);
        m_prev_pages_menu->addAction (m_prev_pages_actions[i]);
        m_next_pages_menu->addAction (m_next_pages_actions[i]);
      }

    connect (m_prev_pages_menu, SIGNAL (triggered (QAction *)),
             this, SLOT (open_hist_url (QAction *)));
    connect (m_next_pages_menu, SIGNAL (triggered (QAction *)),
             this, SLOT (open_hist_url (QAction *)));

    // Find
    m_tool_bar->addSeparator ();
    m_action_find = add_action (resource_manager::icon ("edit-find"),
                                   tr ("Find"), SLOT (activate_find (void)),
                                   this, m_tool_bar);

    // Zoom
    m_tool_bar->addSeparator ();
    m_action_zoom_in = add_action (resource_manager::icon ("zoom-in"),
                                   tr ("Zoom in"), SLOT (zoom_in (void)),
                                   m_doc_browser, m_tool_bar);
    m_action_zoom_out = add_action (resource_manager::icon ("zoom-out"),
                                    tr ("Zoom out"), SLOT (zoom_out (void)),
                                    m_doc_browser, m_tool_bar);
    m_action_zoom_original = add_action (resource_manager::icon ("zoom-original"),
                                   tr ("Zoom original"), SLOT (zoom_original (void)),
                                   m_doc_browser, m_tool_bar);
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
    if (! m_internal_search.isEmpty ())
      {
        QHelpSearchEngine *search_engine = m_help_engine->searchEngine ();
        if (search_engine)
          {
#if defined (HAVE_QHELPSEARCHQUERYWIDGET_SEARCHINPUT)
            QVector<QHelpSearchResult> res
              = search_engine->searchResults (0, search_engine->searchResultCount ());
#else
            QList< QPair<QString, QString> > res
              = search_engine->hits (0, search_engine->hitCount ());
#endif

            if (res.count ())
              {
                QUrl url;

                if (res.count () == 1)
#if defined (HAVE_QHELPSEARCHQUERYWIDGET_SEARCHINPUT)
                  url = res.front ().url ();
#else
                  url = res.front ().first;
#endif
                else
                  {
                    // Remove the quotes we added
                    QString search_string = m_internal_search;

                    for (auto r = res.begin (); r != res.end (); r++)
                      {
#if defined (HAVE_QHELPSEARCHQUERYWIDGET_SEARCHINPUT)
                        QString title = r->title ().toLower ();
                        QUrl tmpurl = r->url ();
#else
                        QString title = r->second.toLower ();
                        QUrl tmpurl = r->first;
#endif
                        if (title.contains (search_string.toLower ()))
                          {
                            if (title.indexOf (search_string.toLower ()) == 0)
                              {
                                url = tmpurl;
                                break;
                              }
                            else if (url.isEmpty ())
                              url = tmpurl;
                          }
                      }
                  }

                if (! url.isEmpty ())
                  {
                    connect (this, SIGNAL (show_single_result (const QUrl)),
                             m_doc_browser,
                             SLOT (handle_index_clicked (const QUrl)));

                    emit show_single_result (url);
                  }
              }
           }

        m_internal_search = QString ();
      }

    qApp->restoreOverrideCursor();
  }

  void documentation::notice_settings (const QSettings *settings)
  {
    // Icon size in the toolbar.
    int size_idx = settings->value (global_icon_size.key,
                                    global_icon_size.def).toInt ();
    size_idx = (size_idx > 0) - (size_idx < 0) + 1;  // Make valid index from 0 to 2

    QStyle *st = style ();
    int icon_size = st->pixelMetric (global_icon_sizes[size_idx]);
    m_tool_bar->setIconSize (QSize (icon_size, icon_size));

    // Shortcuts
    shortcut_manager::set_shortcut (m_action_find, "editor_edit:find_replace");
    shortcut_manager::shortcut (m_findnext_shortcut, "editor_edit:find_next");
    shortcut_manager::shortcut (m_findprev_shortcut, "editor_edit:find_previous");
    shortcut_manager::set_shortcut (m_action_zoom_in, "editor_view:zoom_in");
    shortcut_manager::set_shortcut (m_action_zoom_out, "editor_view:zoom_out");
    shortcut_manager::set_shortcut (m_action_zoom_original, "editor_view:zoom_normal");
    shortcut_manager::set_shortcut (m_action_go_home, "doc_browser:go_home");
    shortcut_manager::set_shortcut (m_action_go_prev, "doc_browser:go_back");
    shortcut_manager::set_shortcut (m_action_go_next, "doc_browser:go_next");
  }

  void documentation::copyClipboard (void)
  {
    if (m_doc_browser->hasFocus ())
      {
        m_doc_browser->copy();
      }
  }

  void documentation::pasteClipboard (void) { }

  void documentation::selectAll (void) { }

  void documentation::load_ref (const QString& ref_name)
  {
    if (! m_help_engine)
      return;

    // First search in the function index
    QMap<QString, QUrl> found_links
      = m_help_engine->linksForIdentifier (ref_name);

    QTabWidget *navi = static_cast<QTabWidget*> (widget (0));

    if (found_links.count() > 0)
      {
        m_doc_browser->setSource (found_links.constBegin().value());

        // Switch to function index tab
        m_help_engine->indexWidget()->filterIndices (ref_name);
        QWidget *index_tab
          = navi->findChild<QWidget*> ("documentation_tab_index");
        navi->setCurrentWidget (index_tab);
      }
    else
      {
        // Use full text search to provide the best match
        QHelpSearchEngine *search_engine = m_help_engine->searchEngine ();
        QHelpSearchQueryWidget *search_query = search_engine->queryWidget ();

#if defined (HAVE_QHELPSEARCHQUERYWIDGET_SEARCHINPUT)
        QString query = ref_name;
        query.prepend ("\"").append ("\"");
#else
        QList<QHelpSearchQuery> query;
        query << QHelpSearchQuery (QHelpSearchQuery::DEFAULT,
                                   QStringList (QString("\"") + ref_name + QString("\"")));
#endif
        m_internal_search = ref_name;
        search_engine->search (query);

        // Switch to search tab
#if defined (HAVE_QHELPSEARCHQUERYWIDGET_SEARCHINPUT)
        search_query->setSearchInput (query);
#else
        search_query->setQuery (query);
#endif
        QWidget *search_tab
          = navi->findChild<QWidget*> ("documentation_tab_search");
        navi->setCurrentWidget (search_tab);
      }
  }

  void documentation::activate_find (void)
  {
    if (m_find_line_edit->parentWidget ()->isVisible ())
      {
        m_find_line_edit->parentWidget ()->hide ();
        m_doc_browser->setFocus ();
      }
    else
      {
        m_find_line_edit->parentWidget ()->show ();
        m_find_line_edit->selectAll ();
        m_find_line_edit->setFocus ();
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
    record_anchor_position ();
  }

  void documentation::find_backward (void)
  {
    if (! m_help_engine)
      return;

    m_doc_browser->find (m_find_line_edit->text (), QTextDocument::FindBackward);
    record_anchor_position ();
  }

  void documentation::find_forward_from_anchor (const QString& text)
  {
    if (! m_help_engine)
      return;

    QTextCursor textcur = m_doc_browser->textCursor ();
    textcur.setPosition (m_search_anchor_position);
    m_doc_browser->setTextCursor (textcur);
    m_doc_browser->find (text);
  }

  void documentation::record_anchor_position (void)
  {
    if (! m_help_engine)
      return;

    m_search_anchor_position = m_doc_browser->textCursor ().position ();
  }

  void documentation::handle_cursor_position_change (void)
  {
    if (! m_help_engine)
      return;

    if (m_doc_browser->hasFocus ())
      record_anchor_position ();
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

  void documentation::update_history_menus (void)
  {
    if (m_prev_pages_count != m_doc_browser->backwardHistoryCount ())
      {
        update_history (m_doc_browser->backwardHistoryCount (),
                        m_prev_pages_actions);
        m_prev_pages_count = m_doc_browser->backwardHistoryCount ();
      }

    if (m_next_pages_count != m_doc_browser->forwardHistoryCount ())
      {
        update_history (m_doc_browser->forwardHistoryCount (),
                        m_next_pages_actions);
        m_next_pages_count = m_doc_browser->forwardHistoryCount ();
      }
  }

  void documentation::update_history (int new_count, QAction **actions)
  {
    // Which menu has to be updated?
    int prev_next = -1;
    QAction *a = m_action_go_prev;
    if (actions == m_next_pages_actions)
      {
        prev_next = 1;
        a = m_action_go_next;
      }

    // Get maximal count limited by array size
    int count = qMin (new_count, int (max_history_entries));

    // Fill used menu entries
    for (int i = 0; i < count; i++)
      {
        QString title = m_doc_browser->historyTitle (prev_next*(i+1));
        title.remove (QRegExp ("\\s*\\(*GNU Octave \\(version [^\\)]*\\)[: \\)]*"));

        if (i == 0)
          a->setText (title); // set tool tip for prev/next buttons

        actions[i]->setText (title);
        actions[i]->setData (m_doc_browser->historyUrl (prev_next*(i+1)));
        actions[i]->setEnabled (true);
        actions[i]->setVisible (true);
      }

    // Hide unused menu entries
    for (int j = count; j < max_history_entries; j++)
      {
        actions[j]->setEnabled (false);
        actions[j]->setVisible (false);
      }
  }

  void documentation::open_hist_url (QAction *a)
  {
    m_doc_browser->setSource (a->data ().toUrl ());
  }


  // The documentation browser
  documentation_browser::documentation_browser (QHelpEngine *he, QWidget *p)
    : QTextBrowser (p), m_help_engine (he), m_zoom_level (0)
  {
    setOpenLinks (false);
    connect (this, SIGNAL (anchorClicked (QUrl)),
             this, SLOT (handle_index_clicked (QUrl)));
  }

  documentation_browser::~documentation_browser (void)
  { }

  void documentation_browser::handle_index_clicked (const QUrl& url,
                                                    const QString&)
  {
    if (url.scheme () == "qthelp")
      setSource (url);
    else
      QDesktopServices::openUrl (url);
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

  void documentation_browser::zoom_in (void)
  {
    if (m_zoom_level < max_zoom_level)
      {
        zoomIn ();
        m_zoom_level++;
      }
  }

  void documentation_browser::zoom_out (void)
  {
    if (m_zoom_level > min_zoom_level)
      {
        zoomOut ();
        m_zoom_level--;
      }
  }

  void documentation_browser::zoom_original (void)
  {
    zoomIn (- m_zoom_level);
    m_zoom_level = 0;
  }

  void documentation_browser::wheelEvent (QWheelEvent *we)
  {
    if (we->modifiers () == Qt::ControlModifier)
      {
        if (we->delta () > 0)
          zoom_in ();
        else
          zoom_out ();

        we->accept ();
      }
    else
      QTextEdit::wheelEvent (we);
  }

}
