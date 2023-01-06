////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2018-2023 The Octave Project Developers
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

#include <QAction>
#include <QApplication>
#include <QCompleter>
#include <QDesktopServices>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QFontDatabase>
#include <QHelpContentWidget>
#include <QHelpIndexWidget>
#if defined (HAVE_NEW_QHELPINDEXWIDGET_API) \
  || defined (HAVE_QHELPENGINE_DOCUMENTSFORIDENTIFIER)
#  include <QHelpLink>
#endif
#include <QHelpSearchEngine>
#include <QHelpSearchQueryWidget>
#include <QHelpSearchResultWidget>
#include <QLabel>
#include <QLineEdit>
#include <QMessageBox>
#include <QTabWidget>
#include <QTimer>
#include <QVBoxLayout>

#include "documentation.h"
#include "documentation-bookmarks.h"
#include "gui-preferences-global.h"
#include "gui-preferences-dc.h"
#include "gui-preferences-sc.h"
#include "octave-qobject.h"
#include "shortcut-manager.h"

#include "defaults.h"
#include "file-ops.h"
#include "oct-env.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// The documentation splitter, which is the main widget
// of the doc dock widget
documentation::documentation (QWidget *p, base_qobject& oct_qobj)
: QSplitter (Qt::Horizontal, p),
  m_octave_qobj (oct_qobj), m_doc_widget (this),
  m_tool_bar (new QToolBar (this)),
  m_query_string (QString ()),
  m_indexed (false),
  m_current_ref_name (QString ()),
  m_prev_pages_menu (new QMenu (this)),
  m_next_pages_menu (new QMenu (this)),
  m_prev_pages_count (0),
  m_next_pages_count (0),
  m_findnext_shortcut (new QShortcut (this)),
  m_findprev_shortcut (new QShortcut (this))
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

  // Mark help as readonly to avoid error if collection file is stored in a
  // readonly location
  m_help_engine->setProperty ("_q_readonly",
                              QVariant::fromValue<bool> (true));

  QString tmpdir = QString::fromStdString (sys::env::get_temp_directory ());
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

  connect(m_help_engine->searchEngine (), SIGNAL(indexingFinished ()),
          this, SLOT(load_index ()));
  connect(m_help_engine, SIGNAL(setupFinished ()),
          m_help_engine->searchEngine (), SLOT(reindexDocumentation ()));

  if (! m_help_engine->setupData())
    {
#ifdef ENABLE_DOCS
      QMessageBox::warning (this, tr ("Octave Documentation"),
                            tr ("Could not setup the data required for the\n"
                                "documentation viewer. Only help texts in\n"
                                "the Command Window will be available."));
#endif

      disconnect (m_help_engine, 0, 0, 0);

      delete m_help_engine;
      m_help_engine = nullptr;
    }

  // The browser
  QWidget *browser_find = new QWidget (this);
  m_doc_browser = new documentation_browser (m_help_engine, browser_find);
  connect (m_doc_browser, &documentation_browser::cursorPositionChanged,
           this, &documentation::handle_cursor_position_change);

  // Tool bar
  construct_tool_bar ();

  // Find bar
  QWidget *find_footer = new QWidget (browser_find);
  QLabel *find_label = new QLabel (tr ("Find:"), find_footer);
  m_find_line_edit = new QLineEdit (find_footer);
  connect (m_find_line_edit, &QLineEdit::returnPressed,
           this, [=] () { find (); });
  connect (m_find_line_edit, &QLineEdit::textEdited,
           this, &documentation::find_forward_from_anchor);
  QToolButton *forward_button = new QToolButton (find_footer);
  forward_button->setText (tr ("Search forward"));
  forward_button->setToolTip (tr ("Search forward"));
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  forward_button->setIcon (rmgr.icon ("go-down"));
  connect (forward_button, &QToolButton::pressed,
           this, [=] () { find (); });
  QToolButton *backward_button = new QToolButton (find_footer);
  backward_button->setText (tr ("Search backward"));
  backward_button->setToolTip (tr ("Search backward"));
  backward_button->setIcon (rmgr.icon ("go-up"));
  connect (backward_button, &QToolButton::pressed,
           this, &documentation::find_backward);
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

  notice_settings (rmgr.get_settings ());

  m_findnext_shortcut->setContext (Qt::WidgetWithChildrenShortcut);
  connect (m_findnext_shortcut, &QShortcut::activated,
           this, [=] () { find (); });
  m_findprev_shortcut->setContext (Qt::WidgetWithChildrenShortcut);
  connect (m_findprev_shortcut, &QShortcut::activated,
           this, &documentation::find_backward);

  find_footer->hide ();
  m_search_anchor_position = 0;

  if (m_help_engine)
    {
#if defined (HAVE_NEW_QHELPINDEXWIDGET_API)
      // Starting in Qt 5.15, help engine uses filters instead of old API
      m_help_engine->setUsesFilterEngine (true);
#endif
      // Layout contents, index and search
      QTabWidget *navi = new QTabWidget (this);
      navi->setTabsClosable (false);
      navi->setMovable (true);

      // Contents
      QHelpContentWidget *content = m_help_engine->contentWidget ();
      content->setObjectName ("documentation_tab_contents");
      navi->addTab (content, tr ("Contents"));

      connect (m_help_engine->contentWidget (),
               &QHelpContentWidget::linkActivated,
               m_doc_browser, [=] (const QUrl& url) {
                 m_doc_browser->handle_index_clicked (url); });

      // Index
      QHelpIndexWidget *index = m_help_engine->indexWidget ();

      m_filter = new QComboBox (this);
      m_filter->setToolTip (tr ("Enter text to search the indices"));
      m_filter->setEditable (true);
      m_filter->setInsertPolicy (QComboBox::NoInsert);
      m_filter->setMaxCount (10);
      m_filter->setMaxVisibleItems (10);
      m_filter->setSizeAdjustPolicy (QComboBox::AdjustToMinimumContentsLengthWithIcon);
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

#if defined (HAVE_NEW_QHELPINDEXWIDGET_API)
      connect (m_help_engine->indexWidget (),
               &QHelpIndexWidget::documentActivated,
               this, [=] (const QHelpLink &link) {
                 m_doc_browser->handle_index_clicked (link.url); });
#else
      connect (m_help_engine->indexWidget (),
               &QHelpIndexWidget::linkActivated,
               m_doc_browser, &documentation_browser::handle_index_clicked);
#endif

      connect (m_filter, &QComboBox::editTextChanged,
               this, &documentation::filter_update);

      connect (m_filter->lineEdit (), &QLineEdit::editingFinished,
               this, &documentation::filter_update_history);

      // Bookmarks (own class)
      m_bookmarks
        = new documentation_bookmarks (this, m_doc_browser, m_octave_qobj, navi);
      navi->addTab (m_bookmarks, tr ("Bookmarks"));

      connect (m_action_bookmark, &QAction::triggered,
               m_bookmarks, [=] () { m_bookmarks->add_bookmark (); });

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

      connect (search, &QHelpSearchQueryWidget::search,
               this, &documentation::global_search);

      connect (search_engine, &QHelpSearchEngine::searchingStarted,
               this, &documentation::global_search_started);
      connect (search_engine, &QHelpSearchEngine::searchingFinished,
               this, &documentation::global_search_finished);

      connect (search_engine->resultWidget (),
               &QHelpSearchResultWidget::requestShowLink,
               this, &documentation::handle_search_result_clicked);

      // Fill the splitter
      insertWidget (0, navi);
      insertWidget (1, browser_find);
      setStretchFactor (1, 1);
    }
}

documentation::~documentation (void)
{
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
      for (const auto& fi : dir.entryInfoList (namefilter))
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

  if (member)
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
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  m_action_go_home
    = add_action (rmgr.icon ("go-home"), tr ("Go home"), SLOT (home (void)),
                  m_doc_browser, m_tool_bar);

  m_action_go_prev
    = add_action (rmgr.icon ("go-previous"), tr ("Go back"),
                  SLOT (backward (void)), m_doc_browser, m_tool_bar);
  m_action_go_prev->setEnabled (false);

  // popdown menu with prev pages files
  QToolButton *popdown_button_prev_pages = new QToolButton ();
  popdown_button_prev_pages->setToolTip (tr ("Previous pages"));
  popdown_button_prev_pages->setMenu (m_prev_pages_menu);
  popdown_button_prev_pages->setPopupMode (QToolButton::InstantPopup);
  popdown_button_prev_pages->setToolButtonStyle (Qt::ToolButtonTextOnly);
  popdown_button_prev_pages->setCheckable (false);
  popdown_button_prev_pages->setArrowType(Qt::DownArrow);
  m_tool_bar->addWidget (popdown_button_prev_pages);

  m_action_go_next
    = add_action (rmgr.icon ("go-next"), tr ("Go forward"),
                  SLOT (forward (void)), m_doc_browser, m_tool_bar);
  m_action_go_next->setEnabled (false);

  // popdown menu with prev pages files
  QToolButton *popdown_button_next_pages = new QToolButton ();
  popdown_button_next_pages->setToolTip (tr ("Next pages"));
  popdown_button_next_pages->setMenu (m_next_pages_menu);
  popdown_button_next_pages->setPopupMode (QToolButton::InstantPopup);
  popdown_button_next_pages->setToolButtonStyle (Qt::ToolButtonTextOnly);
  popdown_button_next_pages->setArrowType(Qt::DownArrow);
  m_tool_bar->addWidget (popdown_button_next_pages);

  connect (m_doc_browser, &documentation_browser::backwardAvailable,
           m_action_go_prev, &QAction::setEnabled);
  connect (m_doc_browser, &documentation_browser::backwardAvailable,
           popdown_button_prev_pages, &QToolButton::setEnabled);
  connect (m_doc_browser, &documentation_browser::forwardAvailable,
           m_action_go_next, &QAction::setEnabled);
  connect (m_doc_browser, &documentation_browser::forwardAvailable,
           popdown_button_next_pages, &QToolButton::setEnabled);
  connect (m_doc_browser, &documentation_browser::historyChanged,
           this, &documentation::update_history_menus);

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

  connect (m_prev_pages_menu, &QMenu::triggered,
           this, &documentation::open_hist_url);
  connect (m_next_pages_menu, &QMenu::triggered,
           this, &documentation::open_hist_url);

  // Find
  m_tool_bar->addSeparator ();
  m_action_find
    = add_action (rmgr.icon ("edit-find"), tr ("Find"),
                  SLOT (activate_find (void)), this, m_tool_bar);

  // Zoom
  m_tool_bar->addSeparator ();
  m_action_zoom_in
    = add_action (rmgr.icon ("view-zoom-in"), tr ("Zoom in"),
                  SLOT (zoom_in (void)), m_doc_browser, m_tool_bar);
  m_action_zoom_out
    = add_action (rmgr.icon ("view-zoom-out"), tr ("Zoom out"),
                  SLOT (zoom_out (void)), m_doc_browser, m_tool_bar);
  m_action_zoom_original
    = add_action (rmgr.icon ("view-zoom-original"), tr ("Zoom original"),
                  SLOT (zoom_original (void)), m_doc_browser, m_tool_bar);

  // Bookmarks (connect slots later)
  m_tool_bar->addSeparator ();
  m_action_bookmark
    = add_action (rmgr.icon ("bookmark-new"), tr ("Bookmark current page"),
                  nullptr, nullptr, m_tool_bar);
}

void documentation::global_search (void)
{
  if (! m_help_engine)
    return;

  QString query_string;
#if defined (HAVE_QHELPSEARCHQUERYWIDGET_SEARCHINPUT)
  QString queries
    = m_help_engine->searchEngine ()->queryWidget ()->searchInput ();
  query_string = queries;
#else
  // FIXME: drop this part when support for Qt4 is dropped
  QList<QHelpSearchQuery> queries
    = m_help_engine->searchEngine ()->queryWidget ()->query ();
  if (queries.count ())
    query_string = queries.first ().wordList.join (" ");
  else
    query_string = "";
#endif

  if (query_string.isEmpty ())
    return;

  // Get quoted search strings first, then take first string as fall back
  QRegExp rx ("\"([^\"]*)\"");
  if (rx.indexIn (query_string, 0) != -1)
    m_internal_search = rx.cap (1);
  else
#if defined (HAVE_QT_SPLITBEHAVIOR_ENUM)
    m_internal_search = query_string.split (" ", Qt::SkipEmptyParts).first ();
#else
  m_internal_search = query_string.split (" ", QString::SkipEmptyParts).first ();
#endif

  m_help_engine->searchEngine ()->search (queries);
}

void documentation::global_search_started (void)
{
  qApp->setOverrideCursor(QCursor(Qt::WaitCursor));
}

void documentation::global_search_finished (int)
{
  if (! m_help_engine)
    return;

  if (! m_internal_search.isEmpty ())
    {
      m_query_string = m_internal_search;

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
                  connect (this, &documentation::show_single_result,
                           this, &documentation::handle_search_result_clicked);

                  emit show_single_result (url);
                }
            }
        }

      m_internal_search = QString ();
    }

  qApp->restoreOverrideCursor();
}

void documentation::handle_search_result_clicked (const QUrl& url)
{
  // Open url with matching text
  m_doc_browser->handle_index_clicked (url);

  // Select all occurrences of matching text
  select_all_occurrences (m_query_string);

  // Open search widget with matching text as search string
  m_find_line_edit->setText (m_query_string);
  m_find_line_edit->parentWidget ()->show ();

  // If no occurrence can be found go to the top of the page
  if (! m_doc_browser->find (m_find_line_edit->text ()))
    m_doc_browser->moveCursor (QTextCursor::Start);
  else
    {
      // Go to to first occurrence of search text.  Going to the end and then
      // search backwards until the last occurrence ensures the search text
      // is visible in the first line of the visible part of the text.
      m_doc_browser->moveCursor (QTextCursor::End);
      while (m_doc_browser->find (m_find_line_edit->text (),
                                  QTextDocument::FindBackward));
    }
}

void documentation::select_all_occurrences (const QString& text)
{
  // Get highlight background and text color
  QPalette pal = QApplication::palette ();
  QTextCharFormat format;
  QColor col = pal.color (QPalette::Highlight);
  col.setAlphaF (0.25);
  format.setBackground (QBrush (col));
  format.setForeground (QBrush (pal.color (QPalette::Text)));

  // Create list for extra selected items
  QList<QTextEdit::ExtraSelection> selected;
  m_doc_browser->moveCursor (QTextCursor::Start);

  // Find all occurrences and add them to the selection
  while ( m_doc_browser->find (text) )
    {
      QTextEdit::ExtraSelection selected_item;
      selected_item.cursor = m_doc_browser->textCursor ();
      selected_item.format = format;
      selected.append (selected_item);
    }

  // Apply selection and move back to the beginning
  m_doc_browser->setExtraSelections (selected);
  m_doc_browser->moveCursor (QTextCursor::Start);
}

void documentation::notice_settings (const gui_settings *settings)
{
  // If m_help_engine is not defined, the objects accessed by this method
  // are not valid.  Thus, just return in this case.
  if (! m_help_engine)
    return;

  // Icon size in the toolbar.
  int size_idx = settings->value (global_icon_size).toInt ();
  size_idx = (size_idx > 0) - (size_idx < 0) + 1;  // Make valid index from 0 to 2

  QStyle *st = style ();
  int icon_size = st->pixelMetric (global_icon_sizes[size_idx]);
  m_tool_bar->setIconSize (QSize (icon_size, icon_size));

  // Shortcuts
  shortcut_manager& scmgr = m_octave_qobj.get_shortcut_manager ();

  scmgr.set_shortcut (m_action_find, sc_edit_edit_find_replace);
  scmgr.shortcut (m_findnext_shortcut, sc_edit_edit_find_next);
  scmgr.shortcut (m_findprev_shortcut, sc_edit_edit_find_previous);
  scmgr.set_shortcut (m_action_zoom_in, sc_edit_view_zoom_in);
  scmgr.set_shortcut (m_action_zoom_out, sc_edit_view_zoom_out);
  scmgr.set_shortcut (m_action_zoom_original, sc_edit_view_zoom_normal);
  scmgr.set_shortcut (m_action_go_home, sc_doc_go_home);
  scmgr.set_shortcut (m_action_go_prev, sc_doc_go_back);
  scmgr.set_shortcut (m_action_go_next, sc_doc_go_next);
  scmgr.set_shortcut (m_action_bookmark, sc_doc_bookmark);

  // Settings for the browser
  m_doc_browser->notice_settings (settings);
}

void documentation::save_settings (void)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  m_doc_browser->save_settings (settings);
  m_bookmarks->save_settings (settings);
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

void documentation::load_index (void)
{
  m_indexed = true;

  // Show index if no other page is required.
  if (m_current_ref_name.isEmpty ())
    m_doc_browser->setSource
      (QUrl ("qthelp://org.octave.interpreter-1.0/doc/octave.html/index.html"));
  else
    load_ref (m_current_ref_name);

  m_help_engine->contentWidget ()->expandToDepth (0);
}

void documentation::load_ref (const QString& ref_name)
{
  if (! m_help_engine || ref_name.isEmpty ())
    return;

  m_current_ref_name = ref_name;

  if (! m_indexed)
    return;

#if defined (HAVE_QHELPENGINE_DOCUMENTSFORIDENTIFIER)
  QList<QHelpLink> found_links
    = m_help_engine->documentsForIdentifier (ref_name);
#else
  QMap<QString, QUrl> found_links
    = m_help_engine->linksForIdentifier (ref_name);
#endif

  QTabWidget *navi = static_cast<QTabWidget *> (widget (0));

  if (found_links.count() > 0)
    {
      // First search in the function index
#if defined (HAVE_QHELPENGINE_DOCUMENTSFORIDENTIFIER)
      QUrl first_url = found_links.constFirst().url;
#else
      QUrl first_url = found_links.constBegin().value ();
#endif

      m_doc_browser->setSource (first_url);

      // Switch to function index tab
      m_help_engine->indexWidget()->filterIndices (ref_name);
      QWidget *index_tab
        = navi->findChild<QWidget *> ("documentation_tab_index");
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
        = navi->findChild<QWidget *> ("documentation_tab_search");
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

void documentation::find_backward (void)
{
  find (true);
}

void documentation::find (bool backward)
{
  if (! m_help_engine)
    return;

  QTextDocument::FindFlags find_flags;
  if (backward)
    find_flags = QTextDocument::FindBackward;

  if (! m_doc_browser->find (m_find_line_edit->text (), find_flags))
    {
      // Nothing was found, restart search from the begin or end of text
      QTextCursor textcur = m_doc_browser->textCursor ();
      if (backward)
        textcur.movePosition (QTextCursor::End);
      else
        textcur.movePosition (QTextCursor::Start);
      m_doc_browser->setTextCursor (textcur);
      m_doc_browser->find (m_find_line_edit->text (), find_flags);
    }

  record_anchor_position ();
}

void documentation::find_forward_from_anchor (const QString& text)
{
  if (! m_help_engine)
    return;

  // Search from the current position
  QTextCursor textcur = m_doc_browser->textCursor ();
  textcur.setPosition (m_search_anchor_position);
  m_doc_browser->setTextCursor (textcur);

  if (! m_doc_browser->find (text))
    {
      // Nothing was found, restart search from the beginning
      textcur.movePosition (QTextCursor::Start);
      m_doc_browser->setTextCursor (textcur);
      m_doc_browser->find (text);
    }
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
          return;
        }

      if (do_setup)
        m_help_engine->setupData();
    }
}

void documentation::unregisterDoc (const QString& qch)
{
  if (! m_help_engine)
    return;

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
      QString title
        = title_and_anchor (m_doc_browser->historyTitle (prev_next*(i+1)),
                            m_doc_browser->historyUrl (prev_next*(i+1)));

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

// Utility functions

QString documentation::title_and_anchor (const QString& title, const QUrl& url)
{
  QString retval = title;
  QString u = url.toString ();

  retval.remove (QRegExp ("\\s*\\(*GNU Octave \\(version [^\\)]*\\)[: \\)]*"));

  // Since the title only contains the section name and not the
  // specific anchor, extract the latter from the url and append
  // it to the title
  if (u.contains ('#'))
    {
      // Get the anchor from the url
      QString anchor = u.split ('#').last ();
      // Remove internal string parts
      anchor.remove (QRegExp ("^index-"));
      anchor.remove (QRegExp ("^SEC_"));
      anchor.remove (QRegExp ("^XREF"));
      anchor.remove ("Concept-Index_cp_letter-");
      anchor.replace ("-", " ");

      // replace encoded special chars by their unencoded versions
      QRegExp rx = QRegExp ("_00([0-7][0-9a-f])");
      int pos = 0;
      while ((pos = rx.indexIn(anchor, pos)) != -1)
        {
          anchor.replace ("_00"+rx.cap (1), QChar (rx.cap (1).toInt (nullptr, 16)));
          pos += rx.matchedLength();
        }

      if (retval != anchor)
        retval = retval + ": " + anchor;
    }

  return retval;
}

//
// The documentation browser
//

documentation_browser::documentation_browser (QHelpEngine *he, QWidget *p)
  : QTextBrowser (p), m_help_engine (he), m_zoom_level (max_zoom_level+1)
{
  setOpenLinks (false);
  connect (this, &documentation_browser::anchorClicked,
           this, [=] (const QUrl& url) { handle_index_clicked (url); });

  // Make sure we have access to one of the monospace fonts listed in
  // octave.css for rendering formated code blocks
  QStringList fonts = {"Fantasque Sans Mono", "FreeMono", "Courier New",
    "Cousine", "Courier"};

  bool load_default_font = true;

  for (int i = 0; i < fonts.size (); ++i)
    {
      QFont font (fonts.at (i));
      if (font.exactMatch ())
        {
          load_default_font = false;
          break;
        }
    }

  if (load_default_font)
    {
      QString fonts_dir =
        QString::fromStdString (sys::env::getenv ("OCTAVE_FONTS_DIR")
                                + sys::file_ops::dir_sep_str ());

      QStringList default_fonts = {"FreeMono", "FreeMonoBold",
        "FreeMonoBoldOblique", "FreeMonoOblique"};

      for (int i = 0; i < default_fonts.size (); ++i)
        {
          QString fontpath =
            fonts_dir + default_fonts.at(i) + QString (".otf");
          QFontDatabase::addApplicationFont (fontpath);
        }
    }
}

void documentation_browser::handle_index_clicked (const QUrl& url,
                                                  const QString&)
{
  if (url.scheme () == "qthelp")
    setSource (url);
  else
    QDesktopServices::openUrl (url);
}

void documentation_browser::notice_settings (const gui_settings *settings)
{
  // Zoom level only at startup, not when other settings have changed
  if (m_zoom_level > max_zoom_level)
    {
      m_zoom_level = settings->value (dc_browser_zoom_level).toInt ();
      zoomIn (m_zoom_level);
    }
}

QVariant documentation_browser::loadResource (int type, const QUrl& url)
{
  if (m_help_engine && url.scheme () == "qthelp")
    return QVariant (m_help_engine->fileData(url));
  else
    return QTextBrowser::loadResource(type, url);
}

void documentation_browser::save_settings (gui_settings *settings)
{
  settings->setValue (dc_browser_zoom_level.key, m_zoom_level);

  settings->sync ();
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
#if defined (HAVE_QWHEELEVENT_ANGLEDELTA)
      if (we->angleDelta().y () > 0)
#else
        if (we->delta() > 0)
#endif
          zoom_in ();
        else
          zoom_out ();

      we->accept ();
    }
  else
    QTextEdit::wheelEvent (we);
}

OCTAVE_END_NAMESPACE(octave)
