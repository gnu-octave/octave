// Copyright (C) 2016 The Qt Company Ltd.
// SPDX-License-Identifier: LicenseRef-Qt-Commercial OR LGPL-3.0-only OR GPL-2.0-only OR GPL-3.0-only

#include "qhelpsearchquerywidget.h"

#include <QtCore/QAbstractListModel>
#include <QtCore/QObject>
#include <QtCore/QStringList>
#include <QtCore/QtGlobal>

#include <QtWidgets/QCompleter>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLayout>
#include <QtWidgets/QLineEdit>
#include <QtGui/QFocusEvent>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QToolButton>

QT_BEGIN_NAMESPACE

class QHelpSearchQueryWidgetPrivate : public QObject
{
    Q_OBJECT

private:
    struct QueryHistory {
        explicit QueryHistory() : curQuery(-1) {}
        QStringList queries;
        int curQuery;
    };

    class CompleterModel : public QAbstractListModel
    {
    public:
        explicit CompleterModel(QObject *parent)
          : QAbstractListModel(parent) {}

        int rowCount(const QModelIndex &parent = QModelIndex()) const override
        {
            return parent.isValid() ? 0 : termList.size();
        }

        QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override
        {
            if (!index.isValid() || index.row() >= termList.size()||
                (role != Qt::EditRole && role != Qt::DisplayRole))
                return QVariant();
            return termList.at(index.row());
        }

        void addTerm(const QString &term)
        {
            if (!termList.contains(term)) {
                beginResetModel();
                termList.append(term);
                endResetModel();
            }
        }

    private:
        QStringList termList;
    };

    QHelpSearchQueryWidgetPrivate()
        : QObject()
        , m_searchCompleter(new CompleterModel(this), this)
    {
    }

    ~QHelpSearchQueryWidgetPrivate() override
    {
        // nothing todo
    }

    void retranslate()
    {
        m_searchLabel->setText(QHelpSearchQueryWidget::tr("Search for:"));
        m_searchButton->setText(QHelpSearchQueryWidget::tr("Search"));
#if QT_CONFIG(tooltip)
        m_prevQueryButton->setToolTip(QHelpSearchQueryWidget::tr("Previous search"));
        m_nextQueryButton->setToolTip(QHelpSearchQueryWidget::tr("Next search"));
#endif
    }

    void saveQuery(const QString &query)
    {
        // We only add the query to the list if it is different from the last one.
        if (!m_queries.queries.isEmpty() && m_queries.queries.last() == query)
            return;

        m_queries.queries.append(query);
        static_cast<CompleterModel *>(m_searchCompleter.model())->addTerm(query);
    }

    void nextOrPrevQuery(int maxOrMinIndex, int addend, QToolButton *thisButton,
        QToolButton *otherButton)
    {
        m_lineEdit->clear();

        // Otherwise, the respective button would be disabled.
        Q_ASSERT(m_queries.curQuery != maxOrMinIndex);

        m_queries.curQuery = qBound(0, m_queries.curQuery + addend, m_queries.queries.size() - 1);
        const QString &query = m_queries.queries.at(m_queries.curQuery);
        m_lineEdit->setText(query);

        if (m_queries.curQuery == maxOrMinIndex)
            thisButton->setEnabled(false);
        otherButton->setEnabled(true);
    }

    void enableOrDisableToolButtons()
    {
        m_prevQueryButton->setEnabled(m_queries.curQuery > 0);
        m_nextQueryButton->setEnabled(m_queries.curQuery
            < m_queries.queries.size() - 1);
    }

private slots:
    bool eventFilter(QObject *ob, QEvent *event) override
    {
        if (event->type() == QEvent::KeyPress) {
            QKeyEvent *const keyEvent = static_cast<QKeyEvent *>(event);
            if (keyEvent->key() == Qt::Key_Down) {
                if (m_queries.curQuery + 1 < m_queries.queries.size())
                    nextQuery();
                return true;
            }
            if (keyEvent->key() == Qt::Key_Up) {
                if (m_queries.curQuery > 0)
                    prevQuery();
                return true;
            }

        }
        return QObject::eventFilter(ob, event);
    }

    void searchRequested()
    {
        saveQuery(m_lineEdit->text());
        m_queries.curQuery = m_queries.queries.size() - 1;
        if (m_queries.curQuery > 0)
            m_prevQueryButton->setEnabled(true);
        m_nextQueryButton->setEnabled(false);
    }

    void nextQuery()
    {
        nextOrPrevQuery(m_queries.queries.size() - 1, 1, m_nextQueryButton,
                m_prevQueryButton);
    }

    void prevQuery()
    {
        nextOrPrevQuery(0, -1, m_prevQueryButton, m_nextQueryButton);
    }

private:
    friend class QHelpSearchQueryWidget;

    QLabel *m_searchLabel = nullptr;
    QPushButton *m_searchButton = nullptr;
    QLineEdit *m_lineEdit = nullptr;
    QToolButton *m_nextQueryButton = nullptr;
    QToolButton *m_prevQueryButton = nullptr;
    QueryHistory m_queries;
    QCompleter m_searchCompleter;
    bool m_compactMode = false;
};

/*!
    \class QHelpSearchQueryWidget
    \since 4.4
    \inmodule QtHelp
    \brief The QHelpSearchQueryWidget class provides a simple line edit or
    an advanced widget to enable the user to input a search term in a
    standardized input mask.
*/

/*!
    \fn void QHelpSearchQueryWidget::search()

    This signal is emitted when a the user has the search button invoked.
    After receiving the signal you can ask the QHelpSearchQueryWidget for the
    search input that you may pass to the QHelpSearchEngine::search() function.
*/

/*!
    Constructs a new search query widget with the given \a parent.
*/
QHelpSearchQueryWidget::QHelpSearchQueryWidget(QWidget *parent)
    : QWidget(parent)
{
    d = new QHelpSearchQueryWidgetPrivate();

    QVBoxLayout *vLayout = new QVBoxLayout(this);
    vLayout->setContentsMargins(QMargins());

    QHBoxLayout* hBoxLayout = new QHBoxLayout();
    d->m_searchLabel = new QLabel(this);
    d->m_lineEdit = new QLineEdit(this);
    d->m_lineEdit->setClearButtonEnabled(true);
    d->m_lineEdit->setCompleter(&d->m_searchCompleter);
    d->m_lineEdit->installEventFilter(d);
    d->m_prevQueryButton = new QToolButton(this);
    d->m_prevQueryButton->setArrowType(Qt::LeftArrow);
    d->m_prevQueryButton->setEnabled(false);
    d->m_nextQueryButton = new QToolButton(this);
    d->m_nextQueryButton->setArrowType(Qt::RightArrow);
    d->m_nextQueryButton->setEnabled(false);
    d->m_searchButton = new QPushButton(this);
    hBoxLayout->addWidget(d->m_searchLabel);
    hBoxLayout->addWidget(d->m_lineEdit);
    hBoxLayout->addWidget(d->m_prevQueryButton);
    hBoxLayout->addWidget(d->m_nextQueryButton);
    hBoxLayout->addWidget(d->m_searchButton);

    vLayout->addLayout(hBoxLayout);

    connect(d->m_prevQueryButton, &QAbstractButton::clicked,
            d, &QHelpSearchQueryWidgetPrivate::prevQuery);
    connect(d->m_nextQueryButton, &QAbstractButton::clicked,
            d, &QHelpSearchQueryWidgetPrivate::nextQuery);
    connect(d->m_searchButton, &QAbstractButton::clicked,
            this, &QHelpSearchQueryWidget::search);
    connect(d->m_lineEdit, &QLineEdit::returnPressed,
            this, &QHelpSearchQueryWidget::search);

    d->retranslate();
    connect(this, &QHelpSearchQueryWidget::search,
            d, &QHelpSearchQueryWidgetPrivate::searchRequested);
    setCompactMode(true);
}

/*!
    Destroys the search query widget.
*/
QHelpSearchQueryWidget::~QHelpSearchQueryWidget()
{
    delete d;
}

/*!
    Expands the search query widget so that the extended search fields are shown.
*/
void QHelpSearchQueryWidget::expandExtendedSearch()
{
    // TODO: no extended search anymore, deprecate it?
}

/*!
    Collapses the search query widget so that only the default search field is
    shown.
*/
void QHelpSearchQueryWidget::collapseExtendedSearch()
{
    // TODO: no extended search anymore, deprecate it?
}

#if QT_DEPRECATED_SINCE(5, 9)
/*!
    \deprecated

    Use searchInput() instead.
*/
QList<QHelpSearchQuery> QHelpSearchQueryWidget::query() const
{
    return QList<QHelpSearchQuery>() << QHelpSearchQuery(QHelpSearchQuery::DEFAULT,
           searchInput().split(QChar::Space, Qt::SkipEmptyParts));
}

/*!
    \deprecated

    Use setSearchInput() instead.
*/
void QHelpSearchQueryWidget::setQuery(const QList<QHelpSearchQuery> &queryList)
{
    if (queryList.isEmpty())
        return;

    setSearchInput(queryList.first().wordList.join(QChar::Space));
}
#endif // QT_DEPRECATED_SINCE(5, 9)

/*!
    \since 5.9

    Returns a search phrase to use in combination with the
    QHelpSearchEngine::search(const QString &searchInput) function.
*/
QString QHelpSearchQueryWidget::searchInput() const
{
    if (d->m_queries.queries.isEmpty())
        return QString();
    return d->m_queries.queries.last();
}

/*!
    \since 5.9

    Sets the QHelpSearchQueryWidget input field to the value specified by
    \a searchInput.

    \note The QHelpSearchEngine::search(const QString &searchInput) function has
    to be called to perform the actual search.
*/
void QHelpSearchQueryWidget::setSearchInput(const QString &searchInput)
{
    d->m_lineEdit->clear();

    d->m_lineEdit->setText(searchInput);

    d->searchRequested();
}

bool QHelpSearchQueryWidget::isCompactMode() const
{
    return d->m_compactMode;
}

void QHelpSearchQueryWidget::setCompactMode(bool on)
{
    if (d->m_compactMode != on) {
        d->m_compactMode = on;
        d->m_prevQueryButton->setVisible(!on);
        d->m_nextQueryButton->setVisible(!on);
        d->m_searchLabel->setVisible(!on);
    }
}

/*!
    \reimp
*/
void QHelpSearchQueryWidget::focusInEvent(QFocusEvent *focusEvent)
{
    if (focusEvent->reason() != Qt::MouseFocusReason) {
        d->m_lineEdit->selectAll();
        d->m_lineEdit->setFocus();
    }
}

/*!
    \reimp
*/
void QHelpSearchQueryWidget::changeEvent(QEvent *event)
{
    if (event->type() == QEvent::LanguageChange)
        d->retranslate();
    else
        QWidget::changeEvent(event);
}

QT_END_NAMESPACE

#include "qhelpsearchquerywidget.moc"
