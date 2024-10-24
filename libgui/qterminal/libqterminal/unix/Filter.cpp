/*
    Copyright (C) 2007, 2013 by Robert Knight <robertknight@gmail.com>

    Rewritten for QT4 by e_k <e_k at users.sourceforge.net>, Copyright (C)2008
    Adoption to octave by Torsten <mttl@mailbox.org>, Copyright (c) 2017

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
    02110-1301  USA.
*/

// Own
#include "unix/Filter.h"

// Qt
#include <QtCore>

#include <QAction>
#include <QApplication>
#include <QClipboard>
#include <QDesktopServices>
#include <QSharedData>
#include <QString>

// Konsole
#include "unix/TerminalCharacterDecoder.h"

FilterChain::~FilterChain()
{
    QMutableListIterator<Filter*> iter(*this);

    while ( iter.hasNext() )
    {
        Filter* filter = iter.next();
        iter.remove();
        delete filter;
    }
}

void FilterChain::addFilter(Filter* filter)
{
    append(filter);
}
void FilterChain::removeFilter(Filter* filter)
{
    removeAll(filter);
}
bool FilterChain::containsFilter(Filter* filter)
{
    return contains(filter);
}
void FilterChain::reset()
{
    QListIterator<Filter*> iter(*this);
    while (iter.hasNext())
        iter.next()->reset();
}
void FilterChain::setBuffer(const QString* buffer , const QList<int>* linePositions)
{
    QListIterator<Filter*> iter(*this);
    while (iter.hasNext())
        iter.next()->setBuffer(buffer,linePositions);
}
void FilterChain::process()
{
    QListIterator<Filter*> iter(*this);
    while (iter.hasNext())
        iter.next()->process();
}
void FilterChain::clear()
{
    QList<Filter*>::clear();
}
Filter::HotSpot* FilterChain::hotSpotAt(int line , int column) const
{
    QListIterator<Filter*> iter(*this);
    while (iter.hasNext())
    {
        Filter* filter = iter.next();
        Filter::HotSpot* spot = filter->hotSpotAt(line,column);
        if ( spot != nullptr )
        {
            return spot;
        }
    }

    return nullptr;
}

QList<Filter::HotSpot*> FilterChain::hotSpots() const
{
    QList<Filter::HotSpot*> list;
    QListIterator<Filter*> iter(*this);
    while (iter.hasNext())
    {
        Filter* filter = iter.next();
        list << filter->hotSpots();
    }
    return list;
}

TerminalImageFilterChain::TerminalImageFilterChain()
: _buffer(nullptr)
, _linePositions(nullptr)
{
}

TerminalImageFilterChain::~TerminalImageFilterChain()
{
    delete _buffer;
    delete _linePositions;
}

void TerminalImageFilterChain::setImage(const Character* const image , int lines , int columns, const QVector<LineProperty>& lineProperties)
{
//qDebug("%s %d", __FILE__, __LINE__);
    if (empty())
        return;
//qDebug("%s %d", __FILE__, __LINE__);

    // reset all filters and hotspots
    reset();
//qDebug("%s %d", __FILE__, __LINE__);

    PlainTextDecoder decoder;
    decoder.setTrailingWhitespace(false);

//qDebug("%s %d", __FILE__, __LINE__);
    // setup new shared buffers for the filters to process on
    QString* newBuffer = new QString();
    QList<int>* newLinePositions = new QList<int>();
    setBuffer( newBuffer , newLinePositions );

    // free the old buffers
    delete _buffer;
    delete _linePositions;

    _buffer = newBuffer;
    _linePositions = newLinePositions;

    QTextStream lineStream(_buffer);
    decoder.begin(&lineStream);

    for (int i=0 ; i < lines ; i++)
    {
        _linePositions->append(_buffer->length());
        decoder.decodeLine(image + i*columns,columns,LINE_DEFAULT);

        // pretend that each line ends with a newline character.
        // this prevents a link that occurs at the end of one line
        // being treated as part of a link that occurs at the start of the next line
        //
        // the downside is that links which are spread over more than one line are not
        // highlighted.
        //
        // TODO - Use the "line wrapped" attribute associated with lines in a
        // terminal image to avoid adding this imaginary character for wrapped
        // lines
        if ( !(lineProperties.value(i,LINE_DEFAULT) & LINE_WRAPPED) )
        	lineStream << QChar('\n');
    }
    decoder.end();
//    qDebug("%s %d", __FILE__, __LINE__);
}

Filter::Filter() :
_linePositions(nullptr),
_buffer(nullptr)
{
}

Filter::~Filter()
{
    QListIterator<HotSpot*> iter(_hotspotList);
    while (iter.hasNext())
    {
        delete iter.next();
    }
}
void Filter::reset()
{
    _hotspots.clear();
    _hotspotList.clear();
}

void Filter::setBuffer(const QString* buffer , const QList<int>* linePositions)
{
    _buffer = buffer;
    _linePositions = linePositions;
}

void Filter::getLineColumn(int position , int& startLine , int& startColumn)
{
    Q_ASSERT( _linePositions );
    Q_ASSERT( _buffer );


    for (int i = 0 ; i < _linePositions->count() ; i++)
    {
        //kDebug() << "line position at " << i << " = " << _linePositions[i];
        int nextLine = 0;

        if ( i == _linePositions->count()-1 )
        {
            nextLine = _buffer->length() + 1;
        }
        else
        {
            nextLine = _linePositions->value(i+1);
        }

       // kDebug() << "pos - " << position << " line pos(" << i<< ") " << _linePositions->value(i) <<
       //     " next = " << nextLine << " buffer len = " << _buffer->length();

        if ( _linePositions->value(i) <= position && position < nextLine )
        {
            startLine = i;
            startColumn = position - _linePositions->value(i);
            return;
        }
    }
}


/*void Filter::addLine(const QString& text)
{
    _linePositions << _buffer.length();
    _buffer.append(text);
}*/

const QString* Filter::buffer()
{
    return _buffer;
}
Filter::HotSpot::~HotSpot()
{
}
void Filter::addHotSpot(HotSpot* spot)
{
    _hotspotList << spot;

    for (int line = spot->startLine() ; line <= spot->endLine() ; line++)
    {
        _hotspots.insert(line,spot);
    }
}
QList<Filter::HotSpot*> Filter::hotSpots() const
{
    return _hotspotList;
}
QList<Filter::HotSpot*> Filter::hotSpotsAtLine(int line) const
{
    return _hotspots.values(line);
}

Filter::HotSpot* Filter::hotSpotAt(int line , int column) const
{
    QListIterator<HotSpot*> spotIter(_hotspots.values(line));

    while (spotIter.hasNext())
    {
        HotSpot* spot = spotIter.next();

        if ( spot->startLine() == line && spot->startColumn() > column )
            continue;
        if ( spot->endLine() == line && spot->endColumn() < column )
            continue;

        return spot;
    }

    return nullptr;
}

Filter::HotSpot::HotSpot(int startLine , int startColumn , int endLine , int endColumn)
    : _startLine(startLine)
    , _startColumn(startColumn)
    , _endLine(endLine)
    , _endColumn(endColumn)
    , _type(NotSpecified)
{
}
QString Filter::HotSpot::tooltip() const
{
    return QString();
}
QList<QAction*> Filter::HotSpot::actions()
{
    return QList<QAction*>();
}
int Filter::HotSpot::startLine() const
{
    return _startLine;
}
int Filter::HotSpot::endLine() const
{
    return _endLine;
}
int Filter::HotSpot::startColumn() const
{
    return _startColumn;
}
int Filter::HotSpot::endColumn() const
{
    return _endColumn;
}
Filter::Type Filter::HotSpot::type() const
{
    return _type;
}
void Filter::HotSpot::setType(Filter::Type type)
{
    _type = type;
}

RegExpFilter::RegExpFilter(Type t)
    : _type (t)
{
}

RegExpFilter::HotSpot::HotSpot(int startLine,int startColumn,
                               int endLine,int endColumn, Filter::Type t)
    : Filter::HotSpot(startLine,startColumn,endLine,endColumn)
{
    setType(t);
}

void RegExpFilter::HotSpot::activate(QObject*)
{
}

void RegExpFilter::HotSpot::setCapturedTexts(const QStringList& texts)
{
    _capturedTexts = texts;
}
QStringList RegExpFilter::HotSpot::capturedTexts() const
{
    return _capturedTexts;
}

void RegExpFilter::setRegExp (const QRegularExpression& regExp)
{
    _searchText = regExp;
}
QRegularExpression RegExpFilter::regExp () const
{
    return _searchText;
}
/*void RegExpFilter::reset(int)
{
    _buffer = QString();
}*/
void RegExpFilter::process ()
{
    int pos = 0;
    const QString* text = buffer();

    Q_ASSERT( text );

    // ignore any regular expressions which match an empty string.
    // otherwise the while loop below will run indefinitely
    static const QString emptyString("");
    QRegularExpressionMatch match = _searchText.match (emptyString);
    if ( match.hasMatch () )
        return;

    match = _searchText.match (*text, pos);
    while (match.hasMatch ())
    {
        pos = match.capturedStart ();
        int startLine = 0;
        int endLine = 0;
        int startColumn = 0;
        int endColumn = 0;

        //kDebug() << "pos from " << pos << " to " << pos + _searchText.matchedLength();

        getLineColumn (match.capturedStart (), startLine, startColumn);
        getLineColumn (match.capturedEnd (), endLine, endColumn);

        RegExpFilter::HotSpot* spot = newHotSpot(startLine,startColumn,
                                       endLine,endColumn,_type);
        spot->setCapturedTexts (match.capturedTexts ());

        addHotSpot( spot );
        pos += match.capturedLength ();

        // if matchedLength == 0, the program will get stuck in an infinite loop
        Q_ASSERT( match.capturedLength () > 0 );

        match = _searchText.match (*text, pos);
    }
}

RegExpFilter::HotSpot* RegExpFilter::newHotSpot(int startLine,int startColumn,
                                                int endLine,int endColumn,
                                                Filter::Type t)
{
    return new RegExpFilter::HotSpot(startLine,startColumn,
                                     endLine,endColumn, t);
}
UrlFilter::HotSpot* UrlFilter::newHotSpot(int startLine,int startColumn,int endLine,
                                                    int endColumn, Filter::Type t)
{
    return new UrlFilter::HotSpot(startLine,startColumn,
                                               endLine,endColumn,t);
}

void UrlFilter::process ()
{
    int pos = 0;
    const QString* text = buffer();

    Q_ASSERT( text );

    // ignore any regular expressions which match an empty string.
    // otherwise the while loop below will run indefinitely
    static const QString emptyString("");
    QRegularExpressionMatch match = _searchText.match (emptyString);
    if ( match.hasMatch () )
        return;

    match = _searchText.match (*text, pos);
    while (match.hasMatch ())
    {
        int startLine = 0;
        int endLine = 0;
        int startColumn = 0;
        int endColumn = 0;

        //kDebug() << "pos from " << pos << " to " << pos + _searchText.matchedLength();

        getLineColumn (match.capturedStart (), startLine, startColumn);
        getLineColumn (match.capturedEnd (), endLine, endColumn);

        UrlFilter::HotSpot* spot = newHotSpot(startLine,startColumn,
                                              endLine,endColumn,_type);
        spot->setCapturedTexts(match.capturedTexts ());

        // Connect the signal of the urlobject to the slot of the filter;
        // the filter is then signaling to the main window
        connect (spot->get_urlObject (),
                 SIGNAL (request_open_file_signal (const QString&, int)),
                 this, SLOT (request_open_file (const QString&, int)));

        addHotSpot( spot );
        pos += match.capturedLength();

        // if matchedLength == 0, the program will get stuck in an infinite loop
        Q_ASSERT( match.capturedLength () > 0 );

        match = _searchText.match (*text, pos);
    }
}

UrlFilter::HotSpot::HotSpot(int startLine,int startColumn,
                            int endLine,int endColumn,Type t)
: RegExpFilter::HotSpot(startLine,startColumn,endLine,endColumn,t)
, _urlObject(new FilterObject(this))
{
}

QString UrlFilter::HotSpot::tooltip() const
{
    QString url = capturedTexts().first();

    const UrlType kind = urlType();

    if ( kind == StandardUrl )
        return QString();
    else if ( kind == Email )
        return QString();
    else
        return QString();
}
UrlFilter::HotSpot::UrlType UrlFilter::HotSpot::urlType () const
{
    QString url = capturedTexts().first();

    QRegularExpressionMatch match = FullUrlRegExp.match (url);
    if ( match.hasMatch () )
        return StandardUrl;
    match = EmailAddressRegExp.match (url);
    if ( match.hasMatch () )
        return Email;
    match = ErrorLinkRegExp.match (url);
    if ( match.hasMatch () )
        return ErrorLink;
    match = ParseErrorLinkRegExp.match (url);
    if ( match.hasMatch () )
        return ParseErrorLink;

    return Unknown;
}

void UrlFilter::HotSpot::activate (QObject* object)
{
    QString url = capturedTexts().first();

    const UrlType kind = urlType();

    const QString& actionName = object ? object->objectName() : QString();

    if ( actionName == "copy-action" )
    {
        //kDebug() << "Copying url to clipboard:" << url;

        QApplication::clipboard()->setText(url);
        return;
    }

    if ( !object || actionName == "open-action" )
      {
        if ( kind == StandardUrl )
          {
            // if the URL path does not include the protocol ( eg. "www.kde.org" ) then
            // prepend http:// ( eg. "www.kde.org" --> "http://www.kde.org" )
            if (!url.contains("://"))
              url.prepend("http://");
            QDesktopServices::openUrl (QUrl (url));
          }
        else if ( kind == Email )
          {
            url.prepend("mailto:");
            QDesktopServices::openUrl (QUrl (url));
          }
        else if (kind == ErrorLink)
          {
            QRegularExpressionMatch match = ErrorLinkRegExp.match (url);
            if (match.hasMatch ())
              {
                QString file_name = match.captured (1);
                QString line = match.captured (2);
                // call the urlobject's method for opening a file; this
                // method then signals to the filter
                _urlObject->request_open_file (file_name, line.toInt ());
              }
          }
        else if (kind == ParseErrorLink)
          {
            QRegularExpressionMatch match = ParseErrorLinkRegExp.match (url);
            if (match.hasMatch ())
              {
                QString line = match.captured (1);
                QString file_name = match.captured (2);
                // call the urlobject's method for opening a file; this
                // method then signals to the filter
                _urlObject->request_open_file (file_name, line.toInt ());
              }
          }


      }

}

// Note:  Altering these regular expressions can have a major effect on the performance of the filters
// used for finding URLs in the text, especially if they are very general and could match very long
// pieces of text.
// Please be careful when altering them.

//regexp matches:
// full url:
// protocolname:// or www. followed by anything other than whitespaces, <, >, ' or ", and ends before whitespaces, <, >, ', ", ], !, comma and dot
const QRegularExpression
UrlFilter::FullUrlRegExp {"(www\\.(?!\\.)|[a-z][a-z0-9+.-]*://)[^\\s<>'\"]+[^!,\\.\\s<>'\"\\]]"};
// email address:
// [word chars, dots or dashes]@[word chars, dots or dashes].[word chars]
const QRegularExpression
UrlFilter::EmailAddressRegExp {"\\b(\\w|\\.|-)+@(\\w|\\.|-)+\\.\\w+\\b"};
// matches full url or email address
const QRegularExpression
UrlFilter::CompleteUrlRegExp {'(' + FullUrlRegExp.pattern() + '|' +
                              EmailAddressRegExp.pattern() + ')'};
// error link:
//   normal error
const QRegularExpression
UrlFilter::ErrorLinkRegExp {"(\\S+) at line (\\d+) column (?:\\d+)"};
//   parse error
const QRegularExpression
UrlFilter::ParseErrorLinkRegExp {"parse error near line (\\d+) of file (\\S+)"};
//   complete regexp
const QRegularExpression
UrlFilter::CompleteErrorLinkRegExp {'('+ErrorLinkRegExp.pattern ()+'|'+
                                    ParseErrorLinkRegExp.pattern ()+')'};


UrlFilter::UrlFilter (Type t)
    : RegExpFilter (t)
{
    if (_type == ErrorLink)
      setRegExp (CompleteErrorLinkRegExp);
    else
      setRegExp (CompleteUrlRegExp);
}
UrlFilter::HotSpot::~HotSpot()
{
    delete _urlObject;
}
void FilterObject::activated()
{
    _filter->activate(sender());
}
QList<QAction*> UrlFilter::HotSpot::actions ()
{
    QList<QAction*> list;

    const UrlType kind = urlType();

    QAction* openAction = new QAction(_urlObject);
    QAction* copyAction = new QAction(_urlObject);

    Q_ASSERT (kind == StandardUrl || kind == Email
                                  || kind == ErrorLink
                                  || kind == ParseErrorLink);

    if ( kind == StandardUrl )
    {
        openAction->setText(tr ("Open Link"));
        copyAction->setText(tr ("Copy Link Address"));
    }
    else if ( kind == Email )
    {
        openAction->setText(tr ("Send Email To..."));
        copyAction->setText(tr ("Copy Email Address"));
    }
    else if ( kind == ErrorLink )
    {
      QString url = capturedTexts().first();
      QRegularExpressionMatch match = ErrorLinkRegExp.match (url);
      if (match.hasMatch ())
        {
          QString file_name = match.captured (1);
          QString line = match.captured (2);
          openAction->setText(tr ("Edit %1 at line %2")
                              .arg (file_name).arg (line));
        }
    }
    else if ( kind == ParseErrorLink )
    {
      QString url = capturedTexts().first();
      QRegularExpressionMatch match = ParseErrorLinkRegExp.match (url);
      if (match.hasMatch ())
        {
          QString line = match.captured (1);
          QString file_name = match.captured (2);
          openAction->setText(tr ("Edit %1 at line %2")
                              .arg (file_name).arg (line));
        }
    }

    // object names are set here so that the hotspot performs the
    // correct action when activated() is called with the triggered
    // action passed as a parameter.
    openAction->setObjectName("open-action");
    copyAction->setObjectName("copy-action");

    QObject::connect( openAction , SIGNAL(triggered()) , _urlObject , SLOT(activated()) );
    list << openAction;

    if (kind != ErrorLink && kind != ParseErrorLink)
      {
        QObject::connect ( copyAction , SIGNAL(triggered()) ,
                           _urlObject , SLOT(activated()) );
        list << copyAction;
      }
    return list;
}

void
UrlFilter::request_open_file (const QString& file, int line)
{
  QFileInfo file_info = QFileInfo (file);

  // We have to distinguish between a parse error, where we get the full
  // path of the file or a general error in a script, where we only get
  // the function name. depending on this we have to invoke different
  // slots in main_window
  if (file_info.isAbsolute () && file_info.exists ())
    Q_EMIT request_open_file_signal (file, QString (), line);
  else
    Q_EMIT request_edit_mfile_signal (file, line);
}

//#include "moc_Filter.cpp"
