/****************************************************************************
**
** Copyright (C) 2006-2009 fullmetalcoder <fullmetalcoder@hotmail.fr>
**
** This file is part of the Edyuk project <http://edyuk.org>
** 
** This file may be used under the terms of the GNU General Public License
** version 3 as published by the Free Software Foundation and appearing in the
** file GPL.txt included in the packaging of this file.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
****************************************************************************/

#include "qeditsession.h"

/*!
	\file qeditsession.cpp
	\brief Implementation of the QEditSession class.
*/

#include "qeditor.h"
#include "qdocument.h"
#include "qdocument_p.h"
#include "qdocumentline.h"
#include "qdocumentcursor.h"

#include "qlinemarksinfocenter.h"

#include <QFile>
#include <QFileInfo>
#include <QDataStream>
#include <QScrollBar>

/*!
	\class QEditSession
	
	\brief A session recording class
	
	The purpose of this class is to collect session data from several QEditor object,
	to serialize it and to re-create the same session by deserializing the stored data.
*/

/*!
	\brief ctor
*/
QEditSession::QEditSession(QObject *p)
 : QObject(p), m_id(-1), m_delay(0)
{
	
}

/*!
	\brief ctor
*/
QEditSession::QEditSession(const QString& f, QObject *p)
 : QObject(p), m_id(-1), m_delay(0)
{
	setFileName(f);
}

/*!
	\brief dtor
*/
QEditSession::~QEditSession()
{
	
}

/*!
	\return The update interval, in milliseconds
	
	A value of zero means the data is NOT automatically updated.
	
	\see updateData()
*/
int QEditSession::autoUpdateInterval() const
{
	return m_delay;
}

/*!
	\brief Set the update interval
	
	If \a ms is strictly positive then the data will be
	updated every \a ms milliseconds
	
	If the session has been given a valid filename, the updated
	data will automatically be saved to that file.
*/
void QEditSession::setAutoUpdateInterval(int ms)
{
	if ( m_delay )
	{
		killTimer(m_id);
		m_id = -1;
	}
	
	m_delay = ms;
	
	if ( m_delay )
	{
		m_id = startTimer(m_delay);
	}
}

/*!
	\return The file name used as storage
	
	If it is empty then no auto-save is performed.
*/
QString QEditSession::fileName() const
{
	return m_fileName;
}

/*!
	\brief Set the storage destination
	
	Every time the data is updated, either when the autoupdate timer
	ticks or when updateData() is called programmatically, it will be
	written to that file, provided the filename is valid and points
	to a writeable location.
	
	\see updateData()
	\see autoUpdateInterval()
*/
void QEditSession::setFileName(const QString& filename, bool r)
{
	m_fileName = filename;
	
	if ( r )
		restore();
	
}

/*!
	\brief Add an editor to the session
*/
void QEditSession::addEditor(QEditor *e)
{
	if ( m_editors.contains(e) )
		return;
	
	//qDebug("+ 0x%x", e);
	
	Document *d = new Document;
	
	m_editors << e;
	m_sessionData << d;
	
	connect(e	, SIGNAL( destroyed(QObject*) ),
			this, SLOT  ( destroyed(QObject*) ) );
	
	connect(e	, SIGNAL( saved(QEditor*, QString) ),
			this, SLOT  ( saved(QEditor*, QString) ) );
	
	connect(e	, SIGNAL( loaded(QEditor*, QString) ),
			this, SLOT  ( loaded(QEditor*, QString) ) );
	
	update(e, d);
}

/*!
	\brief Remove an editor from the session
*/
void QEditSession::removeEditor(QEditor *e)
{
	int idx = m_editors.indexOf(e);
	
	if ( idx == -1 )
		return;
	
	//qDebug("- 0x%x", e);
	
	disconnect(	e	, SIGNAL( destroyed(QObject*) ),
				this, SLOT  ( destroyed(QObject*) ) );
	
	disconnect(	e	, SIGNAL( saved(QEditor*, QString) ),
				this, SLOT  ( saved(QEditor*, QString) ) );
	
	disconnect(	e	, SIGNAL( loaded(QEditor*, QString) ),
				this, SLOT  ( loaded(QEditor*, QString) ) );
	
	m_editors.removeAt(idx);
	delete m_sessionData.takeAt(idx);
}

/*!
	
*/
void QEditSession::clear(bool cleanup)
{
	if ( cleanup )
		qDeleteAll(m_editors);
	
	qDeleteAll(m_sessionData);
	
	m_editors.clear();
	m_sessionData.clear();
}

/*!
	\brief Serialize session data
*/
void QEditSession::save()
{
	QFile f(m_fileName);
	
	if ( f.open(QFile::WriteOnly) )
	{
		QDataStream s(&f);
		
		save(s);
	}
}

/*!
	\brief Serialize session data
*/
void QEditSession::restore()
{
	QFile f(m_fileName);
	
	if ( f.open(QFile::ReadOnly) )
	{
		QDataStream s(&f);
		
		restore(s);
	}
}

static const char _magic[] = "QES ";

/*!
	\brief Serialize session data
*/
void QEditSession::save(QDataStream& s)
{
	//qDebug("saving");
	
	s << *(reinterpret_cast<const quint32*>(_magic));
	s << m_sessionData.count();
	
	foreach ( Document *d, m_sessionData )
	{
		//qDebug("> %s", qPrintable(d->fileName));
		
		s << d->fileName;
		s << d->timeStamp;
		
		s << d->cursors.count();
		
		foreach ( const Cursor& c, d->cursors )
			s << c.beginLine << c.beginColumn << c.endLine << c.endColumn;
		
		s << d->marks.count();
		
		QHash<int, QList<int> >::const_iterator it = d->marks.constBegin();
		const QHash<int, QList<int> >::const_iterator end = d->marks.constEnd();
		
		while ( it != end )
		{
			s << it.key() << *it;
		}
		
		s << d->scrollX;
		s << d->scrollY;
	}
}

/*!
	\brief Deserialize session data
*/
void QEditSession::restore(QDataStream& s)
{
	//qDebug("restoring");
	
	quint32 magic;
	
	s >> magic;
	
	if ( magic != *(reinterpret_cast<const quint32*>(_magic)) )
	{
		qDebug("header mismatch : %i, %i", magic, s.status());
		return;
	}
	
	int documentCount = 0;
	
	s >> documentCount;
	
	for ( int i = 0; i < documentCount; ++i )
	{
		Document *d = new Document;
		
		s >> d->fileName;
		s >> d->timeStamp;
		
		//qDebug("> %s", qPrintable(d->fileName));
		
		int cursorCount = 0;
		
		s >> cursorCount;
		
		bool exist = QFile::exists(d->fileName);
		QEditor *e = exist ? createEditor() : 0;
		
		if ( e )
			e->load(d->fileName);
		
		for ( int j = 0; j < cursorCount; ++j )
		{
			Cursor c;
			
			s >> c.beginLine;
			s >> c.beginColumn;
			s >> c.endLine;
			s >> c.endColumn;
			
			d->cursors << c;
		}
		
		int markCount = 0;
		s >> markCount;
		
		for ( int j = 0; j < markCount; ++j )
		{
			int line = 0;
			QList<int> marks;
			
			s >> line;
			s >> marks;
			
			d->marks[line] = marks;
			
			foreach ( int mark, marks )
				e->document()->line(line).addMark(mark);
		}
		
		s >> d->scrollX;
		s >> d->scrollY;
		
		if ( e && cursorCount )
		{
			QDocumentCursor c = d->cursors.first().toDocumentCursor(e->document());
			
			e->setCursor(c);
			
			for ( int j = 1; j < cursorCount; ++j )
			{
				e->addCursorMirror(d->cursors.at(j).toDocumentCursor(e->document()));
			}
		}
		
		// TODO : defer. it does not seem to work properly that way
		// TODO : view size independency (store the first visible line number)
		e->verticalScrollBar()->setValue(d->scrollY);
		e->horizontalScrollBar()->setValue(d->scrollX);
		
		if ( e )
		{
			connect(e	, SIGNAL( destroyed(QObject*) ),
					this, SLOT  ( destroyed(QObject*) ) );
			
			connect(e	, SIGNAL( saved(QEditor*, QString) ),
					this, SLOT  ( saved(QEditor*, QString) ) );
			
			connect(e	, SIGNAL( loaded(QEditor*, QString) ),
					this, SLOT  ( loaded(QEditor*, QString) ) );
			
			m_editors << e;
			m_sessionData << d;
			
			emit restored(e);
		} else {
			delete d;
		}
	}
}

/*!
	\brief Updates the data
	
	Fetches up-to-date session data from the attached editors.
	
	If the session has been given a valid filename the data will
	automatically be saved.
	
	\note This will NOT affect the automatic updates timing
*/
void QEditSession::updateData()
{
	for ( int i = 0; i < m_editors.count(); ++i )
	{
		QEditor *e = m_editors.at(i);
		Document *d = m_sessionData.at(i);
		
		update(e, d);
	}
	
	save();
}

void QEditSession::destroyed(QObject *o)
{
	//qDebug("~ 0x%x", o);
	
	for ( int i = 0; i < m_editors.count(); ++i )
	{
		QEditor *e = m_editors.at(i);
		
		if ( !e || ((QObject*)e == o) )
		{
			delete m_sessionData.takeAt(i);
			m_editors.removeAt(i);
			break;
		}
	}
}

/*!
	\brief Called whenever an editor is saved
	
	This handler is responsible for updating file names and time stamps
	which is needed to avoid data loss upon session restoration
*/
void QEditSession::saved(QEditor *e, const QString& fn)
{
	int idx = m_editors.indexOf(e);
	
	if ( idx == -1 )
		return;
	
	//qDebug("saved : %s", qPrintable(fn));
	
	Document *d = m_sessionData.at(idx);
	
	//d->timeStamp = QDateTime::currentDateTime();
	
	update(e, d);
}

/*!
	\brief Called whenever an editor is loaded with new content
	
	This handler is responsible for updating file names and time stamps
	which is needed to avoid data loss upon session restoration
*/
void QEditSession::loaded(QEditor *e, const QString& fn)
{
	int idx = m_editors.indexOf(e);
	
	if ( idx == -1 )
		return;
	
	//qDebug("loaded : %s", qPrintable(fn));
	
	Document *d = m_sessionData.at(idx);
	
	//d->timeStamp = QDateTime::currentDateTime();
	
	update(e, d);
}

void QEditSession::update(QEditor *e, Document *d)
{
	if ( !e || !d )
		return;
	
	//qDebug(">>%s", qPrintable(e->fileName()));
	
	d->fileName = e->fileName();
	d->timeStamp = QFileInfo(d->fileName).lastModified();
	
	d->cursors.clear();
	d->cursors << Cursor(e->cursor());
	
	for ( int i = 0; i < e->cursorMirrorCount(); ++i )
		d->cursors << Cursor(e->cursorMirror(i));
	
	QLineMarkList marks = QLineMarksInfoCenter::instance()->marks(d->fileName);
	
	foreach ( const QLineMark& mark, marks )
	{
		d->marks[mark.line] << mark.mark;
	}
	
	d->scrollX = e->verticalScrollBar()->value();
	d->scrollY = e->horizontalScrollBar()->value();
}

/*!
	\internal
*/
void QEditSession::timerEvent(QTimerEvent *e)
{
	if ( e->timerId() == m_id )
	{
		updateData();
	}
}

QEditor* QEditSession::createEditor()
{
	return new QEditor;
}

QEditSession::Cursor::Cursor(const QDocumentCursor& c)
{
	beginLine = c.lineNumber();
	beginColumn = c.columnNumber();
	endLine = c.hasSelection() ? c.anchorLineNumber() : -1;
	endColumn = c.hasSelection() ? c.anchorColumnNumber() : -1;
	
	//qDebug("((%i, %i), (%i, %i))", beginLine, beginColumn, endLine, endColumn);
}

QDocumentCursor QEditSession::Cursor::toDocumentCursor(QDocument *d) const
{
	//qDebug("((%i, %i), (%i, %i))", beginLine, beginColumn, endLine, endColumn);
	
	QDocumentCursor beg(d, beginLine, beginColumn);
	QDocumentCursor end(d, endLine, endColumn);
	
	if ( endLine != -1 )
	{
		end.setSelectionBoundary(beg);
		return end;
	}
	
	return beg;
}
