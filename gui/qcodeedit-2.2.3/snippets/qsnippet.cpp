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

/*!
	\file qsnippet.cpp
	\brief Implementation of the builtin snippet types and loaders
*/

#include "qsnippet_p.h"

#include <QMap>

/*!
	\class QSnippet
	\brief The base class for snippets
*/

/*!
	\class QSnippetPatternLoader
	\brief The base class for snippet loaders
*/

QSnippetInsertionCommand::QSnippetInsertionCommand(QEditor *e)
 : QDocumentCommandBlock(e->document()), m_editor(e), m_cursor(e->cursor())
{
}

QSnippetInsertionCommand::~QSnippetInsertionCommand()
{
	foreach ( const QEditor::PlaceHolder& ph, m_placeHolders )
		delete ph.affector;
}

void QSnippetInsertionCommand::addPlaceHolder(const QEditor::PlaceHolder& ph)
{
	m_placeHolders << ph;
}

void QSnippetInsertionCommand::addCommand(QDocumentCommand *c)
{
	c->setTargetCursor(m_cursor.handle());
	QDocumentCommandBlock::addCommand(c);
}

void QSnippetInsertionCommand::removeCommand(QDocumentCommand *c)
{
	c->setTargetCursor(0);
	QDocumentCommandBlock::removeCommand(c);
}

void QSnippetInsertionCommand::redo()
{
	m_editor->clearPlaceHolders();
	QDocumentCommandBlock::redo();

	foreach ( const QEditor::PlaceHolder& ph, m_placeHolders )
		m_editor->addPlaceHolder(ph);
	
	m_editor->nextPlaceHolder();
}

void QSnippetInsertionCommand::undo()
{
	// TODO : backup and restore previous placeholders?
	m_editor->clearPlaceHolders();
	QDocumentCommandBlock::undo();
	m_editor->setCursor(m_cursor);
}

//

bool QCE::Snippets::PlainText::loadSnippet(QSnippet *snip, const QString& pattern)
{
	PlainText *target = dynamic_cast<PlainText*>(snip);
	
	if ( !target )
	{
		qWarning("snippet/loader type mismatch.");
		return false;
	}
	
	target->m_data = pattern;
	
	return true;
}

void QCE::Snippets::PlainText::insert(QEditor *e) const
{
	/*
	QDocumentCursor c = e->cursor();
	c.insertText(m_data);
	e->setCursor(c);
	*/
	
	e->write(m_data);
}

//

QString parsePlaceHolder(const QString& s, int& index, int max, QMap<int, QCE::Snippets::Simple::PlaceHolder>& p, int& line, int& column, int baseSize)
{
	QChar c;
	QStringList segments;
	int i = index, depth = 1, last = index + 1;
	
	while ( i + 1 < max )
	{
		c = s.at(++i);
		
		if ( c == QLatin1Char('{') )
		{
			++depth;
		} else if ( c == QLatin1Char('}') ) {
			--depth;
			
			if ( !depth )
			{
				segments << s.mid(last, i - last);
				break;
			}
		} else if ( c == QLatin1Char(':') ) {
			if ( depth == 1 )
			{
				segments << s.mid(last, i - last);
				last = i + 1;
			}
		}
	}
	
	if ( segments.isEmpty() )
	{
		qWarning("invalid placeholder");
		return QString();
	}
	
	int id = segments.at(0).toInt();
	
	QCE::Snippets::Simple::PlaceHolder& ph = p[id];
	
	if ( ph.length == -1 && segments.count() > 1 )
	{
		// new placeholder
		ph.length = segments.last().count();
		ph.lineOffset = line;
		ph.columnOffset = column;
		ph.defaultValue = segments.last();
		// TODO : support recursive snippetting of default value...
	} else {
		// mirror of an existing placeholder
		QCE::Snippets::Simple::Anchor a;
		a.lineOffset = line;
		a.columnOffset = column;
		if ( ph.defaultValue.isEmpty() )
			ph.unresolvedMirrors << baseSize << ph.mirrors.count();
		ph.mirrors << a;
	}
	
	index = i + 1;
	return ph.defaultValue;
}

void performRelocation(QCE::Snippets::Simple::Anchor& a, const QHash<int, QList<int> >& relocationTable, int length)
{
	QHash<int, QList<int> >::const_iterator reloc = relocationTable.constFind(a.lineOffset);
	
	if ( reloc == relocationTable.constEnd() )
		return;
	
	int idx = 0;
	int relocOffset = 0;
	const QList<int>& offsets = *reloc;
	
	while ( ((idx + 1) < offsets.count()) && (offsets.at(idx) <= a.columnOffset) )
	{
		int off = offsets.at(++idx);
		
		if ( offsets.at(idx - 1) < a.columnOffset || off != length )
			relocOffset += off;
		
		++idx;
	}
	
	a.columnOffset += relocOffset;
}

bool QCE::Snippets::Simple::loadSnippet(QSnippet *snip, const QString& pattern)
{
	Simple *target = dynamic_cast<Simple*>(snip);
	
	if ( !target )
	{
		qWarning("snippet/loader type mismatch");
		return false;
	}
	
	target->m_base.clear();
	target->m_placeHolders.clear();
	
	int index = 0, line = 0, column = 0, max = pattern.length();
	
	QString tmp;
	QStringList base;
	QMap<int, PlaceHolder> p;
	
	while ( index < max )
	{
		QChar c = pattern.at(index);
		
		if ( c == QLatin1Char('$') )
		{
			base << tmp;
			tmp.clear();
			
			c = pattern.at(++index);
			
			if ( c == QLatin1Char('{') )
			{
				QString val = parsePlaceHolder(pattern, index, max, p, line, column, base.count());
				base << val;
				
				if ( val.count() )
				{
					int nl = val.count(QLatin1Char('\n'));
					
					line += nl;
					
					if ( nl )
						column = val.count() - val.lastIndexOf(QLatin1Char('\n')) - 1;
					else
						column += val.count();
				}
				continue;
			} else {
				if ( c != QLatin1Char('$') )
				{
					c = pattern.at(--index);
				}
				
				++column;
			}
		} else if ( c == QLatin1Char('\n') ) {
			column = 0;
			++line;
		} else {
			++column;
		}
		
		tmp += c;
		++index;
	}
	
	if ( tmp.count() )
		base << tmp;
	
	QHash<int, QList<int> > relocationTable;
	QMap<int, PlaceHolder>::iterator it = p.begin();
	
	// first : build relocation table (in case several placeholders are on same line
	while ( it != p.end() )
	{
		if ( it->unresolvedMirrors.count() && it->length )
		{
			for ( int i = 0; i + 1 < it->unresolvedMirrors.count(); ++i )
			{
				int idx = it->unresolvedMirrors.at(i);
				int anchor = it->unresolvedMirrors.at(++i);
				
				base[idx] = it->defaultValue;
				
				const Anchor& a = it->mirrors.at(anchor);
				relocationTable[a.lineOffset] << a.columnOffset << it->length;
			}
			
			it->unresolvedMirrors.clear();
		}
		
		++it;
	}
	
	it = p.begin();
	
	// then : apply relocation and store the corrected placeholder data
	while ( it != p.end() )
	{
		performRelocation(*it, relocationTable, it->length);
		
		for ( int i = 0; i < it->mirrors.count(); ++i )
			performRelocation(it->mirrors[i], relocationTable, it->length);
		
		target->m_placeHolders << *it;
		++it;
	}
	
	target->m_base = base.join(QString::null);
	
	return true;
}

void QCE::Snippets::Simple::insert(QEditor *e) const
{
	// TODO : move into command and backup for proper undo/redo
	e->clearPlaceHolders();
	
	QDocument *d = e->document();
	QDocumentCursor c = e->cursor();
	
	if ( c.isNull() )
		c = QDocumentCursor(d);
	
	int line = qMax(c.lineNumber(), 0), column = qMax(c.columnNumber(), 0);
	
	if ( line != c.lineNumber() || column != c.columnNumber() )
		c = QDocumentCursor(d, line, column);
	
	QSnippetInsertionCommand *cmd = new QSnippetInsertionCommand(e);
	
	QDocumentCommand *scmd = 0;
	
	if ( c.hasSelection() )
	{
		QDocumentSelection sel = c.selection();
		
		//qDebug("((%i, %i), (%i, %i))", sel.startLine, sel.start, sel.endLine, sel.end);
		scmd = new QDocumentEraseCommand(sel.startLine, sel.start, sel.endLine, sel.end, d);
		
		cmd->addCommand(scmd);
		
		line = sel.startLine;
		column = sel.start;
	}
	
	//qDebug("%s", qPrintable(m_base));
	
	if ( scmd )
	{
		// trick to get insert command to init properly
		scmd->redo();
	}
	
	cmd->addCommand(new QDocumentInsertCommand(line, column, m_base, d));
	
	if ( scmd )
	{
		// trick to get insert command to init properly
		scmd->undo();
	}
	
	if ( m_placeHolders.count() )
	{
		foreach ( const PlaceHolder& ph, m_placeHolders )
		{
			QEditor::PlaceHolder eph;
			eph.length = ph.length;
			eph.cursor = QDocumentCursor(d, line + ph.lineOffset, ph.columnOffset + (ph.lineOffset ? 0 : column));
			//eph.affector = new StubAffector;
			foreach ( const Anchor& a, ph.mirrors )
				eph.mirrors << QDocumentCursor(d, line + a.lineOffset, a.columnOffset + (a.lineOffset ? 0 : column));
			
			cmd->addPlaceHolder(eph);
		}
	}
	
	d->execute(cmd);
}
