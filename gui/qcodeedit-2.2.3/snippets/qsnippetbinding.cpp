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

#include "qsnippetbinding.h"

/*!
	\file qsnippetbinding.cpp
	\brief Implementation of the QSnippetBinding class
*/

#include "qsnippet.h"
#include "qsnippetmanager.h"

#include "qeditor.h"
#include "qdocumentcursor.h"

#include <QKeyEvent>

/*
class QSnippetCommand : public QEditorInputBinding::Command
{
	public:
		QSnippetCommand(QSnippetManager *m, int idx)
		 : index(idx), manager(m)
		{
			
		}
		
		virtual void exec(QEditor *e)
		{
			if ( index < manager->snippetCount() )
				manager->snippet(index)->insert(e);
		}
		
	private:
		int index;
		QSnippetManager *manager;
};
*/

class QSnippetCommand : public QEditorInputBinding::Command
{
	public:
		QSnippetCommand(QSnippet *s)
		 : snip(s)
		{
			
		}
		
		virtual void exec(QEditor *e)
		{
			snip->insert(e);
		}
		
	private:
		QSnippet *snip;
};

QSnippetBinding::QSnippetBinding(QSnippetManager *manager)
 : m_manager(manager)
{
	//for ( int i = 0; i < 10; ++i )
	//	setMapping(QKeySequence(Qt::Key_F1 + i), new SnippetCommand(manager, i));
}

QString QSnippetBinding::id() const
{
	return "snippet binding";
}

QString QSnippetBinding::name() const
{
	return "snippet binding";
}

bool QSnippetBinding::keyPressEvent(QKeyEvent *event, QEditor *editor)
{
	/*
	if ( event->modifiers() & Qt::ControlModifier )
	{
		for ( int i = 0; i < qMin(10, m->snippetCount()); ++i )
		{
			if ( event->key() == (Qt::Key_F1 + i) )
			{
				m->snippet(i)->insert(editor);
				return true;
			}
		}
	}
	*/
	
	if ( (event->modifiers() & Qt::AltModifier) && (event->key() == Qt::Key_Space || event->text() == " ") )
	{
		QDocumentCursor c = editor->cursor();
		
		//c.select(QDocumentCursor::SelectWord);
		
		if ( !c.hasSelection() )
		{
			c.movePosition(1, QDocumentCursor::PreviousWord, QDocumentCursor::KeepAnchor);
			editor->setCursor(c);
		}
		
		QString s = c.selectedText();
		
		for ( int i = 0; i < m_manager->snippetCount(); ++i )
		{
			QSnippet *snip = m_manager->snippet(i);
			
			if ( snip->name() == s )
			{
				snip->insert(editor);
				return true;
			}
		}
	}
	
	return QEditorInputBinding::keyPressEvent(event, editor);
}
