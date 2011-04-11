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

#ifndef _QSNIPPET_P_H_
#define _QSNIPPET_P_H_

/*!
	\file qsnippet_p.h
	\brief Definition of the QSnippetInsertionCommand class
*/

#include "qsnippet.h"
#include "qsnippetpatternloader.h"

#include "qeditor.h"
#include "qdocument.h"
#include "qdocumentcursor.h"
#include "qdocumentcommand.h"

class QSnippetInsertionCommand : public QDocumentCommandBlock
{
	public:
		QSnippetInsertionCommand(QEditor *e);
		virtual ~QSnippetInsertionCommand();
		
		void addPlaceHolder(const QEditor::PlaceHolder& ph);
		
		virtual void addCommand(QDocumentCommand *c);
		virtual void removeCommand(QDocumentCommand *c);
		
		virtual void redo();
		virtual void undo();
		
	private:
		QEditor *m_editor;
		QDocumentCursor m_cursor;
		QList<QEditor::PlaceHolder> m_placeHolders;
};

#define Q_SNIPPET(T)							\
	friend class Loader;						\
	public:										\
	class Loader : public QSnippetPatternLoader \
	{											\
		public:									\
			virtual QString type() const { return ""#T; }								\
			virtual QSnippet* loadSnippet(const QString& pattern) const					\
			{																			\
				T *snip = new T(this);													\
				snip->m_pattern = pattern;												\
				bool ok = reloadSnippet(snip, pattern);									\
				if ( !ok ) { delete snip; snip = 0; }									\
				return snip;															\
			}																			\
			virtual bool reloadSnippet(QSnippet* snip, const QString& pattern) const	\
			{ return T::loadSnippet(snip, pattern); }									\
	};																					\
	inline T(const QSnippetPatternLoader *pl) : QSnippet(pl) {}							\
	private:																			\
	

namespace QCE
{
	namespace Snippets
	{
		class PlainText : public QSnippet
		{
			Q_SNIPPET(PlainText)
			
			public:
				virtual void insert(QEditor *e) const;
				
				static bool loadSnippet(QSnippet *snip, const QString& pattern);
				
				QString m_data;
		};

		class Simple : public QSnippet
		{
			Q_SNIPPET(Simple)
			
			public:
				struct Anchor
				{
					Anchor() : lineOffset(0), columnOffset(0) {}
					
					int lineOffset;
					int columnOffset;
				};
				
				struct PlaceHolder : public Anchor
				{
					PlaceHolder() : length(-1) {}
					
					int length;
					QString defaultValue;
					QList<Anchor> mirrors;
					QList<int> unresolvedMirrors;
				};
				
				virtual void insert(QEditor *e) const;
				
				static bool loadSnippet(QSnippet *snip, const QString& pattern);
				
				QString m_base;
				QList<PlaceHolder> m_placeHolders;
		};
	}
}

#endif
