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

#ifndef _QSNIPPET_BINDING_H_
#define _QSNIPPET_BINDING_H_

#include "qce-config.h"

/*!
	\file qsnippetbinding.h
	\brief Definition of the QSnippetBinding class
*/

#include "qeditorinputbinding.h"

class QSnippetManager;

class QCE_EXPORT QSnippetBinding : public QEditorInputBinding
{
	public:
		QSnippetBinding(QSnippetManager *manager);
		
		virtual QString id() const;
		virtual QString name() const;
		
		virtual bool keyPressEvent(QKeyEvent *event, QEditor *editor);
		
	private:
		QSnippetManager *m_manager;
};

#endif
