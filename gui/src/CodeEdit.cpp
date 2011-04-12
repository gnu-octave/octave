/* Copyright (C) 2007 Alejandro Álvarez
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <QContextMenuEvent>
#include "CodeEdit.h"

CodeEdit::CodeEdit(QWidget *parent, QString syntaxF): QTextEdit(parent)
, contextMenu(this)
{
  syntax=new Syntax(document());
  if(!syntaxF.isEmpty())
    syntax->load(syntaxF);

  setUndoRedoEnabled(true);
  setTabStopWidth(32);
  setFrameStyle(QFrame::NoFrame);

  autocompletion_ok=true;//(get_config("autoCompletion")!="false");
  brakets_match_ok=true;//(get_config("bracketsMatch")!="false");

  // ContextMenu

  connect(contextMenu.addAction(tr("Undo")), SIGNAL(triggered()),
	  this, SLOT(undo()));
  connect(contextMenu.addAction(tr("Redo")), SIGNAL(triggered()),
	  this, SLOT(redo()));

  contextMenu.addSeparator();

  connect(contextMenu.addAction(tr("Cut")), SIGNAL(triggered()),
	  this, SLOT(cut()));
  connect(contextMenu.addAction(tr("Copy")), SIGNAL(triggered()),
	  this, SLOT(copy()));
  connect(contextMenu.addAction(tr("Paste")), SIGNAL(triggered()),
	  this, SLOT(paste()));
  connect(contextMenu.addAction(tr("Delete")), SIGNAL(triggered()),
	  this, SLOT(deleteSelection()));

  contextMenu.addSeparator();

  connect(contextMenu.addAction(tr("Select all")), SIGNAL(triggered()),
	  this, SLOT(selectAll()));

  contextMenu.addSeparator();

  connect(contextMenu.addAction(tr("Toggle breakpoint")), SIGNAL(triggered()),
	  this, SLOT(toggleBreakpoint()));
	  
  //connect(this->document(), SIGNAL( contentsChange ( int , int , int  )), this, SLOT(buildUndoRedoStack(int , int , int)) );
	  
  if(autocompletion_ok)
  {
	connect(this->document(), SIGNAL( contentsChange ( int , int , int  )), this, SLOT(buildAutoCompletionList(int , int , int)) );
	connect(&completion, SIGNAL(activated ( const QModelIndex &)), this, SLOT(doCompletion(const QModelIndex &)) );
  }

  if(brakets_match_ok)
  {
  	connect(&braketsTimer, SIGNAL(timeout ()), this, SLOT(braketsMatch()));
  }
  braketsTimer.setSingleShot(true);
  braketsTimer.setInterval(50);

  octaveCommandTimer.setSingleShot(true);
  octaveCommandTimer.setInterval(2000);
  
  connect(&octaveCommandTimer, SIGNAL(timeout ()), this, SLOT(octaveCommandCompletion()));
  
  completionTimer.setSingleShot(true);
  completionTimer.setInterval(200);

  connect(&completionTimer, SIGNAL(timeout ()), this, SLOT(buildAutoCompletionList()));

   connect(this, SIGNAL( cursorPositionChanged() ), this, SLOT( cursorChanged_cb() ) );

  auto_indent=true;
  setAcceptDrops(false);
  if(autocompletion_ok)
  {
	completion.setWidget(this);
	completion_model=new QStringListModel(&completion);
	completion.setModel(completion_model);
	completion.setCompletionMode(QCompleter::UnfilteredPopupCompletion);
  }

  text_modified_stop_ok=context_changed_ok=false;

  connect(document(), SIGNAL(modificationChanged (bool)), this, SLOT(textModified_cb(bool)));

  auto_indent=true;//("false"!=get_config("autoindent"));
  automatic_indention_statement_ok=true;//(get_config("autoindent_statements")=="true");
}

CodeEdit::~CodeEdit()
{
	//printf("Borrando la sintaxis\n");
	delete syntax;
	//printf("CodeEdit destruido\n");
}

void CodeEdit::contextMenuEvent(QContextMenuEvent *e)
{
  //setTextCursor(cursorForPosition(e->pos()));
  contextMenu.exec(e->globalPos());
}

void CodeEdit::undo()
{
	document()->undo();
}

void CodeEdit::redo()
{
  document()->redo();
}

void CodeEdit::deleteSelection()
{
  textCursor().removeSelectedText();
}

void CodeEdit::toggleBreakpoint()
{
  int line = 1;
  for(QTextBlock tb = document()->begin();
      tb.isValid();
      line++, tb = tb.next())
  {
    if(tb == textCursor().block())
    {
      emit toggleBreakpoint(line);
      return;
    }
  }
}

bool CodeEdit::event( QEvent * e )
{

	if(QEvent::KeyPress==e->type() )
	{
		QKeyEvent *k=(QKeyEvent *)e;
		if( autocompletion_ok && ( Qt::Key_Left==k->key() || Qt::Key_Right==k->key() ) )
		{
			completion.popup()->hide();
		}
		else if( Qt::Key_Return==k->key() ||  Qt::Key_Enter==k->key())
		{
			if(autocompletion_ok && !completion.popup()->isHidden())
			{
				doCompletion(completion.popup()->currentIndex());
			}
			else if( auto_indent )
			{
				QTextCursor cursor=textCursor();
				int pos=cursor.position();
				cursor.movePosition(QTextCursor::StartOfBlock,QTextCursor::KeepAnchor);
				QString line=cursor.selectedText();
				QString start_blank;
				start_blank.append('\n');
				for(int i=0;i<line.length() && (line.at(i)==' ' || line.at(i)=='\t');i++)
					start_blank.append(line.at(i));
				if( automatic_indention_statement_ok )
				{
					QRegExp re("^while[ |(].*|^if[ |(].*|^for .*|^switch[ |(].*|^do$|^try|^else|^elseif$");
					if(re.exactMatch( line.trimmed() ) )
						start_blank.append("\t");
				}

				cursor.setPosition(pos);
				cursor.insertText(start_blank);
				setTextCursor( cursor );
			}
			else
			{
                                return QTextEdit::event(e);
			}
			return true;
		}
	}
        return QTextEdit::event(e);
}

void CodeEdit::setAutoindent(bool ai_ok)
{
	auto_indent=ai_ok;
}

bool CodeEdit::getAutoindent() {return auto_indent;}


void CodeEdit::cursorChanged_cb()
{
	if(brakets_match_ok)
	{
		if(!context_changed_ok) braketsTimer.start();
		else
		{
			context_changed_ok=false;
			//braketsTimer.stop();
		}
	}
}


void CodeEdit::braketsMatch(bool rehigh)
{
	bool text_modified_ok=document()->isModified();
	text_modified_stop_ok=true;
	//if(autocompletion_ok) disconnect(this->document(), SIGNAL( contentsChange ( int , int , int  )), this, SLOT(buildAutoCompletionList(int , int , int)) );
	int pos=textCursor().position();
	int start=-1, end=-1;
	if(pos>=0) syntax->braketsMacth(pos, start, end, rehigh);
	QList<QTextEdit::ExtraSelection> selection_list;
	if(start>=0 && end >=0)
	{
		QTextEdit::ExtraSelection selection;
		selection.cursor=textCursor();
		selection.cursor.setPosition(start);
		selection.cursor.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor);
		selection.format=selection.cursor.blockCharFormat();
		QBrush brush(Qt::yellow);
		selection.format.setBackground (brush);
		selection_list.append(selection);
		selection.cursor=textCursor();
		selection.cursor.setPosition(end);
		selection.cursor.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor);
		selection_list.append(selection);
	}
	setExtraSelections(selection_list);
	//if(autocompletion_ok) connect(this->document(), SIGNAL( contentsChange ( int , int , int  )), this, SLOT(buildAutoCompletionList(int , int , int)) );
	document()->setModified(text_modified_ok);
	text_modified_stop_ok=false;
}

void CodeEdit::buildAutoCompletionList(int pos, int charsRemoved, int charsAdded )
{
	octaveCommandTimer.stop();

	if(brakets_match_ok)
	{
		braketsTimer.start();
		//braketsMatch(true);
	}
	document()->setModified(true);
	context_changed_ok=true;
	
	completionPosition=pos;
	completionTimer.start();
	completion.popup()->hide();
}

void CodeEdit::buildAutoCompletionList()
{
	QTextCursor cursor=textCursor();
	int pos=cursor.position()-1;
	//printf("[CodeEdit::buildAutoCompletionList] pos=%d completionPosition=%d\n", pos, completionPosition);
	if(pos!=completionPosition)
		return;
	
	//QTextBlock block=document()->findBlock(0);
	//Cleans brakets match information

	//while( block.isValid() )
	//{
		//block.setUserData(NULL);
		//block=block.next();
	//}
	//Buscando palabra a completar
	QTextBlock block=document()->findBlock(pos);
	//int _pos=pos;
	pos-=block.position();
	int i=pos;
	QString text=block.text();
	QRegExp re("([^a-zA-Z_0-9]+)");
	i=re.lastIndexIn(text, i);
	//printf("pos=%d i=%d len=%d\n", pos, i, re.matchedLength());
	if( i==pos ) {completion.popup()->hide();return;}
	QString word_to_complete=text.mid(i+1,pos-i);
	//printf("i=%d word=>%s<\n",i, word_to_complete.toLocal8Bit().data());
	QString actual_word;
	re.setPattern("([a-zA-Z_0-9]+)");
	i=re.indexIn(text, pos);
	if( i==pos ) actual_word=word_to_complete+text.mid(pos+1,re.matchedLength()-1);
	//printf("i=%d word=>%s<\n",i, actual_word.toLocal8Bit().data());

	if(word_to_complete.length()==2)
	{
		completion_model->setStringList(syntax->octave_comands);
		completion.setCompletionPrefix(word_to_complete);
		completion.popup()->hide();

		octaveCommandTimer.start();

		return;
	}
	else if(word_to_complete.length()<3) {completion.popup()->hide();return;}

	//Searchs help for command
	//printf("%s\n", word_to_complete.toLocal8Bit().data());
	emit dynamic_help_required(word_to_complete);

	//Se construye la lista de palabras a completar
	
	QTextBlock blockInit, blockEnd;
	blockInit=document()->firstBlock();
	blockEnd =document()->lastBlock();
	completion_list.clear();
	buildAutoCompletionListSlide(completion_list, blockInit, blockEnd, word_to_complete, actual_word);

	if(completion_list.isEmpty()) {completion.popup()->hide();return;}

	completion_model->setStringList(completion_list);


	QRect _position=cursorRect();

	//printf("x=%d y=%d width=%d height=%d\n", _position.x(), _position.y(), _position.width(), _position.height() );

	//_position.moveTo(_position.bottomRight() );
	////_position.setWidth(100);
	_position.setWidth(width()/3);

	completion.setCompletionPrefix(word_to_complete);
	completion.complete(_position);
	completion.popup()->show();
	completion.popup()->setFocus(Qt::TabFocusReason);
}

void CodeEdit::buildAutoCompletionListSlide(QStringList &list, QTextBlock blockInit, QTextBlock blockEnd, QString word_to_complete, QString actual_word)
{
	//QStringList list;

	//printf("Buscando lista\n");
	//block=document()->findBlock(0);

	QTextBlock block=blockInit;

	//QString match;
	QRegExp rx("([a-zA-Z_0-9]+)");

	while( block.isValid() )
	{
		QString text=block.text();
		int i = 0;

		while ((i = rx.indexIn(text, i)) != -1) {
			QString word=rx.cap(1);
			if( word.startsWith(word_to_complete) && !list.contains(word) && word!=actual_word )
			{
				list << word;
				//printf("i=%d word=>%s< actual_word=>%s<\n",i, word.toLocal8Bit().data(), actual_word.toLocal8Bit().data());
			}
			i += rx.matchedLength();
		}

		if(block!=blockEnd) block=block.next();
		else break;
	}
}

void CodeEdit::octaveCommandCompletion()
{
	QRect _position=cursorRect();

	_position.setWidth(width());

	completion.complete(_position);
	completion.popup()->show();
	completion.popup()->setFocus(Qt::TabFocusReason);
}


void CodeEdit::doCompletion(const QModelIndex &index)
{
	QString word=index.data().toString();
	QString prefix=completion.completionPrefix();

	QString suffix=word.mid(prefix.length());

	QTextCursor cursor=textCursor();
	cursor.insertText(suffix);

	completion.popup()->hide();
}

bool CodeEdit::getbraketsMatchOk()
{
	return syntax->getIsActiveBraketsMacth();
}

void CodeEdit::textModified_cb(bool ok)
{
	//printf("[CodeEdit::textModified_cb] Entered\n");
	if(text_modified_stop_ok) return;
	emit text_modified(ok);
	//printf("[CodeEdit::textModified_cb] text_modified emit\n");
}

void CodeEdit::publicBlockBoundingRectList(QVector<qreal> &list, int &first_line)
{/*
        qreal pageBottom = viewport()->height();
	QPointF offset=contentOffset();
	QTextBlock block=firstVisibleBlock();
	first_line=block.blockNumber()+1;
	qreal first_position=blockBoundingGeometry(block).topLeft().y();
	
	for ( ; block.isValid(); block = block.next() )
	{
		QRectF position=blockBoundingGeometry(block);
		qreal y=position.topLeft().y()+offset.y()-first_position;
		
		if(y>pageBottom) break;
		
		list.append(y);
	}
        */
}

