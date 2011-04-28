/* This file is part of the KDE libraries
    Copyright (C) 2005, 2006 KJSEmbed Authors
    See included AUTHORS file.

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public License
    along with this library; see the file COPYING.LIB.  If not, write to
    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301, USA.
*/
#include <QTextDocument>
#include <QTextBlock>
#include <QHBoxLayout>
#include <QScrollBar>
#include <QPainter>
#include <QAbstractTextDocumentLayout>
#include <QToolTip>
#include <QTextStream>
#include <QProcess>
#include <QRegExp>
#include <QMessageBox>
#include <QFileInfo>

#include "NumberedCodeEdit.h"
#include "config.h"

NumberBar::NumberBar( QWidget *parent )
    : QWidget( parent ), edit(0), currentLine(-1), bugLine(-1)
{
    // Make room for 4 digits and the breakpoint icon
    setFixedWidth( fontMetrics().width( QString("0000") + 10 + 32 ) );
    stopMarker = QPixmap();// QString(ICON_PATH) + "/stop.png" );
    currentMarker = QPixmap();// QString(ICON_PATH) + "/bookmark.png" );
    bugMarker = QPixmap();// QString(ICON_PATH) + "/bug.png" );
}

NumberBar::~NumberBar()
{
}

void NumberBar::setCurrentLine( int lineno )
{
    currentLine = lineno;
    update();
}

void NumberBar::setBugLine( int lineno )
{
    bugLine = lineno;
}

void NumberBar::toggleBreakpoint( int lineno )
{
  if(lineno > 0)
  {
    int i = breakpoints.indexOf(lineno);

    if(i > -1)
      breakpoints.removeAt(i);
    else
      breakpoints.push_back(lineno);
  }
  update();
}

void NumberBar::setTextEdit( SimpleEditor *edit )
{
    this->edit = edit;
    setFixedWidth( edit->fontMetrics().width( QString("0000") + 10 + 32 ) );
    connect( edit->document()->documentLayout(), SIGNAL( update(const QRectF &) ),
	     this, SLOT( update() ) );
    connect( edit->verticalScrollBar(), SIGNAL(valueChanged(int) ),
	     this, SLOT( update() ) );
}

void NumberBar::paintEvent( QPaintEvent * )
{
    QVector<qreal> lines_list;
    int first_line_no;
    edit->publicBlockBoundingRectList(lines_list, first_line_no);
    
    const QFontMetrics fm = edit->fontMetrics();
    const int ascent = fontMetrics().ascent(); // height = ascent + descent
   
    QPainter p(this);
    p.setPen(palette().windowText().color());
    
    bugRect = QRect();
    stopRect = QRect();
    currentRect = QRect();
    
    int position_y;
    int lineCount;
    
    const int lines_list_size=lines_list.size();
    
    for(int i=0;i<lines_list_size;i++)
    {
    	position_y=qRound( lines_list[i] );
    	lineCount=first_line_no+i;
    	
    	const QString txt = QString::number( lineCount );
        p.drawText( width() - fm.width(txt)- 2, position_y+ascent, txt );
        
        // Bug marker
	if ( bugLine == lineCount ) {
	    p.drawPixmap( 1, position_y, bugMarker );
	    bugRect = QRect( 19, position_y, bugMarker.width(), bugMarker.height() );
	}
	
	// Stop marker
	if ( breakpoints.contains(lineCount) ) {
	    p.drawPixmap( 1, position_y, stopMarker );
	    stopRect = QRect( 1, position_y,stopMarker.width(),  stopMarker.height() );
	}
	
	// Current line marker
	if ( currentLine == lineCount ) {
	    p.drawPixmap( 1, position_y, currentMarker );
	    currentRect = QRect( 1, position_y, currentMarker.width(), currentMarker.height() );
	}
    }
    
    /*
    
    int contentsY = edit->verticalScrollBar()->value();
    qreal pageBottom = contentsY + edit->viewport()->height();
    const QFontMetrics fm = fontMetrics();
    const int ascent = fontMetrics().ascent() + 1; // height = ascent + descent + 1
    int lineCount = 1;

    QPainter p(this);
    p.setPen(palette().windowText().color());

    bugRect = QRect();
    stopRect = QRect();
    currentRect = QRect();

    for ( QTextBlock block = edit->document()->begin();
	  block.isValid(); block = block.next(), ++lineCount ) {

        const QRectF boundingRect = edit->publicBlockBoundingRect( block );

        QPointF position = boundingRect.topLeft();
        if ( position.y() + boundingRect.height() < contentsY )
            continue;
        if ( position.y() > pageBottom )
            break;

        const QString txt = QString::number( lineCount );
        p.drawText( width() - fm.width(txt), qRound( position.y() ) - contentsY + ascent, txt );

	// Bug marker
	if ( bugLine == lineCount ) {
	    p.drawPixmap( 1, qRound( position.y() ) - contentsY, bugMarker );
	    bugRect = QRect( 1, qRound( position.y() ) - contentsY, bugMarker.width(), bugMarker.height() );
	}

	// Stop marker
	if ( breakpoints.contains(lineCount) ) {
	    p.drawPixmap( 19, qRound( position.y() ) - contentsY, stopMarker );
	    stopRect = QRect( 19, qRound( position.y() ) - contentsY, stopMarker.width(), stopMarker.height() );
	}

	// Current line marker
	if ( currentLine == lineCount ) {
	    p.drawPixmap( 19, qRound( position.y() ) - contentsY, currentMarker );
	    currentRect = QRect( 19, qRound( position.y() ) - contentsY, currentMarker.width(), currentMarker.height() );
	}
    }
    */
}

bool NumberBar::event( QEvent *event )
{
    if ( event->type() == QEvent::ToolTip ) {
	QHelpEvent *helpEvent = static_cast<QHelpEvent *>(event);

	if ( stopRect.contains( helpEvent->pos() ) ) {
	    QToolTip::showText( helpEvent->globalPos(), tr("Stop Here"));
	}
	else if ( currentRect.contains( helpEvent->pos() ) ) {
	    QToolTip::showText( helpEvent->globalPos(), tr("Current Line"));
	}
	else if ( bugRect.contains( helpEvent->pos() ) ) {
	    QToolTip::showText( helpEvent->globalPos(), tr("Error Line" ));
	}
    }

    return QWidget::event(event);
}

QList<int> *NumberBar::getBreakpoints()
{
  return &breakpoints;
}



NumberedCodeEdit::NumberedCodeEdit( QWidget *parent, SimpleEditor *textEdit )
    : QFrame( parent )
{
	setFrameStyle( QFrame::StyledPanel | QFrame::Sunken );
	setLineWidth( 2 );
	
	view=textEdit;
        view->installEventFilter( this );
	
	connect( view->document(), SIGNAL(contentsChange(int,int,int)), this, SLOT(textChanged(int,int,int)) );
	
	connect( view, SIGNAL(cursorPositionChanged()), this, SLOT(cursor_moved_cb()) );
	
	// Setup the line number pane
	
	numbers = new NumberBar( this );
	numbers->setTextEdit( view );
	//numbers=NULL;
	
	
	vbox = new QVBoxLayout(this);
	vbox->setSpacing( 0 );
	vbox->setMargin( 0 );
	
	hbox = new QHBoxLayout;
	vbox->addLayout(hbox);
	
	hbox->setSpacing( 0 );
	hbox->setMargin( 0 );
	hbox->addWidget( numbers );
	hbox->addWidget( view );

	textModifiedOk=false;
	
	QHBoxLayout *messages_layout= new QHBoxLayout;
	vbox->addLayout(messages_layout);
	messages_layout->setSpacing( 0 );
	messages_layout->setMargin( 0 );
	}


NumberedCodeEdit::~NumberedCodeEdit()
{
	hide();
	//printf("Borrado ntv\n");
}

void NumberedCodeEdit::setCurrentLine( int lineno )
{
	currentLine = lineno;
	if(numbers!=NULL) numbers->setCurrentLine( lineno );
	
	//Move cursor to lineno
	if(lineno>-1)
	{
		QTextCursor cursor=textEdit()->textCursor();
		
		cursor.movePosition(QTextCursor::Start);
		
		for(int i=1;i<lineno;i++)
			cursor.movePosition(QTextCursor::NextBlock);
		
		textEdit()->setTextCursor(cursor);
	}
	
	textChanged( 0, 0, 1 );
}

void NumberedCodeEdit::toggleBreakpoint( int lineno )
{
	if(numbers!=NULL) numbers->toggleBreakpoint( lineno );
}

void NumberedCodeEdit::setBugLine( int lineno )
{
	if(numbers!=NULL) numbers->setBugLine( lineno );
}

void NumberedCodeEdit::textChanged( int /*pos*/, int removed, int added )
{
    //Q_UNUSED( pos );

    if ( removed == 0 && added == 0 )
	return;

    //QTextBlock block = highlight.block();
    //QTextBlock block = view->document()->begin();
    //QTextBlockFormat fmt = block.blockFormat();
    //QColor bg = view->palette().base().color();
    //fmt.setBackground( bg );
    //highlight.setBlockFormat( fmt );
    /*
    QTextBlockFormat fmt;

    int lineCount = 1;
    for ( QTextBlock block = view->document()->begin();
	  block.isValid() && block!=view->document()->end(); block = block.next(), ++lineCount ) {

	if ( lineCount == currentLine )
	{
	    fmt = block.blockFormat();
	    QColor bg = view->palette().highlight().color();
	    fmt.setBackground( bg );

	    highlight = QTextCursor( block );
	    highlight.movePosition( QTextCursor::EndOfBlock, QTextCursor::KeepAnchor );
	    highlight.setBlockFormat( fmt );

	    break;
	}
    }
    */
    
    if( !textModifiedOk && view->document()->isModified() )
    {
    	textModifiedOk=true;
    	emit textModified();
    }
}

bool NumberedCodeEdit::eventFilter( QObject *obj, QEvent *event )
{
    if ( obj != view )
	return QFrame::eventFilter(obj, event);

    if ( event->type() == QEvent::ToolTip ) {
	QHelpEvent *helpEvent = static_cast<QHelpEvent *>(event);

	QTextCursor cursor = view->cursorForPosition( helpEvent->pos() );
	cursor.movePosition( QTextCursor::StartOfWord, QTextCursor::MoveAnchor );
	cursor.movePosition( QTextCursor::EndOfWord, QTextCursor::KeepAnchor );

	QString word = cursor.selectedText();
	emit mouseHover( word );
	emit mouseHover( helpEvent->pos(), word );

	// QToolTip::showText( helpEvent->globalPos(), word ); // For testing
    }

    return false;
}

QList<int> *NumberedCodeEdit::getBreakpoints()
{
	QList<int> *br=NULL;
	if(numbers!=NULL) br=numbers->getBreakpoints();
	return br;
}

void NumberedCodeEdit::open(QString path)
{
  FILE *fl;

  fl = fopen(path.toLocal8Bit().constData(), "rt");
  if(fl)
  {
	fclose(fl);
	filePath = path;
	
	textEdit()->load(path);
	
	textModifiedOk=false;
	textEdit()->document()->setModified(false);
  }else{
    throw path;
  }
}

bool NumberedCodeEdit::save(QString path)
{
  FILE *fl;

  if(path.isEmpty()) path = filePath;
  QRegExp re("[A-Za-z_][A-Za-z0-9_]*\\.m");
  
  if( ! re.exactMatch( QFileInfo(path).fileName() ) )
  {
	QMessageBox msgBox;
	msgBox.setText( tr("This file name is not valid.") );
	msgBox.setInformativeText(tr("Octave doesn't understand this file name:\n")+path+tr("\nPlease, change it.\n Do you want to save your changes?"));
	msgBox.setStandardButtons(QMessageBox::Save | QMessageBox::Cancel);
	msgBox.setDefaultButton(QMessageBox::Save);
	int ret = msgBox.exec();
	switch (ret)
	{
		case QMessageBox::Save:
		    // Save was clicked
		    break;
		case QMessageBox::Cancel:
		    // Cancel was clicked
		    	return false;
		    break;
		default:
		    // should never be reached
		    break;
	}
  }
  
  
  fl = fopen(path.toLocal8Bit().constData(), "wt");
  if(fl)
  {
    filePath = path;
    QTextStream *stream = new QTextStream(fl);
    (*stream) << textEdit()->document()->toPlainText();
    delete stream;
    fclose(fl);
    textModifiedOk=false;
    view->document()->setModified(false);
  }else{
    return false;
  }
  
  return true;
}

QString NumberedCodeEdit::path()
{
  return filePath;
}

void NumberedCodeEdit::setPath(QString path)
{
	filePath=path;
	textEdit()->setFile(path);
}

void NumberedCodeEdit::setModified(bool modify)
{
	textModifiedOk=modify;
}

bool NumberedCodeEdit::modified()
{
	return textModifiedOk;
}

void NumberedCodeEdit::cursor_moved_cb()
{
	QTextCursor cursor=view->textCursor();
	QTextBlock actual_block=cursor.block();
	int lineCount=1;
	QTextBlock block = view->document()->begin();
	
	for ( ;block.isValid() && actual_block!=block; block = block.next()) lineCount++ ;
}

static QString startLineInsertText(QString str, QString textToInsert)
{
	str.replace(QChar(0x2029), "\n");
	//printf("str=%s\n", str.toLocal8Bit().data() );
	
	QStringList list = str.split("\n");
	
	for(int i=0;i<list.size();i++)
	{
		QString s=list[i];
		
		int x;
		
		for(x=0;x<s.size();x++)
		{
			if( s.at(x)!=' ' && s.at(x)!='\t' ) break;
		}
		
		QString s1=s.left(x);
                QString s2=s.right(s.size()-x);
		list[i]=s1+textToInsert+s2;
	}
	
	return list.join("\n");
}

static QString startLineRemoveText(QString str, QStringList textToRemove)
{
	str.replace(QChar(0x2029), "\n");
	
	QStringList list = str.split("\n");
	
	for(int i=0;i<list.size();i++)
	{
		QString s=list[i];
		
		int x;
		
		for(x=0;x<s.size();x++)
		{
			if( s.at(x)!=' ' && s.at(x)!='\t' ) break;
		}
		
		QString s1=s.left(x);
		QString s2=s.right(s.size()-x);
		
		for(int k=0;k<textToRemove.size();k++)
		{
			if(s1.endsWith(textToRemove[k]))
			{
				s1=s1.left(s1.size()-textToRemove[k].size());
				break;
			}
			else if(s2.startsWith(textToRemove[k]))
			{
				s2=s2.right(s2.size()-textToRemove[k].size());
				break;
			}
		}
		
		//printf("s1=%s s2=%s \n", s1.toLocal8Bit().data(), s2.toLocal8Bit().data());
		list[i]=s1+s2;
	}
	
	return list.join("\n");
}

void NumberedCodeEdit::indent()
{
	QTextCursor cursor(textEdit()->textCursor());
	
	if( !cursor.hasSelection() ) return;
	
	QString str=cursor.selectedText();
	
	str=startLineInsertText(str, "\t");
	
	cursor.insertText(str);
	cursor.setPosition(cursor.position()-str.size(), QTextCursor::KeepAnchor);
	textEdit()->setTextCursor(cursor);
}

void NumberedCodeEdit::unindent()
{
	//QTextDocument *doc=textEdit()->document();
	
	QTextCursor cursor(textEdit()->textCursor());
	
	if( !cursor.hasSelection() ) return;
	
	QString str=cursor.selectedText();
	
	QStringList textToRemove;
	textToRemove << "\t" << " ";
	str=startLineRemoveText(str, textToRemove);
	
	cursor.insertText(str);
	cursor.setPosition(cursor.position()-str.size(), QTextCursor::KeepAnchor);
	textEdit()->setTextCursor(cursor);
}

void NumberedCodeEdit::comment()
{
	//QTextDocument *doc=textEdit()->document();
	
	QTextCursor cursor(textEdit()->textCursor());
	
	if( !cursor.hasSelection() ) return;
	
	QString str=cursor.selectedText();
	
	str=startLineInsertText(str, "%");
	
	cursor.insertText(str);
	cursor.setPosition(cursor.position()-str.size(), QTextCursor::KeepAnchor);
	textEdit()->setTextCursor(cursor);
}

void NumberedCodeEdit::uncomment()
{
	//QTextDocument *doc=textEdit()->document();
	
	QTextCursor cursor(textEdit()->textCursor());
	
	if( !cursor.hasSelection() ) return;
	
	QString str=cursor.selectedText();
	
	QStringList textToRemove;
	textToRemove << "%" << "#";
	str=startLineRemoveText(str, textToRemove);
	
	cursor.insertText(str);
	cursor.setPosition(cursor.position()-str.size(), QTextCursor::KeepAnchor);
	textEdit()->setTextCursor(cursor);
}
