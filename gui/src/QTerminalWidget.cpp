/*  Copyright (C) 2008 e_k (e_k@users.sourceforge.net)

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
						

#include "QTerminalWidget.h"
#include "Session.h"
#include "TerminalDisplay.h"

struct TermWidgetImpl
{
    TermWidgetImpl(QWidget* parent = 0);

    TerminalDisplay *m_terminalDisplay;
    Session *m_session;
    Session* createSession();
    TerminalDisplay* createTerminalDisplay(Session *session, QWidget* parent);
};

TermWidgetImpl::TermWidgetImpl(QWidget* parent)
{
    this->m_session = createSession();
    this->m_terminalDisplay = createTerminalDisplay(this->m_session, parent);
}

Session *TermWidgetImpl::createSession()
{
    Session *session = new Session();
    session->setTitle(Session::NameRole, "QTerminalWidget");
    session->setProgram("/bin/bash");
    session->setArguments(QStringList());
    session->setAutoClose(true);
    session->setCodec(QTextCodec::codecForName("UTF-8"));
    session->setFlowControlEnabled(true);
    session->setHistoryType(HistoryTypeBuffer(1000));
    session->setDarkBackground(true);
    session->setKeyBindings("");
    return session;
}

TerminalDisplay *TermWidgetImpl::createTerminalDisplay(Session *session, QWidget* parent)
{
    TerminalDisplay* display = new TerminalDisplay(parent);
    display->setBellMode(TerminalDisplay::NotifyBell);
    display->setTerminalSizeHint(true);
    display->setTripleClickMode(TerminalDisplay::SelectWholeLine);
    display->setTerminalSizeStartup(true);
    display->setRandomSeed(session->sessionId() * 31);
    return display;
}

QTerminalWidget::QTerminalWidget(int startnow, QWidget *parent)
    :QWidget(parent)
{
    m_impl = new TermWidgetImpl(this);
    
    initialize();

    if (startnow && m_impl->m_session) {
	m_impl->m_session->run();
    }
    
    this->setFocus( Qt::OtherFocusReason );
    m_impl->m_terminalDisplay->resize(this->size());
    
    this->setFocusProxy(m_impl->m_terminalDisplay);
}

void QTerminalWidget::startShellProgram()
{
    if ( m_impl->m_session->isRunning() )
	return;
	
    m_impl->m_session->run();
}

void QTerminalWidget::initialize()
{    
    m_impl->m_terminalDisplay->setSize(80, 40);
    
    QFont font = QApplication::font(); 
    font.setFamily("Monospace");
    font.setPointSize(10);
    font.setStyleHint(QFont::TypeWriter);
    setTerminalFont(font);
    setScrollBarPosition(NoScrollBar);

    m_impl->m_session->addView(m_impl->m_terminalDisplay);
    
    connect(m_impl->m_session, SIGNAL(finished()), this, SLOT(sessionFinished()));
}

QTerminalWidget::~QTerminalWidget()
{
    emit destroyed();
}

void QTerminalWidget::setTerminalFont(QFont &font)
{
    if (!m_impl->m_terminalDisplay)
	return;
    m_impl->m_terminalDisplay->setVTFont(font);
}

void QTerminalWidget::setShellProgram(QString progname)
{
    if (!m_impl->m_session)
	return;
    m_impl->m_session->setProgram(progname);	
}

void QTerminalWidget::setArgs(QStringList &args)
{
    if (!m_impl->m_session)
	return;
    m_impl->m_session->setArguments(args);	
}

void QTerminalWidget::setTextCodec(QTextCodec *codec)
{
    if (!m_impl->m_session)
	return;
    m_impl->m_session->setCodec(codec);	
}

void QTerminalWidget::setColorScheme(ColorScheme scheme)
{
    switch(scheme) {
        case WhiteOnBlack:
		m_impl->m_terminalDisplay->setColorTable(whiteonblack_color_table);
		break;		
        case GreenOnBlack:
		m_impl->m_terminalDisplay->setColorTable(greenonblack_color_table);
		break;
        case BlackOnLightYellow:
		m_impl->m_terminalDisplay->setColorTable(blackonlightyellow_color_table);
		break;
    };
}

void QTerminalWidget::setSize(int h, int v)
{
    if (!m_impl->m_terminalDisplay)
	return;
    m_impl->m_terminalDisplay->setSize(h, v);
}

void QTerminalWidget::setHistorySize(int lines)
{
    if (lines < 0)
        m_impl->m_session->setHistoryType(HistoryTypeFile());
    else
	m_impl->m_session->setHistoryType(HistoryTypeBuffer(lines));
}

void QTerminalWidget::setScrollBarPosition(ScrollBarPosition pos)
{
    if (!m_impl->m_terminalDisplay)
	return;
    m_impl->m_terminalDisplay->setScrollBarPosition((TerminalDisplay::ScrollBarPosition)pos);
}

void QTerminalWidget::sendText(QString &text)
{
    m_impl->m_session->sendText(text); 
}

void QTerminalWidget::resizeEvent(QResizeEvent*)
{
    m_impl->m_terminalDisplay->resize(this->size());
    m_impl->m_terminalDisplay->update();
}

void QTerminalWidget::sessionFinished()
{
    emit finished();
}


