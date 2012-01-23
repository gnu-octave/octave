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
						

#include "QTerminal.h"

using namespace Konsole;

QTerminal::QTerminal(QWidget *parent)
    : QWidget(parent) {
    init();
    
    setFocus(Qt::OtherFocusReason);
    m_terminalDisplay->resize(this->size());
    
    this->setFocusProxy(m_terminalDisplay);
}

void QTerminal::init()
{
    m_session = new Session();

    m_session->setTitle(Session::NameRole, "QTermWidget");
    m_session->setProgram("/bin/bash");
    QStringList args("");
    m_session->setArguments(args);
    m_session->setAutoClose(true);
    m_session->setCodec(QTextCodec::codecForName("UTF-8"));
    m_session->setFlowControlEnabled(true);
    m_session->setHistoryType(HistoryTypeBuffer(1000));
    m_session->setDarkBackground(true);
    m_session->setKeyBindings("");

    m_terminalDisplay = new TerminalDisplay(this);
    m_terminalDisplay->setBellMode(TerminalDisplay::NotifyBell);
    m_terminalDisplay->setTerminalSizeHint(true);
    m_terminalDisplay->setTripleClickMode(TerminalDisplay::SelectWholeLine);
    m_terminalDisplay->setTerminalSizeStartup(true);
    m_terminalDisplay->setRandomSeed(m_session->sessionId() * 31);
    m_terminalDisplay->setSize(80, 40);
    
    QFont font = QApplication::font(); 
    font.setFamily("Monospace");
    font.setPointSize(10);
    font.setStyleHint(QFont::TypeWriter);
    setTerminalFont(font);  

    m_session->run();
    m_session->addView(m_terminalDisplay);
    m_terminalDisplay->setScrollBarPosition(TerminalDisplay::ScrollBarRight);

    connect(m_session, SIGNAL(finished()), this, SLOT(sessionFinished()));
}

QTerminal::~QTerminal()
{
    emit destroyed();
}


void QTerminal::setTerminalFont(QFont &font)
{
    if(!m_terminalDisplay)
	return;
    m_terminalDisplay->setVTFont(font);
}

void QTerminal::setShellProgram(const QString &progname)
{
    if(!m_session)
	return;
    m_session->setProgram(progname);
}

void QTerminal::setWorkingDirectory(const QString& dir)
{
    if(!m_session)
        return;
    m_session->setInitialWorkingDirectory(dir);
}

void QTerminal::setArgs(QStringList &args)
{
    if (!m_session)
	return;
    m_session->setArguments(args);
}

void QTerminal::setTextCodec(QTextCodec *codec)
{
    if(!m_session)
	return;
    m_session->setCodec(codec);
}

void QTerminal::setSize(int h, int v)
{
    if(!m_terminalDisplay)
	return;
    m_terminalDisplay->setSize(h, v);
}

void QTerminal::setHistorySize(int lines)
{
    if(lines < 0)
        m_session->setHistoryType(HistoryTypeFile());
    else
        m_session->setHistoryType(HistoryTypeBuffer(lines));
}

void QTerminal::setReadOnly(bool readonly)
{
    m_terminalDisplay->setReadOnly(readonly);
}

void QTerminal::resizeEvent(QResizeEvent*)
{
    m_terminalDisplay->resize(this->size());
}

void QTerminal::sessionFinished()
{
    emit finished();
}

void QTerminal::copyClipboard()
{
    m_terminalDisplay->copyClipboard();
}

void QTerminal::pasteClipboard()
{
    m_terminalDisplay->pasteClipboard();
}

void QTerminal::setFlowControlEnabled(bool enabled)
{
    m_session->setFlowControlEnabled(enabled);
}

bool QTerminal::flowControlEnabled(void)
{
    return m_session->flowControlEnabled();
}

void QTerminal::setFlowControlWarningEnabled(bool enabled)
{
    if(flowControlEnabled()) {
        m_terminalDisplay->setFlowControlWarningEnabled(enabled);
    }
}

void QTerminal::setEnvironment(const QStringList& environment)
{
    m_session->setEnvironment(environment);
}

void* QTerminal::getTerminalDisplay()
{
    return static_cast<void*>(m_terminalDisplay);
}

