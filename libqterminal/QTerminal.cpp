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
#include "kpty.h"

QTerminal::QTerminal(QWidget *parent)
    : QWidget(parent) {
    setMinimumSize(600, 400);
    init();
    
    setFocus(Qt::OtherFocusReason);
    m_sessionView->resize(this->size());
    
    this->setFocusProxy(m_sessionView);
}

void QTerminal::init()
{
    KPty *kpty = new KPty();
    kpty->open();
    int fds = kpty->slaveFd();

    dup2 (fds, 0);
    dup2 (fds, 1);
    dup2 (fds, 2);

    m_sessionModel = new SessionModel(kpty);

    m_sessionModel->setTitle(SessionModel::NameRole, "QTermWidget");
    m_sessionModel->setProgram("/bin/bash");
    QStringList args("");
    m_sessionModel->setArguments(args);
    m_sessionModel->setAutoClose(true);
    m_sessionModel->setCodec(QTextCodec::codecForName("UTF-8"));
    m_sessionModel->setFlowControlEnabled(true);
    m_sessionModel->setHistoryType(HistoryTypeBuffer(1000));
    m_sessionModel->setDarkBackground(true);
    m_sessionModel->setKeyBindings("");

    m_sessionView = new SessionView(this);
    m_sessionView->setBellMode(SessionView::NotifyBell);
    m_sessionView->setTerminalSizeHint(true);
    m_sessionView->setTripleClickMode(SessionView::SelectWholeLine);
    m_sessionView->setTerminalSizeStartup(true);
    m_sessionView->setRandomSeed(m_sessionModel->sessionId() * 31);
    m_sessionView->setSize(80, 40);
    
    QFont font = QApplication::font(); 
    font.setFamily("Monospace");
    font.setPointSize(10);
    font.setStyleHint(QFont::TypeWriter);
    setTerminalFont(font);  

    m_sessionModel->run();
    m_sessionModel->addView(m_sessionView);
    m_sessionView->setScrollBarPosition(SessionView::ScrollBarRight);

    connect(m_sessionModel, SIGNAL(finished()), this, SLOT(sessionFinished()));
}

QTerminal::~QTerminal()
{
    emit destroyed();
}


void QTerminal::setTerminalFont(QFont &font)
{
    if(!m_sessionView)
	return;
    m_sessionView->setVTFont(font);
}

void QTerminal::setShellProgram(const QString &progname)
{
    if(!m_sessionModel)
	return;
    m_sessionModel->setProgram(progname);
}

void QTerminal::setWorkingDirectory(const QString& dir)
{
    if(!m_sessionModel)
        return;
    m_sessionModel->setInitialWorkingDirectory(dir);
}

void QTerminal::setArgs(QStringList &args)
{
    if (!m_sessionModel)
	return;
    m_sessionModel->setArguments(args);
}

void QTerminal::setTextCodec(QTextCodec *codec)
{
    if(!m_sessionModel)
	return;
    m_sessionModel->setCodec(codec);
}

void QTerminal::setSize(int h, int v)
{
    if(!m_sessionView)
	return;
    m_sessionView->setSize(h, v);
}

void QTerminal::setHistorySize(int lines)
{
    if(lines < 0)
        m_sessionModel->setHistoryType(HistoryTypeFile());
    else
        m_sessionModel->setHistoryType(HistoryTypeBuffer(lines));
}

void QTerminal::setReadOnly(bool readonly)
{
    m_sessionView->setReadOnly(readonly);
}

void QTerminal::resizeEvent(QResizeEvent*)
{
    m_sessionView->resize(this->size());
    m_sessionView->updateImage();
    m_sessionView->update();
}

void QTerminal::sessionFinished()
{
    emit finished();
}

void QTerminal::copyClipboard()
{
    m_sessionView->copyClipboard();
}

void QTerminal::pasteClipboard()
{
    m_sessionView->pasteClipboard();
}

void QTerminal::setFlowControlEnabled(bool enabled)
{
    m_sessionModel->setFlowControlEnabled(enabled);
}

bool QTerminal::flowControlEnabled(void)
{
    return m_sessionModel->flowControlEnabled();
}

void QTerminal::setFlowControlWarningEnabled(bool enabled)
{
    if(flowControlEnabled()) {
        m_sessionView->setFlowControlWarningEnabled(enabled);
    }
}

void QTerminal::setEnvironment(const QStringList& environment)
{
    m_sessionModel->setEnvironment(environment);
}

void* QTerminal::getTerminalDisplay()
{
    return static_cast<void*>(m_sessionView);
}

