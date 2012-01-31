/*  Copyright (C) 2008 e_k (e_k@users.sourceforge.net)
    Copyright (C) 2012 Jacob Dawid <jacob.dawid@googlemail.com>

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
						
#include <QDebug>

#include "unix/QUnixTerminalImpl.h"
#include "unix/kpty.h"

#include <termios.h>

QUnixTerminalImpl::QUnixTerminalImpl(QWidget *parent)
    : QTerminalInterface(parent) {
    setMinimumSize(600, 400);
    initialize();
}

void QUnixTerminalImpl::initialize()
{
    m_kpty = new KPty();
    m_kpty->open();

    m_sessionModel = new TerminalModel(m_kpty);

    m_sessionModel->setAutoClose(true);
    m_sessionModel->setCodec(QTextCodec::codecForName("UTF-8"));
    m_sessionModel->setHistoryType(HistoryTypeBuffer(1000));
    m_sessionModel->setDarkBackground(true);
    m_sessionModel->setKeyBindings("");

    m_sessionView = new TerminalView(this);
    m_sessionView->setBellMode(TerminalView::NotifyBell);
    m_sessionView->setTerminalSizeHint(true);
    m_sessionView->setTripleClickMode(TerminalView::SelectWholeLine);
    m_sessionView->setTerminalSizeStartup(true);
    m_sessionView->setSize(80, 40);
    
    QFont font = QApplication::font(); 
    font.setFamily("Monospace");
    font.setPointSize(10);
    font.setStyleHint(QFont::TypeWriter);
    setTerminalFont(font);  

    m_sessionModel->run();
    m_sessionModel->addView(m_sessionView);
    m_sessionView->setScrollBarPosition(TerminalView::ScrollBarRight);

    setFocusProxy(m_sessionView);

    setFocus(Qt::OtherFocusReason);
    m_sessionView->resize(this->size());

    connectToPty();
}

void QUnixTerminalImpl::connectToPty()
{
    int fds = m_kpty->slaveFd();

    dup2 (fds, STDIN_FILENO);
    dup2 (fds, STDOUT_FILENO);
    dup2 (fds, STDERR_FILENO);

    if(!isatty(STDIN_FILENO)) {
        qDebug("Error: stdin is not a tty.");
    }

    if(!isatty(STDOUT_FILENO)) {
        qDebug("Error: stdout is not a tty.");
    }

    if(!isatty(STDERR_FILENO)) {
        qDebug("Error: stderr is not a tty.");
    }
}

QUnixTerminalImpl::~QUnixTerminalImpl()
{
    emit destroyed();
}

void QUnixTerminalImpl::setTerminalFont(QFont &font)
{
    if(!m_sessionView)
	return;
    m_sessionView->setVTFont(font);
}

void QUnixTerminalImpl::setSize(int h, int v)
{
    if(!m_sessionView)
	return;
    m_sessionView->setSize(h, v);
}

void QUnixTerminalImpl::sendText(QString text)
{
    m_sessionModel->sendText(text);
}

void QUnixTerminalImpl::focusInEvent(QFocusEvent *focusEvent)
{
    Q_UNUSED(focusEvent);
    m_sessionView->updateImage();
    m_sessionView->repaint();
    m_sessionView->update();
}

void QUnixTerminalImpl::showEvent(QShowEvent *)
{
    m_sessionView->updateImage();
    m_sessionView->repaint();
    m_sessionView->update();
}

void QUnixTerminalImpl::resizeEvent(QResizeEvent*)
{
    m_sessionView->resize(this->size());
    m_sessionView->updateImage();
    m_sessionView->repaint();
    m_sessionView->update();
}

void QUnixTerminalImpl::copyClipboard()
{
    m_sessionView->copyClipboard();
}

void QUnixTerminalImpl::pasteClipboard()
{
    m_sessionView->pasteClipboard();
}

