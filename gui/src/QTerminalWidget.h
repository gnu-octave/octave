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

#ifndef QTERMINALWIDGET_H
#define QTERMINALWIDGET_H

#include <QtGui>

struct TermWidgetImpl;
/**
  * \class QTerminalWidget
  * This class forms a widget class that can be inserted into other widgets.
  */
class QTerminalWidget : public QWidget
{
    Q_OBJECT
public:
    /**
      * \enum ColorScheme
      * Different color schemes for the terminal.
      */
    enum ColorScheme {
        WhiteOnBlack,
        GreenOnBlack,
        BlackOnLightYellow
    };

    /**
      * \enum ScrollBarPosition
      * Defines the scrollbar position of the terminal.
      */
    enum ScrollBarPosition
    {
        NoScrollBar,
        ScrollBarLeft,
        ScrollBarRight
    };

    QTerminalWidget(int startnow = 1, QWidget *parent = 0);
    ~QTerminalWidget();

    void startShellProgram();
    void openTeletype(int fd);

    /** Default is application font with family Monospace, size 10. */
    void setTerminalFont(QFont &font); 
    
    /**	Shell program, default is /bin/bash. */
    void setShellProgram(QString progname);
    
    /** Shell program args, default is none. */
    void setArgs(QStringList &args);
    
    /** Text codec, default is UTF-8. */
    void setTextCodec(QTextCodec *codec);
    
    /** Resize terminal widget. */
    void setSize(int h, int v);
    
    /** History size for scrolling, values below zero mean infinite. */
    void setHistorySize(int lines);

    /** Presence of scrollbar. By default, there is no scrollbar present. */
    void setScrollBarPosition(ScrollBarPosition);
    
    /** Send some text to the terminal. */
    void sendText(QString &text);
            
signals:
    /** Emitted, when the current program has finished. */
    void finished();
        
protected: 
    virtual void resizeEvent(QResizeEvent *);
    
protected slots:
    void sessionFinished();        
    
private:
    /** Performs initial operations on this widget. */
    void initialize();
    TermWidgetImpl *m_impl;
};

#endif // QTERMINALWIDGET_H
