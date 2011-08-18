/*
    This file is part of Konsole, an X terminal.

    Copyright 2007-2008 by Robert Knight <robertknight@gmail.com>
    Copyright 1997,1998 by Lars Doelle <lars.doelle@on-line.de>
    Copyright 2009 by Thomas Dreibholz <dreibh@iem.uni-due.de>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
    02110-1301  USA.
*/

#ifndef SESSION_H
#define SESSION_H

// Qt
#include <QtCore/QStringList>
#include <QtCore/QByteRef>
#include <QtCore/QSize>
#include <QUuid>
#include <QWidget>
#include <QHash>
#include <QTextCodec>

class KProcess;
class Pty;

/**
 * Represents a terminal session consisting of a pseudo-teletype and a terminal emulation.
 * The pseudo-teletype (or PTY) handles I/O between the terminal process and Konsole.
 * The terminal emulation ( Emulation and subclasses ) processes the output stream from the
 * PTY and produces a character image which is then shown on views connected to the session.
 *
 * Each Session can be connected to one or more views by using the addView() method.
 * The attached views can then display output from the program running in the terminal
 * or send input to the program in the terminal in the form of keypresses and mouse
 * activity.
 */
class Session:public QObject
{
Q_OBJECT
public:
  /**
   * Constructs a new session.
   *
   * To start the terminal process, call the run() method,
   * after specifying the program and arguments
   * using setProgram() and setArguments()
   *
   * If no program or arguments are specified explicitly, the Session
   * falls back to using the program specified in the SHELL environment
   * variable.
   */
  explicit Session (QObject * parent = 0);
   ~Session ();

  /** 
   * Connect to an existing terminal.  When a new Session() is constructed it 
   * automatically searches for and opens a new teletype.  If you want to 
   * use an existing teletype (given its file descriptor) call this after
   * constructing the session.
   *
   * Calling openTeletype() while a session is running has no effect.
   *
   * @param masterFd The file descriptor of the pseudo-teletype master (See KPtyProcess::KPtyProcess())
   */
  void openTeletype (int masterFd);

  /**
   * Returns true if the session is currently running.  This will be true
   * after run() has been called successfully.
   */
  bool isRunning () const;

  /**
   * Adds a new view for this session.
   *
   * The viewing widget will display the output from the terminal and
   * input from the viewing widget (key presses, mouse activity etc.)
   * will be sent to the terminal.
   *
   * Views can be removed using removeView().  The session is automatically
   * closed when the last view is removed.
   */
  //void addView (TerminalDisplay * widget);
  /**
   * Removes a view from this session.  When the last view is removed,
   * the session will be closed automatically.
   *
   * @p widget will no longer display output from or send input
   * to the terminal
   */
  //void removeView (TerminalDisplay * widget);

  /**
   * Returns the views connected to this session
   */
   // QList < TerminalDisplay * >views () const;

  /**
   * Returns the terminal emulation instance being used to encode / decode
   * characters to / from the process.
   */
  //Emulation *emulation () const;

  /** Returns the arguments passed to the shell process when run() is called. */
  QStringList arguments () const;
  /** Returns the program name of the shell process started when run() is called. */
  QString program () const;

  /**
   * Sets the command line arguments which the session's program will be passed when
   * run() is called.
   */
  void setArguments (const QStringList & arguments);
  /** Sets the program to be executed when run() is called. */
  void setProgram (const QString & program);

  /** Returns the session's current working directory. */
  QString initialWorkingDirectory ()
  {
    return _initialWorkingDir;
  }

  /**
   * Sets the initial working directory for the session when it is run
   * This has no effect once the session has been started.
   */
  void setInitialWorkingDirectory (const QString & dir);


  /**
   * This enum describes the available title roles.
   */
  enum TitleRole
  {
      /** The name of the session. */
    NameRole,
      /** The title of the session which is displayed in tabs etc. */
    DisplayedTitleRole
  };

  /**
   * Return the session title set by the user (ie. the program running
   * in the terminal), or an empty string if the user has not set a custom title
   */
  QString userTitle () const;

  /** Convenience method used to read the name property.  Returns title(Session::NameRole). */
  QString nameTitle () const
  {
    return title (Session::NameRole);
  }

  /** Sets the name of the icon associated with this session. */
  void setIconName (const QString & iconName);
  /** Returns the name of the icon associated with this session. */
  QString iconName () const;


  /** Sets the text of the icon associated with this session. */
  void setIconText (const QString & iconText);
  /** Returns the text of the icon associated with this session. */
  QString iconText () const;

  /** Sets the session's title for the specified @p role to @p title. */
  void setTitle (TitleRole role, const QString & title);

  /** Returns the session's title for the specified @p role. */
  QString title (TitleRole role) const;

  /**
   * Specifies whether to close the session automatically when the terminal
   * process terminates.
   */
  void setAutoClose (bool b)
  {
    _autoClose = b;
  }

  /** Returns true if the user has started a program in the session. */
  bool isForegroundProcessActive ();

  /** Returns the terminal session's window size in lines and columns. */
  QSize size ();
  /**
   * Emits a request to resize the session to accommodate
   * the specified window size.
   *
   * @param size The size in lines and columns to request.
   */
  void setSize (const QSize & size);

  /**
   * Sets whether the session has a dark background or not.  The session
   * uses this information to set the COLORFGBG variable in the process's
   * environment, which allows the programs running in the terminal to determine
   * whether the background is light or dark and use appropriate colors by default.
   *
   * This has no effect once the session is running.
   */
  void setDarkBackground (bool darkBackground);
  /**
   * Returns true if the session has a dark background.
   * See setDarkBackground()
   */
  bool hasDarkBackground () const;

  /**
   * Attempts to get the shell program to redraw the current display area.
   * This can be used after clearing the screen, for example, to get the
   * shell to redraw the prompt line.
   */
  void refresh ();

 /** 
   * Possible values of the @p what parameter for setUserTitle()
   * See "Operating System Controls" section on http://rtfm.etla.org/xterm/ctlseq.html 
   */
  enum UserTitleChange
  {
    IconNameAndWindowTitle = 0,
    IconName = 1,
    WindowTitle = 2,
    TextColor = 10,
    BackgroundColor = 11,
    SessionName = 30,
    ProfileChange = 50		// this clashes with Xterm's font change command
  };

  // Sets the text codec used by this sessions terminal emulation.
  void setCodec (QTextCodec * codec);

  public slots:
  /**
   * Starts the terminal session.
   *
   * This creates the terminal process and connects the teletype to it.
   */
  void run ();

  /**
   * Returns the environment of this session as a list of strings like
   * VARIABLE=VALUE
   */
  QStringList environment () const;

  /**
   * Sets the environment for this session.
   * @p environment should be a list of strings like
   * VARIABLE=VALUE
   */
  void setEnvironment (const QStringList & environment);

  /**
   * Closes the terminal session.  This sends a hangup signal
   * (SIGHUP) to the terminal process and causes the finished()  
   * signal to be emitted.  If the process does not respond to the SIGHUP signal
   * then the terminal connection (the pty) is closed and Konsole waits for the 
   * process to exit.
   */
  void close ();

  /**
   * Changes the session title or other customizable aspects of the terminal
   * emulation display. For a list of what may be changed see the
   * Emulation::titleChanged() signal.
   *
   * @param what The feature being changed.  Value is one of UserTitleChange
   * @param caption The text part of the terminal command
   */
  void setUserTitle (int what, const QString & caption);

  /**
   * Sets whether flow control is enabled for this terminal
   * session.
   */
  void setFlowControlEnabled (bool enabled);

  /** Returns whether flow control is enabled for this terminal session. */
  bool flowControlEnabled () const;

  /**
   * Sends @p text to the current foreground terminal program.
   */
  void sendText (const QString & text) const;

   /**
    * Sends a mouse event of type @p eventType emitted by button
    * @p buttons on @p column/@p line to the current foreground
    * terminal program
    */
  void sendMouseEvent (int buttons, int column, int line,
				    int eventType);

  /** Sets the text codec used by this sessions terminal emulation.
    * Overloaded to accept a QByteArray for convenience since DBus
    * does not accept QTextCodec directky.
    */
  bool setCodec (QByteArray codec);

  /** Returns the codec used to decode incoming characters in this
   * terminal emulation
   */
  QByteArray codec ();

  /** Sets the session's title for the specified @p role to @p title.
   *  This is an overloaded member function for setTitle(TitleRole, QString)
   *  provided for convenience since enum data types may not be
   *  exported directly through DBus
   */
  void setTitle (int role, const QString & title);

  /** Returns the session's title for the specified @p role.
   * This is an overloaded member function for  setTitle(TitleRole)
   * provided for convenience since enum data types may not be
   * exported directly through DBus
   */
  QString title (int role) const;

signals:

  /** Emitted when the terminal process starts. */
  void started ();

  /**
   * Emitted when the terminal process exits.
   */
  void finished ();

  /**
   * Emitted when output is received from the terminal process.
   */
  void receivedData (const QString & text);

  /** Emitted when the session's title has changed. */
  void titleChanged ();

  /**
   * Emitted when the activity state of this session changes.
   *
   * @param state The new state of the session.  This may be one
   * of NOTIFYNORMAL, NOTIFYSILENCE or NOTIFYACTIVITY
   */
  void stateChanged (int state);

  /**
   * Requests that the color the text for any tabs associated with
   * this session should be changed;
   *
   * TODO: Document what the parameter does
   */
  void changeTabTextColorRequest (int);

  /**
   * Requests that the background color of views on this session
   * should be changed.
   */
  void changeBackgroundColorRequest (const QColor &);
  /** 
   * Requests that the text color of views on this session should
   * be changed to @p color.
   */
  void changeForegroundColorRequest (const QColor &);

  /** TODO: Document me. */
  void openUrlRequest (const QString & url);


  /**
   * Emitted when the terminal process requests a change
   * in the size of the terminal window.
   *
   * @param size The requested window size in terms of lines and columns.
   */
  void resizeRequest (const QSize & size);

  /**
   * Emitted when a profile change command is received from the terminal.
   *
   * @param text The text of the command.  This is a string of the form
   * "PropertyName=Value;PropertyName=Value ..."
   */
  void profileChangeCommandReceived (const QString & text);

 /**
  * Emitted when the flow control state changes.
  *
  * @param enabled True if flow control is enabled or false otherwise.
  */
  void flowControlEnabledChanged (bool enabled);

private slots:
  void done (int);

  void onReceiveBlock (const char *buffer, int len);
  void monitorTimerDone ();

  void onViewSizeChange (int height, int width);

  void activityStateSet (int);

  //automatically detach views from sessions when view is destroyed
  //void viewDestroyed (QObject * view);

  void updateFlowControlState (bool suspended);
  void updateWindowSize (int lines, int columns);
private:

  void updateTerminalSize ();
  bool kill (int signal);
  // print a warning message in the terminal.  This is used
  // if the program fails to start, or if the shell exits in 
  // an unsuccessful manner
  void terminalWarning (const QString & message);
  // checks that the binary 'program' is available and can be executed
  // returns the binary name if available or an empty string otherwise
  QString checkProgram (const QString & program) const;

  QUuid _uniqueIdentifier;	// SHELL_SESSION_ID

  Pty *_shellProcess;
  //Emulation *_emulation;

  //QList < TerminalDisplay * >_views;

  bool _monitorActivity;
  bool _monitorSilence;
  bool _notifiedActivity;
  bool _masterMode;
  bool _autoClose;
  bool _wantedClose;
  QTimer *_monitorTimer;

  int _silenceSeconds;

  QString _nameTitle;
  QString _displayTitle;
  QString _userTitle;

  QString _localTabTitleFormat;
  QString _remoteTabTitleFormat;

  QString _iconName;
  QString _iconText;		// as set by: echo -en '\033]1;IconText\007
  bool _flowControl;
  bool _fullScripting;

  QString _program;
  QStringList _arguments;

  QStringList _environment;
  int _sessionId;

  QString _initialWorkingDir;
  QString _currentWorkingDir;

  int _foregroundPid;

  QColor _modifiedBackground;	// as set by: echo -en '\033]11;Color\007

  QString _profileKey;

  bool _hasDarkBackground;

  static int lastSessionId;

};

/**
 * Provides a group of sessions which is divided into master and slave sessions.
 * Activity in master sessions can be propagated to all sessions within the group.
 * The type of activity which is propagated and method of propagation is controlled
 * by the masterMode() flags.
 */
class SessionGroup:public QObject
{
Q_OBJECT public:
    /** Constructs an empty session group. */
  SessionGroup (QObject * parent);
    /** Destroys the session group and removes all connections between master and slave sessions. */
  ~SessionGroup ();

    /** Adds a session to the group. */
  void addSession (Session * session);
    /** Removes a session from the group. */
  void removeSession (Session * session);

    /** Returns the list of sessions currently in the group. */
    QList < Session * >sessions () const;

    /**
     * Sets whether a particular session is a master within the group.
     * Changes or activity in the group's master sessions may be propagated
     * to all the sessions in the group, depending on the current masterMode()
     *
     * @param session The session whoose master status should be changed.
     * @param master True to make this session a master or false otherwise
     */
  void setMasterStatus (Session * session, bool master);
    /** Returns the master status of a session.  See setMasterStatus() */
  bool masterStatus (Session * session) const;

    /**
     * This enum describes the options for propagating certain activity or
     * changes in the group's master sessions to all sessions in the group.
     */
  enum MasterMode
  {
	/**
         * Any input key presses in the master sessions are sent to all
         * sessions in the group.
         */
    CopyInputToAll = 1
  };

    /**
     * Specifies which activity in the group's master sessions is propagated
     * to all sessions in the group.
     *
     * @param mode A bitwise OR of MasterMode flags.
     */
  void setMasterMode (int mode);
    /**
     * Returns a bitwise OR of the active MasterMode flags for this group.
     * See setMasterMode()
     */
  int masterMode () const;

  private slots:void sessionFinished ();
  void forwardData (const char *data, int size);

private:
    QList < Session * >masters () const;

  // maps sessions to their master status
    QHash < Session *, bool > _sessions;

  int _masterMode;
};

#endif
