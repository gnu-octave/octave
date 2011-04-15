#ifndef HISTORYDOCKWIDGET_H
#define HISTORYDOCKWIDGET_H

#include <QDockWidget>
#include <QListView>
#include <QStringListModel>

// Octave includes
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#undef PACKAGE_URL
#include "octave/config.h"

#include "octave/debug.h"
#include "octave/octave.h"
#include "octave/symtab.h"
#include "octave/parse.h"
#include "octave/unwind-prot.h"
#include "octave/toplev.h"
#include "octave/load-path.h"
#include "octave/error.h"
#include "octave/quit.h"
#include "octave/variables.h"
#include "octave/sighandlers.h"
#include "octave/sysdep.h"
#include "octave/str-vec.h"
#include "octave/cmd-hist.h"
#include "octave/cmd-edit.h"
#include "octave/oct-env.h"
#include "octave/symtab.h"
#include "cmd-edit.h"

class HistoryDockWidget : public QDockWidget {
    Q_OBJECT
public:
    HistoryDockWidget(QWidget *parent = 0);
    void updateHistory(string_vector historyEntries);

signals:
    void information(QString message);

private:
    void construct();
    QListView *m_historyListView;
    QStringListModel *m_historyListModel;
};

#endif // HISTORYDOCKWIDGET_H
