/****************************************************************************
** Meta object code from reading C++ file 'ScreenWindow.h'
**
** Created: Thu Apr 7 10:08:42 2011
**      by: The Qt Meta Object Compiler version 62 (Qt 4.7.2)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../lib/ScreenWindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'ScreenWindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.7.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Konsole__ScreenWindow[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
       4,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       3,       // signalCount

 // signals: signature, parameters, type, tag, flags
      23,   22,   22,   22, 0x05,
      44,   39,   22,   22, 0x05,
      58,   22,   22,   22, 0x05,

 // slots: signature, parameters, type, tag, flags
      77,   22,   22,   22, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_Konsole__ScreenWindow[] = {
    "Konsole::ScreenWindow\0\0outputChanged()\0"
    "line\0scrolled(int)\0selectionChanged()\0"
    "notifyOutputChanged()\0"
};

const QMetaObject Konsole::ScreenWindow::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_Konsole__ScreenWindow,
      qt_meta_data_Konsole__ScreenWindow, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &Konsole::ScreenWindow::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *Konsole::ScreenWindow::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *Konsole::ScreenWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Konsole__ScreenWindow))
        return static_cast<void*>(const_cast< ScreenWindow*>(this));
    return QObject::qt_metacast(_clname);
}

int Konsole::ScreenWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: outputChanged(); break;
        case 1: scrolled((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 2: selectionChanged(); break;
        case 3: notifyOutputChanged(); break;
        default: ;
        }
        _id -= 4;
    }
    return _id;
}

// SIGNAL 0
void Konsole::ScreenWindow::outputChanged()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void Konsole::ScreenWindow::scrolled(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void Konsole::ScreenWindow::selectionChanged()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}
QT_END_MOC_NAMESPACE
