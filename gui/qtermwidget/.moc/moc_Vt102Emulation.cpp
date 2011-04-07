/****************************************************************************
** Meta object code from reading C++ file 'Vt102Emulation.h'
**
** Created: Thu Apr 7 10:08:43 2011
**      by: The Qt Meta Object Compiler version 62 (Qt 4.7.2)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../lib/Vt102Emulation.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'Vt102Emulation.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.7.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Konsole__Vt102Emulation[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      33,   25,   24,   24, 0x0a,
      61,   24,   24,   24, 0x2a,
      90,   85,   24,   24, 0x0a,
     108,   24,   24,   24, 0x0a,
     163,  133,   24,   24, 0x0a,
     195,   24,   24,   24, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_Konsole__Vt102Emulation[] = {
    "Konsole::Vt102Emulation\0\0,length\0"
    "sendString(const char*,int)\0"
    "sendString(const char*)\0text\0"
    "sendText(QString)\0sendKeyEvent(QKeyEvent*)\0"
    "buttons,column,line,eventType\0"
    "sendMouseEvent(int,int,int,int)\0"
    "updateTitle()\0"
};

const QMetaObject Konsole::Vt102Emulation::staticMetaObject = {
    { &Emulation::staticMetaObject, qt_meta_stringdata_Konsole__Vt102Emulation,
      qt_meta_data_Konsole__Vt102Emulation, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &Konsole::Vt102Emulation::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *Konsole::Vt102Emulation::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *Konsole::Vt102Emulation::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Konsole__Vt102Emulation))
        return static_cast<void*>(const_cast< Vt102Emulation*>(this));
    return Emulation::qt_metacast(_clname);
}

int Konsole::Vt102Emulation::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = Emulation::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: sendString((*reinterpret_cast< const char*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 1: sendString((*reinterpret_cast< const char*(*)>(_a[1]))); break;
        case 2: sendText((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 3: sendKeyEvent((*reinterpret_cast< QKeyEvent*(*)>(_a[1]))); break;
        case 4: sendMouseEvent((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2])),(*reinterpret_cast< int(*)>(_a[3])),(*reinterpret_cast< int(*)>(_a[4]))); break;
        case 5: updateTitle(); break;
        default: ;
        }
        _id -= 6;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
