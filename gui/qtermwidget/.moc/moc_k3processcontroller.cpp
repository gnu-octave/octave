/****************************************************************************
** Meta object code from reading C++ file 'k3processcontroller.h'
**
** Created: Thu Apr 7 10:08:48 2011
**      by: The Qt Meta Object Compiler version 62 (Qt 4.7.2)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../lib/k3processcontroller.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'k3processcontroller.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.7.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_K3ProcessController[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      21,   20,   20,   20, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_K3ProcessController[] = {
    "K3ProcessController\0\0slotDoHousekeeping()\0"
};

const QMetaObject K3ProcessController::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_K3ProcessController,
      qt_meta_data_K3ProcessController, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &K3ProcessController::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *K3ProcessController::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *K3ProcessController::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_K3ProcessController))
        return static_cast<void*>(const_cast< K3ProcessController*>(this));
    return QObject::qt_metacast(_clname);
}

int K3ProcessController::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: slotDoHousekeeping(); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
