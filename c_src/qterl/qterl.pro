#-------------------------------------------------
#
# Project created by QtCreator 2013-05-07T13:51:18
#
#-------------------------------------------------

TARGET = qterl
TEMPLATE = lib

QT += widgets uitools

DEFINES += QTERL_LIBRARY

SOURCES += qterl.cpp \
    qterl_drv.cpp \
    qterlstate.cpp

HEADERS += qterl.h \
    qterl_drv.h \
    qterlstate.h

unix:!symbian {
    maemo5 {
        target.path = /opt/usr/lib
    } else {
        target.path = /usr/lib
    }
    INSTALLS += target
}

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../libqte/release/ -lqte
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../libqte/debug/ -lqte
else:unix: LIBS += -L$$OUT_PWD/../libqte/ -lqte

INCLUDEPATH += $$PWD/../libqte
DEPENDPATH += $$PWD/../libqte

win32:CONFIG(release, debug|release): PRE_TARGETDEPS += $$OUT_PWD/../libqte/release/libqte.a
else:win32:CONFIG(debug, debug|release): PRE_TARGETDEPS += $$OUT_PWD/../libqte/debug/libqte.a
else:unix: PRE_TARGETDEPS += $$OUT_PWD/../libqte/libqte.a


##
## Erlang libs
##
## TODO: make this less hard coded...
##

win32: INCLUDEPATH += D:/erl/5.9.3.1/erts-5.9.3.1/include
win32: INCLUDEPATH += D:/erl/5.9.3.1/lib/erl_interface-3.7.9/include
win32: DEPENDPATH += D:/erl/5.9.3.1/lib/erl_interface-3.7.9/include
win32: LIBS += -LD:/erl/5.9.3.1/lib/erl_interface-3.7.9/lib -lei
win32: PRE_TARGETDEPS += D:/erl/5.9.3.1/lib/erl_interface-3.7.9/lib/ei.lib

## Note to self: stay clear of erl_interface. That is a major headache to link against!
