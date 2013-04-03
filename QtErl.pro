#-------------------------------------------------
#
# Project created by QtCreator 2013-03-26T11:50:03
#
#-------------------------------------------------

TARGET = QtErl
TEMPLATE = lib
QT += widgets uitools

DEFINES += QTERL_LIBRARY

SOURCES += \
    c_src/qteconnection.cpp \
    c_src/qteevent.cpp \
    c_src/qterl.cpp \
    c_src/qterl_drv.cpp

HEADERS += \
    c_src/qteconnection.h \
    c_src/qteevent.h \
    c_src/qterl.h \
    c_src/qterl_commands.h \
    c_src/qterl_drv.h

unix:!symbian {
    maemo5 {
        target.path = /opt/usr/lib
    } else {
        target.path = /usr/lib
    }
    INSTALLS += target
}

OTHER_FILES += \
    README.md \
    LICENSE \
    src/qte.erl \
    src/qte_xml.erl \
    include/qte_xml.hrl \
    Makefile \
    Emakefile \
    test/Makefile \
    test/Emakefile \
    example/addrbook.erl \
    example/Makefile \
    example/Emakefile


win32: INCLUDEPATH += $$PWD/../../../../erl/5.9.3.1/erts-5.9.3.1/include
win32: INCLUDEPATH += $$PWD/../../../../erl/5.9.3.1/lib/erl_interface-3.7.9/include
win32: DEPENDPATH += $$PWD/../../../../erl/5.9.3.1/lib/erl_interface-3.7.9/include
win32: LIBS += -L$$PWD/../../../../erl/5.9.3.1/lib/erl_interface-3.7.9/lib -lei
win32: PRE_TARGETDEPS += $$PWD/../../../../erl/5.9.3.1/lib/erl_interface-3.7.9/lib/ei.lib

## Note to self: stay clear of erl_interface. That is a major headache to link against!

FORMS += \
    example/addrbook.ui
