TEMPLATE = app
TARGET = findme
QT += network
CONFIG += qt warn_on release
#DEFINES += FIND_ME_NO_DEBUG

RESOURCES += default.qrc

include(gtracker/gtracker.pri)
include(gpsdevice/gpsdevice.pri)

HEADERS += \
   info_widget.h log.h window.h

SOURCES += \
   info_widget.cpp main.cpp window.cpp

SOURCES += \
   log.c

unix {
   isEmpty(PREFIX) {
      PREFIX = /usr/local
   }

   BINDIR = $$PREFIX/bin
   DATADIR = $$PREFIX/share

   DEFINES += DATADIR=\"$$DATADIR\" PKGDATADIR=\"$$PKGDATADIR\"

   #MAKE INSTALL

   INSTALLS += target desktop icon26 icon40 icon64 scalable

   target.path = $$BINDIR

   desktop.path = $$DATADIR/applications/hildon
   desktop.files += deb/$${TARGET}.desktop

   icon26.path = $$DATADIR/icons/hicolor/26x26/hildon/
   icon26.files += deb/26x26/$${TARGET}.png

   icon40.path = $$DATADIR/icons/hicolor/26x26/hildon/
   icon40.files += deb/40x40/$${TARGET}.png

   icon64.path = $$DATADIR/icons/hicolor/64x64/apps
   icon64.files += deb/64x64/$${TARGET}.png

   scalable.path = $$DATADIR/icons/hicolor/scalable/hildon
   scalable.files += deb/scalable/$${TARGET}.png
}
