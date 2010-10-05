#ifndef __GPSDEVICE_MAEMO_INTERNAL_H
#define __GPSDEVICE_MAEMO_INTERNAL_H

#include <QSocketNotifier>
#include <QFile>
#include <termios.h>
#include "nmea_device.h"

namespace GPSDevice
{
   class MaemoInternal : public NMEADevice
   {
      Q_OBJECT
      public:
         static const QString DefaultDevice;

      public:
         MaemoInternal(QString device = DefaultDevice, bool autoEnable = true, QObject * parent = 0);
         virtual ~MaemoInternal();

         virtual void openDevice();
         virtual void closeDevice();

         virtual QString type() const;

      private:
         void enableGPS(bool val);
         void close();
         void parseBuffer();
         ssize_t bytesAvailable();

      private slots:
         void activated(int fd);

      private:
         QString m_device;
         bool m_auto_enable;
         int m_fd;
         QSocketNotifier * m_notifier;
         QString m_buffer;
         termios m_prev;
   };
}

#endif // __GPSDEVICE_MAEMO_INTERNAL_H
