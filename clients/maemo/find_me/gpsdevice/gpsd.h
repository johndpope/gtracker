#ifndef __GPSDEVICE_GPSD_H
#define __GPSDEVICE_GPSD_H

#include "nmea_device.h"
#include <QTcpSocket>

namespace GPSDevice
{
   class GPSD : public NMEADevice
   {
      Q_OBJECT
      public:
         static const QString DefaultHost;
         static const quint16 DefaultPort;

      public:
         GPSD(QString hostname = DefaultHost, quint16 port = DefaultPort, QObject * parent = 0);

         virtual void openDevice();
         virtual void closeDevice();

         virtual QString type() const;

      private slots:
         void socketStateChanged(QAbstractSocket::SocketState state);
         void enableNMEA();
         void socketReadyRead();

      private:
         QString     m_hostname;
         quint16     m_port;
         QTcpSocket  m_socket;
   };
}

#endif // __GPSDEVICE_GPSD_H
