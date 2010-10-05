#include "gpsd.h"
#include "../log.h"

namespace GPSDevice
{
   const QString GPSD::DefaultHost = "localhost";
   const quint16 GPSD::DefaultPort = 2947;

   GPSD::GPSD(QString hostname, quint16 port, QObject * parent) :
      NMEADevice(parent),
      m_hostname(hostname),
      m_port(port)
   {
      connect(&m_socket, SIGNAL(connected()), this, SLOT(enableNMEA()));
      connect(&m_socket, SIGNAL(readyRead()), this, SLOT(socketReadyRead()));
      connect(&m_socket, SIGNAL(stateChanged(QAbstractSocket::SocketState)),
            this, SLOT(socketStateChanged(QAbstractSocket::SocketState)));
   }

   void GPSD::openDevice()
   {
      DEBUG("GPSD| openDevice = { host: %s, port: %u }", m_hostname.toAscii().data(), m_port);
      m_socket.connectToHost(m_hostname, m_port);
   }

   void GPSD::closeDevice()
   {
      DEBUG("GPSD| closeDevice");
      m_socket.disconnectFromHost();
   }

   QString GPSD::type() const
   {
      return "GPSD";
   }

   void GPSD::socketStateChanged(QAbstractSocket::SocketState state)
   {
      DEBUG("GPSD| socketStateChanged = { state: %d }", state);
      switch (state)
      {
         case QAbstractSocket::UnconnectedState:
            setState(GPSDevice::Disconnected);
            break;

         case QAbstractSocket::ConnectingState:
            setState(GPSDevice::Connecting);
            break;

         case QAbstractSocket::ConnectedState:
            setState(GPSDevice::Connected);
            break;

         default:
            break;
      }
   }

   void GPSD::enableNMEA()
   {
      DEBUG("GPSD| enableNMEA");
      m_socket.write("r\n");
   }

   void GPSD::socketReadyRead()
   {
      while (m_socket.canReadLine())
         parseLine(m_socket.readLine());
   }
}
