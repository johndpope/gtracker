#ifndef __FIND_ME_WINDOW_H
#define __FIND_ME_WINDOW_H

#include "gtracker/client.h"
#include "gpsdevice/gpsdevice.h"
#include "info_widget.h"
#include <QMainWindow>
#include <QLabel>

namespace FindMe
{
   using ::GTracker::Client;
   using ::GPSDevice::GPSDevice;

   class Window : public QMainWindow
   {
      Q_OBJECT
      Q_DISABLE_COPY(Window)

      public:
         Window(QWidget * parent = 0);
         ~Window();

      private:
         void createInterface();

      private slots:
         void clientReconnect();
         void clientUserIdChanged(QString userId);
         void clientStateChanged(GTracker::Client::State state);
         void clientError(int code);

         void gpsReconnect();
         void gpsStateChanged(GPSDevice::GPSDevice::State state);
         void gpsFixChanged(GPSDevice::GPSDevice::Fix fix);
         void gpsSatellitesChanged(int inView, int used);
         void gpsSpeedChanged(int speed);

      private:
         QString     m_client_host;
         quint16     m_client_port;
         int         m_client_interval;
         int         m_gps_interval;

         Client *    m_client;
         GPSDevice * m_gps;

         QLabel      m_user_id;
         QLabel      m_client_state;
         QLabel      m_gps_state;
         QLabel      m_gps_fix;
         QLabel      m_gps_sat;
         QLabel      m_gps_speed;
         QLabel      m_error_msg;

         InfoWidget  m_info;
   };
} // namespace FindMe

#endif // __FIND_ME_WINDOW_H
