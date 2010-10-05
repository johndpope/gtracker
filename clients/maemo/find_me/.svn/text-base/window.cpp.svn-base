#include "window.h"
#include "log.h"
#include "gpsdevice/make.h"
#include <QTimer>
#include <QStatusBar>
#include <QGridLayout>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QDebug>
#include <QSettings>
#include <QCoreApplication>
#include <QMessageBox>

namespace FindMe
{
   namespace
   {
      const QString NoID      ("No ID");
      const QString NoError   ("No Error");
   }

   Window::Window(QWidget * parent) :
      QMainWindow(parent)
   {
      createInterface();

      QSettings cfg;

      m_client_host     = cfg.value("GTracker/hostname", "gtracker.ru").toString();
      m_client_port     = cfg.value("GTracker/port", 7777).toUInt();
      m_client_interval = cfg.value("GTracker/interval", 5000).toInt();
      m_gps_interval    = cfg.value("GPS/interval", 5000).toInt();

      m_client = new Client(cfg.value("GTracker/last_id", "").toString(), this);
      m_gps = ::GPSDevice::make(cfg);
      m_gps->setParent(this);

      connect(m_client, SIGNAL(userIdChanged(QString)), this, SLOT(clientUserIdChanged(QString)));
      connect(m_client, SIGNAL(stateChanged(GTracker::Client::State)),
            this, SLOT(clientStateChanged(GTracker::Client::State)));
      connect(m_client, SIGNAL(error(int)), this, SLOT(clientError(int)));

      connect(m_gps, SIGNAL(stateChanged(GPSDevice::GPSDevice::State)),
            this, SLOT(gpsStateChanged(GPSDevice::GPSDevice::State)));
      connect(m_gps, SIGNAL(positionChanged(double, double, QDateTime)),
            m_client, SLOT(updatePosition(double, double, QDateTime)));
      connect(m_gps, SIGNAL(fixChanged(GPSDevice::GPSDevice::Fix)),
            this, SLOT(gpsFixChanged(GPSDevice::GPSDevice::Fix)));
      connect(m_gps, SIGNAL(satellitesChanged(int, int)),
            this, SLOT(gpsSatellitesChanged(int, int)));
      connect(m_gps, SIGNAL(speedChanged(int)), this, SLOT(gpsSpeedChanged(int)));

      connect(&m_info, SIGNAL(cancelClicked()), &m_info, SLOT(hide()));
      connect(&m_info, SIGNAL(okClicked()), &m_info, SLOT(hide()));
      connect(&m_info, SIGNAL(okClicked()), m_client, SLOT(resetId()));

      clientUserIdChanged(m_client->userId());
      clientError(0);
      clientReconnect();
      gpsFixChanged(GPSDevice::FixNo);
      gpsSatellitesChanged(0, 0);
      gpsSpeedChanged(0);
      gpsReconnect();
   }

   Window::~Window()
   {
      m_gps->closeDevice();
      m_client->disconnectFromHost();

      QSettings cfg;
      cfg.setValue("GTracker/hostname",   m_client_host);
      cfg.setValue("GTracker/port",       m_client_port);
      cfg.setValue("GTracker/interval",   m_client_interval);
      cfg.setValue("GTracker/last_id",    m_client->userId());
      cfg.setValue("GPS/type",            m_gps->type());
      cfg.setValue("GPS/interval",        m_gps_interval);
   }

   void Window::createInterface()
   {
      m_info.hide();

      setObjectName("Window");
      m_user_id.setObjectName("State");
      m_client_state.setObjectName("State");
      m_gps_state.setObjectName("State");
      m_gps_fix.setObjectName("State");
      m_gps_sat.setObjectName("State");
      m_gps_speed.setObjectName("State");
      m_error_msg.setObjectName("State");

      QGridLayout * state_layout = new QGridLayout;
      state_layout->addWidget(new QLabel("Your ID:", this),                1, 0, Qt::AlignLeft);
      state_layout->addWidget(&m_user_id,                                  1, 1, Qt::AlignRight);
      state_layout->addWidget(new QLabel("Server state:", this),           2, 0, Qt::AlignLeft);
      state_layout->addWidget(&m_client_state,                             2, 1, Qt::AlignRight);
      state_layout->addWidget(new QLabel("GPS state:", this),              3, 0, Qt::AlignLeft);
      state_layout->addWidget(&m_gps_state,                                3, 1, Qt::AlignRight);
      state_layout->addWidget(new QLabel("GPS Fix:", this),                4, 0, Qt::AlignLeft);
      state_layout->addWidget(&m_gps_fix,                                  4, 1, Qt::AlignRight);
      state_layout->addWidget(new QLabel("Satellite used/in view:", this), 5, 0, Qt::AlignLeft);
      state_layout->addWidget(&m_gps_sat,                                  5, 1, Qt::AlignRight);
      state_layout->addWidget(new QLabel("Speed:", this),                  6, 0, Qt::AlignLeft);
      state_layout->addWidget(&m_gps_speed,                                6, 1, Qt::AlignRight);
      state_layout->addWidget(new QLabel("Error:", this),                  7, 0, Qt::AlignLeft);
      state_layout->addWidget(&m_error_msg,                                7, 1, Qt::AlignRight);

      QVBoxLayout * main_layout = new QVBoxLayout;
      main_layout->addLayout(state_layout, 10);
      main_layout->addWidget(&m_info, 1);

      QWidget * widget = new QWidget(this);
      widget->setLayout(main_layout);

      setCentralWidget(widget);
      setWindowTitle(QCoreApplication::applicationName());
   }

   void Window::clientReconnect()
   {
      m_client->connectToHost(m_client_host, m_client_port);
      clientError(0);
   }

   void Window::clientUserIdChanged(QString userId)
   {
      if (userId.isEmpty())
         m_user_id.setText(NoID);
      else
         m_user_id.setText(userId);
   }

   void Window::clientStateChanged(GTracker::Client::State state)
   {
      switch (state)
      {
         case Client::Disconnected:
            m_client_state.setText("Disconnected");
            QTimer::singleShot(m_client_interval, this, SLOT(clientReconnect()));
            break;

         case Client::Connecting:
            m_client_state.setText("Connecting");
            break;

         case Client::Authentication:
            m_client_state.setText("Authentication");
            break;

         case Client::Connected:
            m_client_state.setText("Connected");
            break;
      }
   }

   void Window::clientError(int code)
   {
      if (code == 0)
      {
         m_error_msg.setText(NoError);
      }
      else
      {
         m_error_msg.setText(Client::errorString(code));

         // add error checking there
         if (code == GTracker::ERROR_WRONG_CID)
            m_info.show();

         m_client->disconnectFromHost();
      }
   }

   void Window::gpsReconnect()
   {
      m_gps->openDevice();
   }

   void Window::gpsStateChanged(GPSDevice::GPSDevice::State state)
   {
      switch (state)
      {
         case GPSDevice::Disconnected:
            m_gps_state.setText("Disconnected");
            QTimer::singleShot(m_gps_interval, this, SLOT(gpsReconnect()));
            break;

         case GPSDevice::Connecting:
            m_gps_state.setText("Connecting");
            break;

         case GPSDevice::Connected:
            m_gps_state.setText("Connected");
            break;

         case GPSDevice::NoLink:
            m_gps_state.setText("No Link");
            break;

         case GPSDevice::LinkReady:
            m_gps_state.setText("Link Available");
            break;
      }
   }

   void Window::gpsFixChanged(GPSDevice::GPSDevice::Fix fix)
   {
      switch (fix)
      {
         case GPSDevice::FixNo:
            m_gps_fix.setText("No Fix");
            break;

         case GPSDevice::Fix2D:
            m_gps_fix.setText("2D");
            break;

         case GPSDevice::Fix3D:
            m_gps_fix.setText("3D");
            break;

      }
   }

   void Window::gpsSatellitesChanged(int inView, int used)
   {
      m_gps_sat.setText(QString("%1/%2").arg(used).arg(inView));
   }

   void Window::gpsSpeedChanged(int speed)
   {
      m_gps_speed.setText(QString("%1 km/h").arg(speed));
   }
} // namespace FindMe
