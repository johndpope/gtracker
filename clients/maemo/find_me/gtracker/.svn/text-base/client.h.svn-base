#ifndef __GTRACKER_CLIENT_H
#define __GTRACKER_CLIENT_H

#include "messages.h"
#include <QObject>
#include <QTcpSocket>
#include <QDateTime>
#include <QTemporaryFile>
#include <QTimer>

namespace GTracker
{
   class Client : public QObject
   {
      Q_OBJECT
      Q_ENUMS(State)

      public:
         enum State { Disconnected, Connecting, Authentication, Connected };

      public:
         Client(QString userId = QString(), QObject * parent = 0);
         virtual ~Client();

         QString  userId() const;
         State    state() const;

         void connectToHost(QString hostName, quint16 port = 7777);
         void disconnectFromHost();

      signals:
         void userIdChanged(QString userId);
         void stateChanged(GTracker::Client::State state);
         void error(int code);

      public slots:
         void updatePosition(double lat, double lon, QDateTime timestamp);
         void resetId();
         
      private slots:
         void socketStateChanged(QAbstractSocket::SocketState state);
         void authenticate();
         void socketReadyRead();
         void socketError(QAbstractSocket::SocketError error);

      private:
         void setState(State state);
         void sendMessage(Length length, const void * data);
         void authenticateResponseMessage(const AuthenticateResponseMessage & msg);
         void errorMessage(const ErrorMessage & msg);

      public:
         static const char * errorString(int code);

      private:
         QString           m_user_id;
         State             m_state;
         QTcpSocket        m_socket;
         QByteArray        m_buffer;

         QTemporaryFile    m_tmp;
   };
}

#endif // __GTRACKER_CLIENT_H
