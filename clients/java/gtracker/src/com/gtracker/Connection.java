/*
 * Copyright (C) 04/20/2010 Dmitry S. Melnikov (dmitryme@gmail.com)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package com.gtracker;


import javax.microedition.io.*;
import java.io.*;

public class Connection implements Runnable
{
   public Connection(IClient clnt)
   {
      client_ = clnt;
      state_ = new State(client_, IStateCallback.StateEnum.ServerDisconnected);
   }

   public void open(
           String addr,
           int port,
           String bkpAddr,
           int bkpPort,
           String deviceID) throws Exception
   {
      if (state_.getState() == IStateCallback.StateEnum.ServerConnected ||
          state_.getState() == IStateCallback.StateEnum.ServerAuthCheck ||
          state_.getState() == IStateCallback.StateEnum.ServerAuthOk)
      {
         return;
      }
      serverName_ = addr;
      bkpServerName_ = bkpAddr;
      address_ = "socket://" + addr + ":" + String.valueOf(port);
      bkpAddress_ = "socket://" + bkpAddr + ":" + String.valueOf(bkpPort);
      deviceID_ = deviceID;
      thread_ = new Thread(this);
      close_ = false;
      thread_.start();
   }

   public void close() throws Exception
   {
      try
      {
        close_ = true;
        if (is_ != null)
        {
           is_.close();
        }
        if (outDataPr_ != null)
        {
           outDataPr_.close();
        }
        outDataPr_ = null;
        if (conn_ != null)
        {
           conn_.close();
        }
        thread_.join();
      }
      catch(Exception ex) {}
   }
   
   public void send(IOutMessage msg)
   {
      if (outDataPr_ != null)
      {
         outDataPr_.queue(msg);
      }
   }

   public void run()
   {
      String err = new String("");
      int i = 0;
      while (!close_)
      {
         try
         {
            connect();
            byte b = is_.readByte();
            if (b == -1)
            {
               throw new EOFException();
            }
            if (buf_ == null)
            {
               buf_ = new byte[b];
               i = 0;
            } else
            {
               buf_[i++] = b;
            }
            if (buf_ != null && buf_.length == i)
            {
               switch (buf_[0])
               {
                  case 0x42:
                  {

                     deviceID_ = new String(buf_, 1, buf_.length - 1);
                     state_.setState(IStateCallback.StateEnum.ServerAuthOk, deviceID_);
                     if (outDataPr_ != null)
                     {
                        outDataPr_.ready2send(true);
                     }
                     break;
                  }
                  case 0x44:
                  {
                     client_.onMessage(new ErrorMessage(buf_));
                     break;
                  }
               }
               client_.onMessage(new BytesInOut(buf_.length + 1 + TCP_HEADER_SIZE, 0));
               buf_ = null;
            }
         }
         catch(IOException ex)
         {
            if(close_)
            {
               break;
            }
            err = ex.toString();
         }
         catch (Exception ex)
         {
            if (outDataPr_ != null)
            {
               outDataPr_.ready2send(false);
            }
            err = ex.toString();
            break;
         }
      }
      if (outDataPr_ != null)
      {
         outDataPr_.ready2send(false);
      }
      state_.setState(IStateCallback.StateEnum.ServerDisconnected, err);
   }

   private void connect() throws Exception
   {
      if (close_ ||
              state_.getState() == IStateCallback.StateEnum.ServerAuthCheck ||
              state_.getState() == IStateCallback.StateEnum.ServerAuthOk)
      {
         return;
      }
      state_.setState(IStateCallback.StateEnum.ServerConnecting);
      if (firstAddressUsed_)
      {
         firstAddressUsed_ = false;
         conn_ = (SocketConnection)Connector.open(address_, Connector.READ_WRITE, true);
      }
      else
      {
         firstAddressUsed_ = true;
         conn_ = (SocketConnection)Connector.open(bkpAddress_, Connector.READ_WRITE, true);
      }
      conn_.setSocketOption(SocketConnection.LINGER, 5);
      is_ = new DataInputStream(conn_.openInputStream());
      if (outDataPr_ != null)
      {
         outDataPr_.close();
         outDataPr_ = null;
      }
      state_.setState(
              IStateCallback.StateEnum.ServerConnected,
              firstAddressUsed_ == false ? serverName_ : bkpServerName_);
      state_.setState(IStateCallback.StateEnum.ServerAuthCheck);
      outDataPr_ = new OutDataProcessor(client_, deviceID_, conn_.openOutputStream());
   }

   private String             address_;
   private String             bkpAddress_;
   private String             serverName_;
   private String             bkpServerName_;
   private boolean            firstAddressUsed_ = true;
   private String             deviceID_ = "";
   private Thread             thread_ = null;
   private OutDataProcessor   outDataPr_ = null;
   private DataInputStream    is_ = null;
   private SocketConnection   conn_ = null;
   private IClient            client_ = null;
   private byte[]             buf_ = null;
   public static int          TCP_HEADER_SIZE = 50;
   private State              state_ = null;
   private boolean            close_ = false;
}
