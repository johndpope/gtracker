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

import java.io.*;
import java.util.Vector;

public class OutDataProcessor implements Runnable
{
   public OutDataProcessor(IClient client, String deviceId, OutputStream os) throws Exception
   {
      client_ = client;
      os_ = os;
      IOutMessage auth_msg = new AuthMessage(deviceId);
      send_buf(auth_msg.toBytes());
      thread_ = new Thread(this);
      thread_.start();
   }

   public void close() throws Exception
   {
      close_ = true;
      synchronized(queue_msgs_)
      {
         notify();
      }
      thread_.join();
   }

   public void ready2send(boolean ready)
   {
      ready2send_ = ready;
   }

   public void queue(IOutMessage msg)
   {
      synchronized (queue_msgs_)
      {
         queue_msgs_.addElement(msg);
         if (queue_msgs_.size() > 60)
         {
            queue_msgs_.removeElement(queue_msgs_.firstElement());
         }
         queue_msgs_.notify();
      }
   }

   public void run()
   {
      while(!close_)
      {
         try
         {
            IOutMessage msg = null;
            synchronized (queue_msgs_)
            {
               if (queue_msgs_.size() > 0)
               {
                  msg = (IOutMessage) queue_msgs_.firstElement();
                  queue_msgs_.removeElement(queue_msgs_.firstElement());
               }
               else
               {
                  queue_msgs_.wait();
               }
            }
            if (msg != null)
            {
               send_buf(msg.toBytes());
            }
         }
         catch(Exception ex)
         {
         }
      }
   }
   
   private void send_buf(byte[] msg) throws Exception
   {
      if (msg.length > 256)
      {
         throw new RuntimeException("Length of buffer " + String.valueOf(msg.length) + " is greater than 256");
      }
      byte tmp[] = new byte[1 + msg.length];
      tmp[0] = (byte) msg.length;
      System.arraycopy(msg, 0, tmp, 1, msg.length);
      os_.write(tmp);
      os_.flush();
      client_.onMessage(new BytesInOut(0, tmp.length + Connection.TCP_HEADER_SIZE));
   }
   private Thread       thread_;
   private IClient      client_ = null;
   private OutputStream os_ = null;
   private boolean      close_ = false;
   private final Vector queue_msgs_ = new Vector();
   private boolean      ready2send_ = false;
}
