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

public class ErrorMessage implements IMessage
{
   class ErrorCode
   {

      public static final int NotAuth = 0x01;
      public static final int WrongMsg = 0x02;
      public static final int FuckOff = 0x03;
      public static final int ServerUnavailable = 0x04;
      public static final int WrongDID = 0x05;
      public static final int AlreadyAuth = 0x06;
   }

   public ErrorMessage(byte[] buf)
   {
      err_ = (int) buf[1];
      if (buf.length > 2)
      {
         descr_ = new String(buf, 2, buf.length - 2);
      }
   }

   public int getErrCode()
   {
      return err_;
   }

   public String getDescr()
   {
      return descr_;
   }

   public void process(GtClient c) throws Exception
   {
      c.process(this);
   }
   int err_;
   String descr_;
}
