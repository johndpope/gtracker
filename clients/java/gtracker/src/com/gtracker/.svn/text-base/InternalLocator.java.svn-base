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

import com.gtracker.optional.locationservices.*;

public class InternalLocator implements ILocator, ILocationListener
{
   public InternalLocator(GtClient client) throws ClassNotFoundException, RuntimeException
   {
      client_ = client;
      lp_ = OptionalLocationProvider.getProvider();
      if (lp_ == null)
      {
         throw new RuntimeException("Unable to get location provider.");
      }
   }

   public void start() throws Exception
   {
      lp_.setListener(this);
      client_.onMessage(new ServerStateMessage(IStateCallback.StateEnum.GpsConnecting));
   }
   
   public void stop() throws Exception
   {
      lp_.setListener(null);
      lp_ = null;
   }

   public void onSpeed(float value)
   {
      client_.onMessage(new SpeedMessage(value));
   }
   public void onCoordinate(double latitude, double longitude, long timestamp)
   {
      client_.onMessage(new CoordinateMessage(latitude, longitude, timestamp));
   }

   public void onStateChanged(int newState)
   {
      client_.onMessage(new ServerStateMessage(newState));
   }

   private IClient                  client_ = null;
   private OptionalLocationProvider lp_ = null;
}
