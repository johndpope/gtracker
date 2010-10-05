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

package com.gtracker.optional.locationservices;

import javax.microedition.location.*;

class OptionalLocationProviderImpl extends OptionalLocationProvider implements LocationListener
{

   public OptionalLocationProviderImpl() throws Exception
   {
      Criteria cr = new Criteria();
      cr.setSpeedAndCourseRequired(true);
      lp_ = LocationProvider.getInstance(cr);
      if (lp_ == null)
      {
         throw new RuntimeException("Unable to get location provider.");
      }
   }

   public void locationUpdated(LocationProvider lp, Location l)
   {
      ll_.onSpeed(l.getSpeed());
      QualifiedCoordinates qc = l.getQualifiedCoordinates();
      if (qc != null)
      {
         if (!connected_)
         {
            connected_ = true;
            ll_.onStateChanged(com.gtracker.IStateCallback.StateEnum.GpsConnected);
         }
         ll_.onCoordinate(qc.getLatitude(), qc.getLongitude(), l.getTimestamp());
      }
   }

   public void providerStateChanged(LocationProvider lp, int newState)
   {
      if (newState == LocationProvider.AVAILABLE)
      {
         ll_.onStateChanged(com.gtracker.IStateCallback.StateEnum.GpsConnected);
      }
      else if (newState == LocationProvider.TEMPORARILY_UNAVAILABLE)
      {
         ll_.onStateChanged(com.gtracker.IStateCallback.StateEnum.GpsConnecting);
      }
      else if (newState == LocationProvider.OUT_OF_SERVICE)
      {
         ll_.onStateChanged(com.gtracker.IStateCallback.StateEnum.GpsConnecting);
      }
   }

   public void setListener(ILocationListener ll)
   {
      ll_ = ll;
      if (ll_ != null)
      {
         lp_.setLocationListener(this, 1, -1, -1);
      }
      else
      {
         lp_.setLocationListener(null, -1, -1, -1);
      }
   }
   private ILocationListener     ll_ = null;
   private boolean               connected_ = false;
   private LocationProvider      lp_ = null;
}
