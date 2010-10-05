<?php

require_once('../utils/db.php');

define('DEFAULT_IMAGE', 'no_img.png');
define('AVATAR_PATH', '../avatars');
define('ME', 'http://gtracker.ru');

class GTracker extends Connection
{
   /*
    * Standard constructor
    */
   public function __construct()
   {
      parent::__construct(true);
   }

   /*
    * Standard destructor
    */
   public function __destruct()
   {
      parent::__destruct();
   }

   /*
    * Return device uniq id
    */
   public function id($device)
   {
      $result = $this->query('SELECT id,alias,name AS device FROM device WHERE name="' . $device .'"');
      if (@mysql_num_rows($result) > 0)
      {
         $row = mysql_fetch_assoc($result);
         return (object) $row;
      }

      $this->fatal('Required device not found');
   }

   /*
    * Check status of id
    */
   public function isOnline($id)
   {
      $result = $this->query('SELECT online, IFNULL((select speed from coordinate as c where c.device_id = "' . $id . '"), 0) as speed FROM device WHERE id="' . $id . '"');
      $row = @mysql_fetch_assoc($result) or $this->fatal('Failed to check online');
      mysql_free_result($result);

      return (object) $row;
   }

   /*
    * Return all tracks by id
    */
   public function tracks($id)
   {
      $result = $this->query('select t.id, t.name, convert_tz(min(c.time), "UTC", d.timezone) as start, convert_tz(max(c.time), "UTC", d.timezone) as stop from track as t inner join device as d on t.device_id = d.id inner join coordinate as c on t.id = c.track_id where t.status="closed" and t.device_id = "' . $id . '" group by t.id');
      $ret = array();
      while ($row = mysql_fetch_assoc($result))
      {
         $ret[] = (object) $row;
      }
      mysql_free_result($result);

      return $ret;
   }

   /*
    * Return array of points
    */
   public function getTrack($id, $track_id)
   {
      $result = $this->query('SELECT latitude, longitude FROM coordinate WHERE track_id = "' . $track_id . '" AND device_id = "' . $id . '"');
      $ret = array();
      while ($row = mysql_fetch_row($result))
      {
         $ret[] = $row[0];
         $ret[] = $row[1];
      }
      mysql_free_result($result);

      return $ret;
   }

   /*
    * Return array of points
    */
   public function getPublicTrack($link)
   {
      $result = $this->query('SELECT c.latitude, c.longitude FROM coordinate AS c, reference AS r WHERE r.value="' . $link . '" AND c.track_id=r.track_id AND c.device_id=r.device_id');
      if (mysql_num_rows($result) == 0)
      {
         $this->fatal('Invalid link');
      }

      $ret = array();
      while ($row = mysql_fetch_row($result))
      {
         $ret[] = $row[0];
         $ret[] = $row[1];
      }
      mysql_free_result($result);

      return $ret;
   }

   /*
    * Remove track
    */
   public function removeTrack($id, $track_id)
   {
      $result = $this->query('DELETE FROM track WHERE id = "' . $track_id . '" AND device_id = "' . $id . '"');
      return $result ? true : false;
   }

   /*
    * Rename track
    */
   public function renameTrack($id, $track_id, $name)
   {
      $result = $this->query('UPDATE track SET name = "' . $name . '" WHERE id = "' . $track_id . '" AND device_id = "' . $id . '"');
      return $result ? true : false;
   }

   /*
    * Return current position
    */
   public function getLocation($id, $last_id = null)
   {
      if (isset($last_id))
      {
         $result = $this->query('SELECT id, latitude AS lt, longitude AS ln FROM coordinate WHERE device_id="' . $id . '" AND id > "' . $last_id . '"');
      }
      else
      {
         $result = $this->query('SELECT id, latitude, longitude FROM coordinate WHERE track_id = (SELECT max(id) from track WHERE status = "opened" AND device_id = "' . $id . '")');
      }

      $ret = array();
      $count = mysql_num_rows($result);
      while ($row = mysql_fetch_row($result))
      {
         $ret[] = $row[1];
         $ret[] = $row[2];

         if (--$count == 0)
         {
            $ret[] = $row[0];
         }
      }
      mysql_free_result($result);

      return $ret;
   }

   /*
    * Return image 48x48
    */
   public function getImage($id)
   {
      if (isset($id))
      {
         $result = $this->query('SELECT pixmap FROM device WHERE id="' . $id . '" AND pixmap IS NOT NULL AND pixmap != ""');
         if (mysql_num_rows($result) > 0)
         {
            $row = mysql_fetch_row($result);
            $data = $row[0];
         }
      }

      if (!isset($data))
      {
         $file = AVATAR_PATH . '/' . DEFAULT_IMAGE;
      }
      else
      {
         $file = AVATAR_PATH . '/' . $data;
      }

      $fp = fopen($file, 'rb');
      $data = fread($fp, filesize($file));
      fclose($fp);

      mysql_free_result($result);

      return $data;
   }

   /*
    * Return options for id
    */
   public function getOpts($id)
   {
      $result = $this->query('SELECT weight, color FROM device WHERE id="' . $id . '"');
      if (mysql_num_rows($result) > 0)
      {
         $row = mysql_fetch_assoc($result);
         return (object) $row;
      }

      $this->fatal('Bad id');
   }

   /*
    * Set image for account. Image should be 48x48
    */
   public function setImage($id, $name)
   {
      $this->query('UPDATE device SET pixmap="' . $name . '" WHERE id="' . $id . '"');
   }

   /*
    * Make link for track
    */
   public function makeTrackLink($id, $track_id)
   {
      $result = $this->query('SELECT value FROM reference WHERE device_id="' . $id . '" AND track_id="' . $track_id . '"');
      if (mysql_num_rows($result) > 0)
      {
         $row = mysql_fetch_row($result);
         return ME . '/view/' . $row[0];
      }

      $hash = md5($track_id . session_id() . $id . date(DATE_RFC822));
      $result = $this->query('INSERT INTO reference (device_id, track_id, value) VALUES ("' . $id . '", "' . $track_id . '", "' . $hash . '")');

      return ME . '/view/' . $hash;
   }

   /*
    * Make link for track
    */
   public function makeRealTimeLink($id)
   {
      // insert into database
      return ME . '/view/' . md5($id . 'dima' . date(DATE_RFC822));
   }

   public function getConfig($id)
   {
      $result = $this->query('SELECT alias, timezone, color, weight FROM device WHERE id="' . $id . '"');
      if (mysql_num_rows($result) > 0)
      {
         $row = mysql_fetch_assoc($result);
         return (object) $row;
      }

      $this->fatal("Bad id");
   }

   public function setConfig($id, $alias, $timezone, $color, $weight)
   {
      $result = $this->query('UPDATE device SET alias = "' . $alias . '", timezone = "' . $timezone . '", color = "' . $color . '", weight = "' . $weight . '" WHERE id="' . $id . '"');
      return $result ? true : false;
   }
}

?>
