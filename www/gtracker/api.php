<?php
   function output_json($data, $error = false)
   {
      header('Content-type: application/json');

      if (is_array($data))
      {
         print(json_encode(array('error' => $error, 'size' => count($data), 'data' => $data)));
      }
      else
      {
         print(json_encode(array('error' => $error, 'data' => $data)));
      }
   }

   function output_image($data)
   {
      header('Content-type: image/png');
      print $data;
   }

   function filter($in)
   {
      if (!isset($in))
      {
         throw new Exception('Invalid argument');
      }

      return (stripslashes($in));
   }

   include_once('gtracker.php');

   session_start();

   try
   {
      if (!isset($_GET['action']))
      {
         throw new Exception('Incorrect api usage');
      }

      // create gtracker object
      $g = new GTracker();
      switch ($_GET['action'])
      {
         case 'id':
            output_json($g->id(filter($_GET['device'])));
            break;

         case 'online':
            output_json($g->isOnline(filter($_GET['id'])));
            break;

         case 'tracks':
            output_json($g->tracks(filter($_GET['id'])));
            break;

         case 'get_track':
            output_json($g->getTrack(filter($_GET['id']), filter($_GET['track_id'])));
            break;

         case 'remove_track':
            output_json($g->removeTrack(filter($_GET['id']), filter($_GET['track_id'])));
            break;

         case 'rename_track':
            output_json($g->renameTrack(filter($_GET['id']), filter($_GET['track_id']), filter($_GET['name'])));
            break;

         case 'get_loc':
            if (isset($_GET['last_id']))
            {
               output_json($g->getLocation(filter($_GET['id']), filter($_GET['last_id'])));
            }
            else
            {
               output_json($g->getLocation(filter($_GET['id'])));
            }
            break;

         case 'get_image':
            output_image($g->getImage(stripslashes($_GET['id'])));
            break;

         case 'get_opts':
            output_json($g->getOpts(filter($_GET['id'])));
            break;

         case 'get_config':
            output_json($g->getConfig(filter($_GET['id'])));
            break;

         case 'set_config':
            output_json($g->setConfig(filter($_GET['id']), filter($_GET['alias']), filter($_GET['timezone']), filter($_GET['color']), filter($_GET['weight'])));
            break;

         case 'make_track_link':
            output_json($g->makeTrackLink(filter($_GET['id']), filter($_GET['track_id'])));
            break;

         case 'make_real_time_link':
            output_json($g->makeRealTimeLink(filter($_GET['id'])));
            break;

         case 'get_public_track':
            output_json($g->getPublicTrack(filter($_GET['link'])));
            break;

         default:
            throw new Exception('Incorrect api usage, invalid action');
      }
   }
   catch (Exception $e)
   {
      output_json($e->getMessage(), true);
   }
?>
