<?php

include_once('page.php');

session_start();

$page = new Page();
$page->setCookie();
switch($_GET['page'])
{
   case 'error':
      $page->showError();
      break;
   case 'about':
      $page->showAbout();
      break;
   case 'download':
      $page->showDownload();
      break;
   case 'view':
      $page->showFullscreenMap();
      break;
   case 'map':
   default:
      $page->showMap();
      break;
}

?>
