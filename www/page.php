<?php

require_once('/usr/share/php/smarty/Smarty.class.php');
require_once('utils/db.php');
require_once('utils/smarty_tr.php');

class BasePage extends Smarty
{
        const www_root = '/var/www';

        protected $css_include          = array( 'base.css' );
        protected $js_include           = array( );
        protected $js_ext_include       = array( 'http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js' );
        protected $default_lang         = 'ru';
        protected $db;
        private   $index;
        private   $time; 

        public function __construct($index = 'index.tbl')
        {
                parent::__construct();
                $this->index = $index;
                $this->time = time();
                $this->db = new Connection(true, false);

                $this->template_dir     = $this->prefix('templates');
                $this->compile_dir      = $this->prefix('temp');
                $this->cache_dir        = $this->prefix('cache');
                $this->register_prefilter('tr_template');

                session_start();

                $_SESSION['lang'] = $_GET['lang'] or $_COOKIE['lang'] or $_SESSION['lang'] or 'ru';
        }

        public function render()
        {
                // set cookie
                $this->setCookie('lang', $_SESSION['lang']);

                // require_once(self::www_root . '/lang/' . $_SESSION['lang'] . '.php');
                $this->assign('css_include', $this->css_include);
                $this->assign('js_include', $this->js_include);
                $this->assign('js_ext_include', $this->js_ext_include);
                $this->assign('lang', $_SESSION['lang']);
                tr_load_from_db('ru');

                $this->display($this->index);
        }

        public function prefix($path)
        {
                return self::www_root . '/' . $path;
        }

        protected function setCookie($name, $value)
        {
                setcookie($name, $value, $this->time + 3600);
        }
}

class Page
{
        const www_path = '/var/www';
        const yandex_key = 'AHYJuksBAAAA4c2-QwIAut5koQTv7-D-EKWPOXoCM5EuNNsAAAAAAAAAAABplviREBrg-V7QLiNyxjxBlFs3dg==';
        const default_map = 'google';

        /*
         * Variables
         */
        private $map_types;
        private $smarty;
        private $page_list = array('map', 'download', 'about');
        private $map_js = array(
                'jquery.cookie',
                'jquery.blurnotify',
                'jquery.tr',
                'jquery-impromptu.3.1.min',
                'gtracker',
                'page'
        );

        /*
         * Standard constructor
         */
        public function __construct()
        {
                $this->map_types = array('yandex', 'google', 'osm');
                $this->smarty = new Smarty();
                $this->smarty->template_dir = self::www_path . '/templates/';
                $this->smarty->compile_dir = self::www_path . '/temp/';
                $this->smarty->cache_dir = self::www_path . '/cache/';

                $this->setLanguage();
                $this->setMap();

                require_once(self::www_path . '/lang/' . $_SESSION['lang'] . '.php');
                $this->smarty->assign('l', $lang);
                $this->smarty->assign('lang', $_SESSION['lang']);
                $this->smarty->assign('nav_items', $this->page_list);
                $this->smarty->assign('meta', $meta);
        }

        /*
         * Set language
         */
        private function setLanguage()
        {
                if (isset($_GET['lang']) && file_exists(self::www_path . '/lang/' . $_GET['lang'] . '.php'))
                {
                        $_SESSION['lang'] = $_GET['lang'];
                }
                elseif (!isset($_COOKIE['lang']) || !file_exists(self::www_path . '/lang/' . $_COOKIE['lang'] . '.php'))
                {
                        $_SESSION['lang'] = 'ru';
                }
                else
                {
                        $_SESSION['lang'] = $_COOKIE['lang'];
                }
        }

        /*
         * Set map
         */
        private function setMap()
        {
                if (isset($_GET['map']) && in_array($_GET['map'], $this->map_types))
                {
                        $_SESSION['map'] = $_GET['map'];
                }
                elseif (!isset($_COOKIE['map']) || !in_array($_COOKIE['map'], $this->map_types))
                {
                        $_SESSION['map'] = self::default_map;
                }
                else
                {
                        $_SESSION['map'] = $_COOKIE['map'];
                }
        }

        /*
         * Update cookie
         */
        public function setCookie()
        {
                $expiration = time() + 3600;
                setcookie('lang', $_SESSION['lang'], $expiration);
                setcookie('map', $_SESSION['map'], $expiration);
        }

        /*
         * Show main page
         */
        public function showMap()
        {
                $this->smarty->assign('page', 'map');
                $this->smarty->assign('map', $_SESSION['map']);
                $this->smarty->assign('js', $this->map_js);
                if ($_SESSION['map'] == 'yandex')
                {
                        $this->smarty->assign('yandex_key', self::yandex_key);
                }
                $this->smarty->display('index2.tbl');
        }

        /*
         * Show error page
         */
        public function showError()
        {
                $this->smarty->assign('page', 'error');
                $this->smarty->display('index2.tbl');
        }

        /*
         * Show about page
         */
        public function showAbout()
        {
                $this->smarty->assign('page', 'about');
                $this->smarty->display('index2.tbl');
        }

        /*
         * Show download page
         */
        public function showDownload()
        {
                $this->smarty->assign('page', 'download');
                $this->smarty->display('index2.tbl');
        }

        /*
         * Show fullscreen map
         */
        public function showFullscreenMap()
        {
                $this->smarty->assign('map', $_SESSION['map']);
                $this->smarty->assign('path', stripslashes($_GET['path']));
                if ($_SESSION['map'] == 'yandex')
                {
                        $this->smarty->assign('yandex_key', self::yandex_key);
                }
                $this->smarty->display('index_map_fs.tbl');
        }
}

?>
