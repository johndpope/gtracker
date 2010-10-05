<?php

define('HOSTNAME', 'localhost');
define('USERNAME', 'gtuser');
define('PASSWORD', 'Meech20h');
define('DATABASE', 'gtracker');

class Connection
{
        /*
         * Private vars
         */
        private $link;

        /*
         * Standard constructor
         */
        public function __construct($persistent = true)
        {
                if ($persistent)
                {
                        $this->link = @mysql_pconnect(HOSTNAME, USERNAME, PASSWORD);
                }
                else
                {
                        $this->link = @mysql_connect(HOSTNAME, USERNAME, PASSWORD);
                }

                if (!$this->link)
                {
                        $this->fatal('Can\'t connect to MySQL server');
                }

                if (!@mysql_select_db(DATABASE, $this->link))
                {
                        $this->fatal('Can\'t select database');
                }
        }

        /*
         * Standard destructor
         */
        public function __destruct()
        {
                @mysql_close($this->link);
        }

        /*
         * Error handling func
         */
        protected function fatal($msg)
        {
                throw new Exception($msg);
        }

        /*
         * Exec query
         */
        public function query($str)
        {
                $result = @mysql_query($str, $this->link);
                if (!$result)
                {
                        //$this->fatal(mysql_error());
                        $this->fatal('Query error');
                }

                return $result;
        }
}
?>
