<?php

require_once('db.php');

$GLOBALS['tr'] = array();

function tr_template($tpl_source, &$smarty)
{
        return preg_replace_callback('/\[\[(.*?)\]\]/',
                'tr_template_item', $tpl_source);
}

function tr_template_item($matches)
{
        return isset($GLOBALS['tr'][$matches[1]]) ? $GLOBALS['tr'][$matches[1]] : $matches[1];
}

function tr_load_from_file($file)
{
        $lines = file($file);
        foreach($lines as $line)
        {
                list($key, $value) = explode('=', $line);
                $GLOBALS['tr'][trim($key)] = trim($value);
        }
}

function tr_load_from_db($lang)
{
        try
        {
                $db = new Connection();
                $result = $db->query('SELECT name, value FROM lang WHERE lang="' . $lang . '"');

                while ($row = mysql_fetch_array($result))
                {
                        $GLOBALS['tr'][$row[0]] = $row[1];
                }

                mysql_free_result($result);
        }
        catch (Exception $e)
        {
        }
}

?>
