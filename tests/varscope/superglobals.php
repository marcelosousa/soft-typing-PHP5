<?php
# PHP has some predefined global variables called superglobals

#  $_GET[];     An array that includes all the GET variables that PHP received from the client browser.
#  $_POST[];    An array that includes all the POST variables that PHP received from the client browser.
#  $_COOKIE[];  An array that includes all the cookies that PHP received from the client browser.
#  $_ENV[];     An array with the environment variables.
#  $_SERVER[];  An array with the values of the web-server variables.

$_ENV['mystring'] = 'Hello World';
$_ENV['myarray'] = array('Alpha', 'Bravo', 'Charlie');

function test() {
    print $_ENV['mystring'];
    print_r($_ENV['myarray']);
}

test();

?>