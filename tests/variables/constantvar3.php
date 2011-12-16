<?php

define('FOO_BAR','It works!');
define('FOO_FOO_BAR','It works again!');

// prints 'It works!'
$changing_variable = 'bar';
echo constant('FOO_' . strtoupper($changing_variable));

// prints 'It works again!'
$changing_variable = 'foo_bar';
echo constant('FOO_' . strtoupper($changing_variable));

?>