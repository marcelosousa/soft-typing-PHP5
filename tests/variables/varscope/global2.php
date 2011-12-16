<?php

function writefunctionvars() {
    global $foo;
    $foo = "something";
}

writefunctionvars();
echo $foo; // displays "something"

?>