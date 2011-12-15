<?php
# PHP does not support global variables
# but printName will print $name if we use GLOBALS

$name = "John";

function printName (){
    print $GLOBALS["name"];
}

printName();

?>