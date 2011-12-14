<?php
# PHP does not support global variables
# printName will not print $name

$name = "John";

function printName (){
    print $name;
}

printName();

?>