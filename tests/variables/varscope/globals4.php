<?php
# PHP does not support global variables although we can use GLOBALS
# What happens if we have two variables with same name and use GLOBALS?

$name = "John\n";

print $GLOBALS["name"];

function printName (){
    $name = "Joe\n";
    print $GLOBALS["name"];
    print $name;
}

printName();

?>