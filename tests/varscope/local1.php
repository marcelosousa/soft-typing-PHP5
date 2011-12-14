<?php
# PHP does not support global variables
# printName will not print $name initially

$name = "John";

function printName (){
    print $name;
    $name = "Doe\n";
    print $name;
}

print $name;
print " ";
printName();

?>