<?php
# PHP does not support global variables
# printSurname will not print $surname

$name = "John";

function printName (){
    $surname = "Doe\n";
    function printSurname (){
        print $surname;
    }
    
    printSurname();
}

printName();

?>