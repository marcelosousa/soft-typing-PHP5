<?php
# PHP does not support global variables although we can use GLOBALS
# What happens if we have two variables with same name and use GLOBALS?
# "Global" scope wins.

print "Global in\n";

$name = "John\n";

print $GLOBALS["name"];

function printName (){
    print "printName in\n";
    $name = "Joe\n";
    
    function printNames (){
        print "printNames in\n";
        print $GLOBALS["name"];
        print "printNames out\n";
    }
    
    print $GLOBALS["name"];
    print $name;
    
    printNames();
    print "printName out\n";
}

printName();
printNames();

print "Global out\n"

?>