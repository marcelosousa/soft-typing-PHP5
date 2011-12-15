<?php

function counterns(){
    $a = 0;
    echo $a;
    $a = $a+1;    
}

function counter(){
    static $a = 0;
    echo $a;
    $a = $a+1;
}

print "Counter with non-static variables\n";
counterns();
counterns();

print "\nCounter with static variables\n";
counter();
counter();

print "\n";

?>