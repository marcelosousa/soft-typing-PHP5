<?php
# Recursive function example with static variables

function countTo10(){
    static $count = 0;

    $count++;
    echo $count;
    if ($count < 10) {
        countTo10();
    }
    $count--;
}

countTo10();
?>