<?php
# Use of global keyword

$a = 1;
$b = 2;

function sum(){
    global $a, $b;

    $b = $a + $b;
} 

sum();

echo $b;
?>