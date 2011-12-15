<?php
# global.php version but using $GLOBALS

$a = 1;
$b = 2;

function sum(){
    $GLOBALS['b'] = $GLOBALS['a'] + $GLOBALS['b'];
} 

sum();
echo $b;
?>