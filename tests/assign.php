<?php

$a = 0;

function f(&$b)
{
 $b = 3;
 $a = 10;
 return 1;
}

$l[$a] = f($a);

foreach ($l as $key => $value){
    print "#$key = $value\n";
}

?>