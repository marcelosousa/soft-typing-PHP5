<?php

function f(&$a)
{
 $a = 3;
 return 1;
}

$a = 0;

$l[$a] = f($a);


foreach ($l as $key => $value){
    print "#$key = $value\n";
}

?>