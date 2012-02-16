<?php

function foo ($x)
{
    $c = $x;
    return $c + $c;
}

$a = 1;
$b = foo ($a);

//check ($a, "[TyInt]");
//check ($b, "[TyInt]");

?>