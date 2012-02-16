<?php

$a = 0;

function f(&$b)
{
 if(true){
   $b = "Hello";
 }
 $a = 10;
 return $a;
}

$b = f($a);
//$a = "bah";
print $a;  


?>
