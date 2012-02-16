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

//$l[$a] = f($a);

//foreach ($l as $key => $value){
//    print "#$key = $value\n";
//}

?>
