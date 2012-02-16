<?php

$foo = "Bob";              // Assign the value 'Bob' to $foo
$bar = &$foo;              // Reference $foo via $bar.
$bar = 1;  // Alter $bar...

//echo $bar;
//echo $foo;                 // $foo is altered too.

$foo = "Bye";

function hello($bar){
  $bar = 4;
  return $bar;
}

$a = hello($bar);
$bar = 10;
echo $a;
//echo $bar;
//echo $foo;                 // $foo is altered too.

?>

