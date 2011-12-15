<?php
$foo = "Bob";              // Assign the value 'Bob' to $foo
$bar = &$foo;              // Reference $foo via $bar.
$bar = "My name is $bar\n";  // Alter $bar...
echo $bar;
echo $foo;                 // $foo is altered too.
?>