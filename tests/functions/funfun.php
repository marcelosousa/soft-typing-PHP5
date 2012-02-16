<?php
function foo() 
{
  function bar() 
  {
    return 1;
  }
  return "res";
}

/* We can't call bar() yet
   since it doesn't exist. */

$c = foo();

/* Now we can call bar(),
   foo()'s processing has
   made it accessible. */

$a = bar();
$a = false;
print $a;

?>