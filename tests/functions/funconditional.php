<?php

$makefoo = true;

/* We can't call foo() from here 
   since it doesn't exist yet,
   but we can call bar() */

bar();

if ($makefoo) {
  function foo()
  {
    return 1;
  }
}

/* Now we can safely call foo()
   since $makefoo evaluated to true */

if ($makefoo){
  $a = foo();
}

function bar() 
{
  echo "I exist immediately upon program start.\n";
}

echo $a;

?>
