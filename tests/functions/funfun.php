<?php
function foo() 
{
  function bar() 
  {
    return "I don't exist until foo() is called.";
  }
}

/* We can't call bar() yet
   since it doesn't exist. */

foo();

/* Now we can call bar(),
   foo()'s processing has
   made it accessible. */

bar();

?>