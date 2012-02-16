<?php
/*

frank at interactinet dot com 02-May-2011 05:15
Be careful when assigning a value in the if statement, for example:

 if($var = $arg)

$var might be assigned "1" instead of the expected value in $arg.
*/

function myMethod(){
    return "test";
}

if($val = myMethod()){
    // $val might be 1 instead of the expected 'test'
    print $val;
}

print $val;

?>