<?php
function takes_array($input){
    $res = $input[0]+$input[1];
    return $res;
    #echo "$input[0] + $input[1] = ", $input[0]+$input[1];
}
$a = array(0 => 1, 2);

$b = takes_array($a);

?>