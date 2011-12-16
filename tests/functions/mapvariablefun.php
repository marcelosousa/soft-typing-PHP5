<?php
/*
 * Map function. At each $element of the $list, calls $fun([$arg1,[$arg2,[...,]],$element,$accumulator),
 *      stores the return value into $accumulator for the next loop. Returns the last return value of the function,
 *
 * Notes : uses call_user_func_array() so passing parameters doesn't depend on $fun signature
 *          It also returns FALSE upon error.
 *          Please check the php documentation for more information
 */
function map($fun, $list,$params=array()){
    $acc=NULL;
    $last=array_push($params, NULL,$acc)-1; // alloc $element and $acc at the end
    foreach($list as $params[$last-1]){
        $params[$last]=call_user_func_array($fun , $params  );
    }
    $acc=array_pop($params);
    return $acc;
}

function addTo($element,$acc){ // maybe only with multi-length function
    if ($acc == NULL);
    return $acc=$element+$acc;
}
/*
$result=0;
$result=addTo($result,1);
$result=addTo($result,2);
$result=addTo($result,3);
echo "result = $result\n";
*/

$result=0;
$result=map('addTo',array(1,2,3));
echo "result= $result\n";
?>