<?php  
function letMeSeeAClosure() {  
    $aLocalNum = 10;  
    return function() use (&$aLocalNum) { return ++$aLocalNum; };  
}  
$aClosure = letMeSeeAClosure();  
echo "$aClosure()\n";  
// Output: 11  
echo $aClosure();  
// Output: 12  
$anotherClosure = letMeSeeAClosure();  
echo $anotherClosure();  
// Output: 11  
echo $aClosure();  
// Output: 13  
echo $aLocalNum;  
// Notice: Undefined Variable: aLocalNum<br>
?>