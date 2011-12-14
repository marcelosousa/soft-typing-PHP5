<?php
# Simple array

$arr = array("foo" => "bar", 12 => true);

echo $arr["foo"]; // bar
echo $arr[12];    // 1

# Array whose value is another array
$arrz = array("somearray" => array(6 => 5, 13 => 9, "a" => 42));

echo $arrz["somearray"][6];    // 5
echo $arrz["somearray"][13];   // 9
echo $arrz["somearray"]["a"];  // 42

?>