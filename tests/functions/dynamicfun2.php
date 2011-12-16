<?php
$tmp = "foo";
$$tmp = function() {
    global $tmp;
    echo $tmp;
}; 

$$tmp();
?>