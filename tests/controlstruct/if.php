<?php

$a = $b = 1;

if ($a >= $b)
    echo "a is equal or bigger than b\n";

if ($a >= $b) {
    echo "a is equal or bigger than b\n";
    $b = $a;
}
?>