<?php

function odd ($x)
{
    if ($x == 0) {
        return false;
    } else {
        return even ($x - 1);
    }
}

function even ($x)
{
    if ($x == 0) {
        return true;
    } else {
        return odd ($x - 1);
    }
}

$x = even (10);

check ($x, "[TyBool]");

?>