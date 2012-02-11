<?php

$a = 1;
$b = 2;

$c = $a == $b;	// Equal	TRUE if $a is equal to $b after type juggling.
$c = $a === $b;	// Identical	TRUE if $a is equal to $b, and they are of the same type.
$c = $a != $b;	// Not equal	TRUE if $a is not equal to $b after type juggling.
$c = $a <> $b;	// Not equal	TRUE if $a is not equal to $b after type juggling.
$c = $a !== $b;	// Not identical	TRUE if $a is not equal to $b, or they are not of the same type.
$c = $a < $b;	//Less than	TRUE if $a is strictly less than $b.
$c = $a > $b;	//Greater than	TRUE if $a is strictly greater than $b.
$c = $a <= $b;	//Less than or equal to	TRUE if $a is less than or equal to $b.
$c = $a >= $b;	//Greater than or equal to	TRUE if $a is greater than or equal to $b.

?>
