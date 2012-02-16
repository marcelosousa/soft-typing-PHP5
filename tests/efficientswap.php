<?PHP
$a=5;
$b=3;

//Please mind the order of these, as it's important for the outcome.

$a^=$b;
$b^=$a;
$a^=$b;

//echo $a."\n".$b."\n";
/* prints:
3
5
*/
?>