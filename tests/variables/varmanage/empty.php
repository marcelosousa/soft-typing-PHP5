<?php
# use of empty

$name = "";

/*
if(isset($name)){
    print '$name is set';
    print "\n";
}
*/

if(empty($name)){
    print 'Error: Forgot to specify a value for $name';
    print "\n";
}else{
    print $name;
}

?>