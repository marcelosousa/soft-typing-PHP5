<?php
# use of unset

$name = "John Doe";
unset($name);
if (isset($name)) {
    print '$name is set';
}else{
    print '$name is not set';
}

?>