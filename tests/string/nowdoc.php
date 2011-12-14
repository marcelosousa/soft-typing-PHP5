<?php

$recipient = "Jonh";
$sender = "Doe";

$x = <<<'END'
Dear $recipient,
 
I wish you to leave Sunnydale and never return.
 
Not Quite Love,
$sender
END;

echo strtolower($x);

?>