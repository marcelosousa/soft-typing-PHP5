<?php
 
$name       = "Joe Smith";
$occupation = "Programmer";
echo <<<EOF
 
        This is a heredoc section.
        For more information talk to $name, your local $occupation.
 
        Thanks!
 
EOF;
 
?>