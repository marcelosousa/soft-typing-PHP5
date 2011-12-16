<?php 
$myFunction = function() { 
      echo 1; 
}; 

if(is_callable($myFunction)) { 
      $myFunction(); 
} 
?>