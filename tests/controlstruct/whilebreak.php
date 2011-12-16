<?php

$arr[0] = "one";

while (list(, $val) = each($arr)) {
    if ($val == "stop") {
        break;    /* You could also write 'break 1;' here. */
    }
    echo "$val<br />\n";
}

?>