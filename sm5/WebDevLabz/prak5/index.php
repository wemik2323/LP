<html>
<head>
<title>Вывод на экран и переменные в РНР</title>
</head>
<body>
<?php
    $b = $a = 5;
    echo "<br>переменная a=$a, b=$b";
    $c = $a++;
    echo "<br>переменная a=$a, c=$c";
    $e = $d = ++$b;
    echo "<br>переменная e=$e, d=$d, b=$b";
    $g = 10;
    $h = $g += 10;
    echo "<br>переменная g=$g, h=$h";
?>
</body>
</html>