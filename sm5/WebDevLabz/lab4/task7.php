<?php
$str = array_key_exists('string', $_POST) ? $_POST['string'] : null;
$count = null;
if ($_SERVER['REQUEST_METHOD'] == 'POST' && !is_null($str)) $count = count(preg_split('/[\s,]+/', $str));
?>
<!DOCTYPE html>
<html>
<head>
<title>Число слов в строке</title>
</head>
<body>
    <p>Посчитать число слов в строке:</p>
    <form action="<?=$_SERVER['PHP_SELF']?>" method='post'> 
        <input type="text" name="string" /><br>
        <input type="submit" value="Отправить" />
    </form>
    <?php
    if (!is_null($count)) { ?>
        <p>Текст: <?=$str?></p> <p>Слов: <?=$count?></p> <?php
    }
    ?>
</body>
</html>