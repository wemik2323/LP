<?php
    $error = null;
    if ($_SERVER['REQUEST_METHOD'] == 'POST') 
    { 
        $surname = $_POST['family']; $markProg = $_POST['markProg']; $markMath = $_POST['markMath']; $markWeb = $_POST['markWeb'];
        $error = empty($surname) || empty($markProg) || empty($markMath) || empty($markWeb);
    }
?>
<!DOCTYPE html>
<html>
<head>
    <title>Отправка данных</title>
</head>
<body>
    <?php
    if ($error == true || is_null($error)) { ?>
        <form action="<?=$_SERVER['PHP_SELF']?>" method="post"> 
            <input type="text" name="family" value="" placeholder="Фамилия" required="required" /><br>
            <label>Оценка по дисциплине «Программирование»</label>
            <input type="number" value="<?=$markProg?>" name="markProg" min="2" max="5" required="required" /><br>
            <label>Оценка по дисциплине «Математика»</label>
            <input type="number" value="<?=$markMath?>" name="markMath" min="2" max="5" required="required"
            /><br>
            <label>Оценка по дисциплине «Web-технологии»</label>
            <input type="number" value="<?=$markWeb?>" name="markWeb" min="2" max="5" required="required" /><br>
            <?php
            if ($error == true)
            echo "<label>Ошибка отправки! Некоторые данные не были введены! Попробуйте снова!</label><br>";
            ?>
            <input type="submit" value="Отправить" />
        </form>
        <?php
    }
    else { ?>
    <h3>Фамилия: <?=$surname?></h3>
    <p>Оценка по дисциплине «Программирование»: <?=$markProg?></p> 
    <p>Оценка по дисциплине «Математика»: <?=$markMath?></p> 
    <p>Оценка по дисциплине «Web-технологии»: <?=$markWeb?></p> 
    <p>Средний балл: <?=($markProg + $markMath + $markWeb) / 3?></p>
    <p>Тип стипендии: <?=($markProg == 5 && $markMath == 5 && $markWeb == 5 ? 'Есть' : 'Нет')?></p>
    <?php
    }
    ?>
</body>
</html>