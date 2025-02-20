<!DOCTYPE html>
<html>
<head>
<title>Embed PHP in a .html File</title>
</head>
<body>
    <h1> 
        <?php
        $var1=$_GET['var1']; 
        $var2=$_GET['var2'];
            echo "$var1 + $var2 = "; 
            echo $var1 + $var2;
        ?>
    </h1>
</body>
</html>