<?php
    echo "<p>1. IP-адрес удаленного компьютера, метод пересылки данных: "; 
    echo $_SERVER['REMOTE_ADDR'] . ", " . $_SERVER['REQUEST_METHOD']."</p>";
    echo "<p>2. Программное обеспечение сервера, протокол передачи данных: ";
    echo $_SERVER['SERVER_SOFTWARE'] . ", " . $_SERVER['SERVER_PROTOCOL']."</p>";
    echo "<p>3. Каталог для хранения документов на сервере, IP-адрес сервера:";
    echo $_SERVER['DOCUMENT_ROOT'] . ", " . $_SERVER['SERVER_ADDR']."</p>"; 
    echo "<p>4. Почтовый адрес администратора сети, имя хост-компьютера: "; 
    echo $_SERVER['SERVER_ADMIN'] . ", " . $_SERVER['HTTP_HOST']."</p>";
?>