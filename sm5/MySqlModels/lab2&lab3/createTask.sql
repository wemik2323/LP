CREATE PROCEDURE `university`.`createTask` (IN tname VARCHAR(45), IN tdate DATETIME, OUT muchdays VARCHAR(45)) 
BEGIN 
    DECLARE tmonth VARCHAR(45); 
    SELECT CONCAT('Task month is: ', 
                 (CASE MONTH(tdate) 
                        WHEN 1 THEN 'Jan' 
                        WHEN 2 THEN 'Feb' 
                        WHEN 3 THEN 'Mar' 
                        WHEN 4 THEN 'Apr' 
                        WHEN 5 THEN 'May' 
                        WHEN 6 THEN 'Jun' 
                        WHEN 7 THEN 'Jul' 
                        WHEN 8 THEN 'Aug' 
                        WHEN 9 THEN 'Sep' 
                        WHEN 10 THEN 'Oct' 
                        WHEN 11 THEN 'Nov' 
                        WHEN 12 THEN 'Dec' 
                        ELSE 'None' 
                        END 
                  )) INTO tmonth; 
    INSERT INTO tasks (taskname, taskday, taskmonth) VALUES (tname, DAY(tdate), tmonth); 
    SELECT CONCAT('Remains days: ', DATEDIFF(tdate, CURDATE())) INTO muchdays;    
END