CREATE PROCEDURE `university`.`getUserInfo` (IN param1 VARCHAR(45), OUT param2 INT) 
BEGIN 
    DECLARE UserName VARCHAR(45); 
    SET UserName = 'Ivan'; 
    
    IF param1 IS NOT NULL THEN 
        SET UserName = param1; 
    END IF; 
    
    SELECT d.name AS departments INTO @department FROM users u LEFT  JOIN departments d ON u.d_id = d.id WHERE u.name = UserName; 
    SELECT COUNT(*) INTO param2 FROM users; 
END