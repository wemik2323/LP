USE `university`; 
DELIMITER // 
CREATE TRIGGER insertResult AFTER INSERT ON users 
    FOR EACH ROW 
   BEGIN 
      INSERT INTO results SET laboratory=false, examination = 0, u_id = NEW.id; 
    END;// 