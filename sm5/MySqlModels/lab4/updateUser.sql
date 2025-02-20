USE `university`; 
DELIMITER // 
CREATE TRIGGER updateUser AFTER UPDATE ON results 
    FOR EACH ROW 
    BEGIN 
      UPDATE users Set isupdate=true WHERE id = NEW.u_id;    
    END;// 
