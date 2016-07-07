# db-model

This is an interface designed to put data from multiple tables into a Haskell data.

The module uses Database.HDBC as the backend, so it supports whatever Database.HDBC supports.

  # Show case
  
    data User m = User {
       key   :: m Integer,
       name  :: m String,
       email :: m String
    } deriving (Generic)
   
    instance MultiTable User where
        relation = User {
           key   = IsKey [("user_table", "user_id")],
           name  = IsCol "user_table" "user_name",
           email = IsCol "user_table" "user_email"
        }
      
    showCase :: Model ()
    showCase = do
    
       ex <- load (name =. "bob") 
      --  prepares SELECT user_id, user_name, user_email FROM user_table WHERE user_name=? 
      --  execute the statement with user_name="bob" 
      
       print (ex :: User Value) 
      >>> User { key = Val 3, name = Val "bob", email = Val "bob@example.com" }
      
       save ex 
      --  prepares INSERT INTO user_table (user_name, user_email) VALUES (?, ?) 
      --  execute the statement with 
      --  user_name="YU LI", 
      --  user_email="bob@example.com" 
      --  fetch last_insert_id(); 
      
       print $ ex # key 
      >>> 219  -- new row inserted with user_id=219

       update (ex {email = Val "ylilarry@gmail.com"}) 
      --  prepares UPDATE user_table SET user_name=?, user_email=? WHERE user_id=? 
      --  execute the statement with 
      --  user_name="YU LI", 
      --  user_email="ylilarry@example.com", 
      --  user_id=219 

       remove ex 
      --  prepares DELETE FROM user_table WHERE user_id=? 
      --  execute the statement with 
      --  user_id=219 
      
