# db-model

A new relational database interface for Haskell.

`db-model` is a relational database interface that currently wraps on Database.HDBC and Data.Aeson.

__Features__

  * No need to write any SQL statement.
  
  * Naturally handles many-to-one table relations.
  
  * No need for Template Haskell.
  
  * Type safe.
  
  * Support for all databases that Database.HDBC supports (MySQL, Oracle, PostgreSQL, ODBC-compliant databases, etc.)
  
__Show case__

```Haskell
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
      
       liftIO $ print (ex :: User Value) 
      >>> User { key = Val 3, name = Val "bob", email = Val "bob@example.com" }
      
       save ex 
      --  prepares INSERT INTO user_table (user_name, user_email) VALUES (?, ?) 
      --  execute the statement with 
      --  user_name="YU LI", 
      --  user_email="bob@example.com" 
      --  fetch last_insert_id(); 
      
       liftIO $ print $ ex # key
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
      
```
