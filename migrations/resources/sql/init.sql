--------------------------------------------------------------------------------
-- init.sql: Initialize composite-realworld's database
--------------------------------------------------------------------------------

CREATE DATABASE crw_db;
CREATE USER     crw_user;

GRANT ALL PRIVILEGES ON DATABASE crw_db TO crw_user;
