--------------------------------------------------------------------------------
-- init.sql: Initialize composite-realworld's database
--------------------------------------------------------------------------------

CREATE DATABASE crw-db;
CREATE USER     crw-user;

GRANT ALL PRIVILEGES ON DATABASE crw-db TO crw-user;
