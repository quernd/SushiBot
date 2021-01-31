CREATE TABLE users (user_id int primary key not null, authorized bool);
CREATE TABLE messages (message_id int primary key not null, message text);
CREATE TABLE updates (update_id int primary key not null, message_id int);
CREATE TABLE photos (photo_id text primary key not null);
