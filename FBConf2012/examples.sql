create table customers
(
id integer not null primary key,
cust_name varchar(100),
address blob sub_type 1,
postal_code varchar(10),
town varchar(50),
email varchar(50),
telephone varchar(15)
)