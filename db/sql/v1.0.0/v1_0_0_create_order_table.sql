CREATE TABLE [IF NOT EXISTS] TBL_ORDER(
  id int AUTO_INCREMENT PRIMARY KEY,
  order_type varchar(50) not null,
  price double not null,
  quantity double not null,
  exchange varchar(50) not null,
  created_at TIMESTAMP not null,
  coin_pair varchar(50) not null
)