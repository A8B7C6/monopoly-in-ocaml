type _balance = {
  mutable total : int;
  mutable fivehun : int;
  mutable hun : int;
  mutable ffty : int;
  mutable twnty : int;
  mutable tens : int;
  mutable fives : int;
  mutable ones : int;
}

let init_balance =
  {
    total = 1500;
    fivehun = 2;
    hun = 2;
    ffty = 2;
    twnty = 6;
    tens = 5;
    fives = 5;
    ones = 5;
  }

let add_one b =
  b.ones <- b.ones + 1;
  b.total <- b.total + 1

let add_five b =
  b.fives <- b.fives + 1;
  b.total <- b.total + 5

let add_ten b =
  b.tens <- b.tens + 1;
  b.total <- b.total + 10

let add_twenty b =
  b.twnty <- b.twnty + 20;
  b.total <- b.total + 20

let add_fifty b =
  b.ffty <- b.ffty + 1;
  b.total <- b.total + 50

let add_hun b =
  b.hun <- b.hun + 1;
  b.total <- b.total + 100

let add_five_hundred b =
  b.fivehun <- b.fivehun + 1;
  b.total <- b.total + 500

let deduct_one b =
  b.ones <- b.ones - 1;
  b.total <- b.total - 1

let deduct_five b =
  b.fives <- b.fives - 1;
  b.total <- b.total - 5

let deduct_ten b =
  b.tens <- b.tens - 1;
  b.total <- b.total - 10

let deduct_twenty b =
  b.twnty <- b.twnty - 1;
  b.total <- b.total - 20

let deduct_fifty b =
  b.ffty <- b.ffty - 1;
  b.total <- b.total - 50

let deduct_hundred b =
  b.hun <- b.hun - 1;
  b.total <- b.total - 100

let deduct_five_hundred b =
  b.fivehun <- b.fivehun - 1;
  b.total <- b.total - 500

let rec add_to_balance b amt =
  if amt > 500 then
    let _ = add_five_hundred b in
    add_to_balance b (amt - 500)
  else if amt > 100 then
    let _ = add_hun b in
    add_to_balance b (amt - 100)
  else if amt > 50 then
    let _ = add_fifty b in
    add_to_balance b (amt - 50)
  else if amt > 20 then
    let _ = add_twenty b in
    add_to_balance b (amt - 20)
  else if amt > 10 then
    let _ = add_ten b in
    add_to_balance b (amt - 10)
  else if amt > 5 then
    let _ = add_five b in
    add_to_balance b (amt - 5)
  else if amt >= 1 then
    let _ = add_one b in
    add_to_balance b (amt - 1)
  else b

let rec deduct_from_balance b amt =
  if amt < 0 then add_to_balance b (amt * -1)
  else if b.ones > 0 then
    let _ = deduct_one b in
    deduct_from_balance b (amt - 1)
  else if b.fives > 0 then
    let _ = deduct_five b in
    deduct_from_balance b (amt - 5)
  else if b.tens > 0 then
    let _ = deduct_ten b in
    deduct_from_balance b (amt - 10)
  else if b.twnty > 0 then
    let _ = deduct_twenty b in
    deduct_from_balance b (amt - 20)
  else if b.ffty > 0 then
    let _ = deduct_fifty b in
    deduct_from_balance b (amt - 50)
  else if b.hun > 0 then
    let _ = deduct_hundred b in
    deduct_from_balance b (amt - 100)
  else if b.fivehun > 0 then
    let _ = deduct_five_hundred b in
    deduct_from_balance b (amt - 500)
  else b

let make_balance total fivehun hun ffty twnty tens fives ones =
  { total; fivehun; hun; ffty; twnty; tens; fives; ones }