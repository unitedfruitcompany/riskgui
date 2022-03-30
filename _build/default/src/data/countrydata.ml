open Country

let america_b = { name = "America"; id = "1"; state = {owner = Blank; troops = 0}}
let china_b = { name = "China"; id = "2"; state = {owner = Blank; troops = 0}}
let canada_b = { name = "Canada"; id = "3"; state = {owner = Blank; troops = 0}}
let all_countries = [america_b; china_b; canada_b]

let edges = 
  [
    ("America", "Canada"); 
  ]

