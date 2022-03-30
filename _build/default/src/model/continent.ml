open Country

type continent = {
  name : string;
  bonus : int;
  country_names : string list
}

let rec string_of_continent_helper country_list country_names = 
  match country_names with 
  | [] -> ""
  | h :: t -> Printf.sprintf("%s\n") 
    (h 
    |> country_by_name country_list 
    |> string_of_country) 
    ^ string_of_continent_helper country_list t

let string_of_continent country_list continent  = 
  Printf.sprintf("%s: \n\n") continent.name ^ 
  (string_of_continent_helper country_list continent.country_names)

let rec string_of_continent_list country_list continent_list = 
  match continent_list with 
  | [] -> ""
  | h :: t -> 
    Printf.sprintf("\n") 
    ^ (string_of_continent country_list h)
    ^ (string_of_continent_list country_list t)

