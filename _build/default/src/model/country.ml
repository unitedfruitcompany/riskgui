open Player

exception InvalidID of string
exception InvalidName of string
exception InvalidCountry of string

type state = { 
  owner : color; 
  troops : int
}

type country_info = {
  name : string;
  id : string;
  state : state
}


(** For printing countries ****************************************************)
(******************************************************************************)

let string_of_country country = 
  "(name: " ^ country.name ^ 
  "; owner: " ^ (string_of_color country.state.owner) ^ 
  "; troops: " ^ (string_of_int country.state.troops) ^ ")"

let rec print_countries_state_helper country_list = 
  match country_list with 
  | [] -> ()
  | h :: t -> 
    let _ = print_endline (string_of_country h) in
    print_countries_state_helper t

let print_countries_state country_list = 
  let _ = Printf.printf "Countries: \n\n" in
    print_countries_state_helper country_list

    
(** For retrieveing countries *************************************************)
(******************************************************************************)

let rec country_by_id (lst : country_info list) id  = 
  match lst with 
  | [] -> raise (InvalidID (id ^ " is an invalid id") )
  | h :: t -> if h.id = id then h else country_by_id t id

let rec country_by_name (lst : country_info list) name  = 
  match lst with 
  | [] -> raise (InvalidName (name ^ " is an invalid name") )
  | h :: t -> if h.name = name then h else country_by_name t name

let rec country_name_exists_countries (lst : country_info list) name  =
  match lst with 
  | [] -> false
  | h :: t -> 
    if h.name = name then true 
    else country_name_exists_countries t name

let rec country_name_exists_string (country_names : string list) name  =
  match country_names with 
  | [] -> false
  | h :: t -> if h = name then true else country_name_exists_string t name


(** For updating a single country and returning the updated country ***********)
(******************************************************************************)

let update_state_of_country (changed_state : state) (country : country_info) = 
  {country with state = changed_state}

let update_owner_of_country (changed_owner : color) (country : country_info) = 
  {country with state = {country.state with owner = changed_owner}}

let update_troops_of_country (changed_troops : int) (country : country_info) = 
  {country with state = {country.state with troops = changed_troops}}


(** For updating the entire countries list ************************************)
(******************************************************************************)

let update_country (new_country : country_info) (old_country : country_info) = 
  if old_country.name = new_country.name then
    new_country
  else old_country

let update_countries_with_country 
  (all_countries : country_info list)
  (changed_country : country_info) 
  = 
  if country_name_exists_countries all_countries changed_country.name  then
    List.map (update_country changed_country) all_countries
  else
    raise (InvalidCountry (changed_country.name ^ " doesn't exist!"))

let update_countries_with_state
  (all_countries : country_info list)
  (name : string)
  (changed_state : state) 
  = 
  update_countries_with_country
    all_countries
    (update_state_of_country 
      changed_state 
      (country_by_name all_countries name )) 

let update_countries_with_owner
  (all_countries : country_info list)
  (name : string)
  (changed_owner : color)
  =
  update_countries_with_country
    all_countries
    (update_owner_of_country 
      changed_owner 
      (country_by_name all_countries name))  

let update_countries_with_troops
  (all_countries : country_info list)
  (name : string)
  (changed_troops : int)
  =
  update_countries_with_country
    all_countries
    (update_troops_of_country 
      changed_troops
      (country_by_name all_countries name ))    

      



