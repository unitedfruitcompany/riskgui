open Misc_helpers

exception InvalidCard of string

type card_type = Man | Horse | Cannon | WildCard

type card = {
  country_name : string;
  card_type : card_type
}


(** For printing players ****************************************************)
(******************************************************************************)

let string_of_card_type t =
  match t with 
  | Man -> "Man"
  | Horse -> "Horse"
  | Cannon -> "Cannon"
  | WildCard -> "Wildcard"

let string_of_card c (offset : string)= 
  let open Printf in
  sprintf("%s(country_name: %s; card_type: %s)") 
  offset
  c.country_name
  (string_of_card_type c.card_type)

let rec string_of_card_list 
  (lst : card list)
  (offset : string)
  = 
  let open Printf in
  match lst with
  | [] -> ""
  | h :: t -> 
    sprintf("%s\n%s") (string_of_card h offset) (string_of_card_list t offset)


(** For retrieving cards ******************************************************)
(******************************************************************************)

let rec get_card_by_name 
  (cards : card list)
  (country_name : string)
  =
  match cards with 
  | [] -> raise (InvalidCard (country_name ^ " is not a card the player owns!"))
  | h :: t -> 
    if h.country_name = country_name then true 
    else get_card_by_name t country_name


(** For checking card matches *************************************************)
(******************************************************************************)

let type_check cards =
  match cards with
  | c1 :: c2 :: c3 :: [] -> 
    begin
    match (c1.card_type, c2.card_type, c3.card_type) with
    | (t1, t2, t3) -> 
      if t1 = t2 && t2 = t3 && t1 = t3 then true
      else if t1 <> t2 && t2 <> t3 && t1 <> t3 then true
      else if t1 = WildCard || t2 = WildCard || t3 = WildCard then true
      else false
    end
  | _ -> false
 
let is_match 
  (check : card list)
  (cards : card list) 
  =
  List.length check = 3 &&
  is_subset check cards &&
  type_check check
  

(** For calcuating a card sets trade in bonus *********************************)
(******************************************************************************)

let rec country_bonus
  (country_names : string list)
  (cards : card list)
  (acc : int)
  =
  match cards with
  | [] -> acc
  | h :: t -> 
    if element_exists country_names h.country_name then
      country_bonus country_names t (acc + 2)
    else 
      country_bonus country_names t acc

let cards_bonus 
  (country_names : string list)
  (sets_traded_in : int)
  (cards : card list)
  =
  let base_bonus = 
    if sets_traded_in = 0 then 4
    else if sets_traded_in >= 1 && sets_traded_in <= 4 
      then 4 + sets_traded_in * 2
    else if sets_traded_in = 5 then 15
    else 15 + (sets_traded_in - 5) * 5
  in
  base_bonus + country_bonus country_names cards 0