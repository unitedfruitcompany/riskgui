open Country
open Countrydata
let rnd = Random.self_init ()
let num_comparisons lst1 lst2 = abs (List.length lst1 - List.length lst2)
  
let highest_roll lst1 lst2 : int = 
  let sorted_lst1 = List.sort compare lst1 in 
    let sorted_lst2 = List.sort compare lst2 in
      if List.length sorted_lst1 != 0 && List.length sorted_lst2 != 0 then 
        if List.hd sorted_lst1 > List.hd sorted_lst2 then 1
        else if List.hd sorted_lst1 < List.hd sorted_lst2 then 2
        else 3
      else 4
        
let rec roll_die die lst = 
  match die with 
  | 0 -> lst 
  | _ -> roll_die (die - 1) (Random.int 7 :: lst)

let rec update_countries self_country_id all_countries = 
  match all_countries with
  | [] -> []
  | h :: t -> if h.id = self_country_id then 
                let x = {h with state = {owner = h.state.owner; troops = h.state.troops - 1}} in x :: (update_countries self_country_id t)
              else h :: update_countries self_country_id t
let remove_troop self_country_id enemy_country_id all_countries num = 
  if(num == 2 || num == 3) then update_countries enemy_country_id all_countries
  else if (num == 1) then update_countries self_country_id all_countries
  else failwith "Error"
        
let rec full_attack_round self_country_id enemy_country_id all_countries rolls_1 rolls_2 num = 
  match num with 
  | 0 -> all_countries
  | _ -> let x = remove_troop self_country_id enemy_country_id all_countries (highest_roll rolls_1 rolls_2) in
            full_attack_round self_country_id enemy_country_id x (List.tl rolls_1) (List.tl rolls_2) (num-1)

(* [self_country_id] attacks [enemy_country_id] and prompts the players to pick number of die to roll [num_rolls] and [num_rolls_2]
    Requires: P1 and P2 pick valid numbers for num_rolls*)            
let attack_country (self_country_id : string) (enemy_country_id : string) (country_lst : country_info list) : country_info list = 
  if((country_by_id all_countries self_country_id).state.troops < 1 ||
    (country_by_id all_countries enemy_country_id).state.troops < 1) then failwith "Not enough troops to attack"
  else
  let () =  print_string "how many die does P1 wish to roll? " in 
    let num_rolls = read_int () in  
      let rolls_1 = roll_die num_rolls [] in
        let () = print_string "how many die does P2 wish to roll? " in
          let num_rolls_2 = read_int () in
            let rolls_2 = roll_die num_rolls_2 [] in
              full_attack_round self_country_id enemy_country_id country_lst rolls_1 rolls_2 (num_comparisons rolls_1 rolls_2)