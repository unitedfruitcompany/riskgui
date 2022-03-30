
(** Custom assoc functions to help with edge search ***************************)
(******************************************************************************)

(** [insert k v lst] is an association list that binds key [k] to value [v]
and otherwise is the same as [lst] *)
let insert k v lst = (k, v) :: lst

(** [find k lst] is [Some v] if association list [lst] binds key [k] to
    value [v]; and is [None] if [lst] does not bind [k]. *)
let rec multi_lookup k = function
| [] -> []
| (k', v') :: t -> 
  if k' = k
    then v' :: multi_lookup k t
  else if v' = k
    then k' :: multi_lookup k t
  else multi_lookup k t


(** Just some custom set functions ********************************************)
(******************************************************************************)

let rec element_exists 
  (lst : 'a list)
  (element : 'a)
  =
  match lst with
  | [] -> false
  | h :: t -> 
    if h = element then true 
    else element_exists t element

let rec is_subset 
  (subset : 'a list) 
  (superset : 'a list) 
  = match subset with
  | [] -> true
  | h :: t -> 
    if element_exists superset h 
      then is_subset t superset
    else false

    
(** Custom list functions *****************************************************)
(******************************************************************************)

let rec remove_element list element =
  match list with 
  | [] -> []
  | h :: t -> 
    if h = element then
      t
    else 
      h :: remove_element list element