open Countrydata
open Country

exception NotValidMove
exception NotMovingValidTroops
let edge_search (c1, c2) lst=
  match (c1, c2) with 
  |(a,b) when List.mem (a,b) lst->true
  |(a,b) when List.mem (b,a) lst->true
  |_->false

let rec move_troops (c1, c2) num_troops lst=
match lst with
|[]->[]
|h::t when h=c1->
  [{name=h.name; id=h.id; 
state={owner=h.state.owner; troops=h.state.troops-num_troops}}] @ 
move_troops (c1, c2) num_troops t
|h::t when h=c2 ->
  [{name=h.name; id=h.id; 
state={owner=h.state.owner; troops=h.state.troops+num_troops}}] @ 
move_troops (c1, c2) num_troops t
|h::t->[h] @ move_troops (c1, c2) num_troops t

(**[move_phase (c1, c2) num_troops st] takes in a tuple of countries 
  [(c1, c2)] and moves [num_troops] from [c1] to [c2] and returns a new [st].
  Requires:[c1] and [c2] are different countries.
  Raises:When [num_troops]<=0 and [num_troops]>troops in [c1]. When [(c1, c2)] are 
  not a valid edge.*)
let move_phase (c1, c2) num_troops st=
if(c1.state.troops>num_troops && num_troops>0) then
  let check=edge_search (c1.name, c2.name) edges in
  match check with
  |true-> move_troops (c1,c2) num_troops st
  |false->raise NotValidMove
else raise NotMovingValidTroops