open World
open Player


let blank_world = {
  all_continents = Continentdata.all_continents;
  edges = Countrydata.edges;
  all_countries = Countrydata.all_countries;
  players = [];
  deck = [];
  sets_traded_in = 0;
  turn_color = Blank;
  turn = -1
}

let init_game players = 
  {blank_world with players = players}

let check_valid_game world =
  check_players world.players && 
  world.turn_color != Blank &&
  world.turn >= 0
    
let next_turn world =
  let next_world = 
    {world with 
    turn_color = (next_player world.players world.turn_color).color;
    turn = world.turn + 1}
  in next_world
  (* |> gainphase 
  |> attack 
  |> move *)