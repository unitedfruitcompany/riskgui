open Lib
open CardData
open Bogue
module W = Widget
module L = Layout

let _ = print_endline (Continent.string_of_continent_list CountryData.all_countries ContinentData.all_continents)

let p1 : Player.player = 
  { color = Red; country_names = ["America"; "China"]; cards = [c1; c2; c3]; troop_queue = 0; alive = true }

let _ = print_endline (Player.string_of_player p1)

let main () =
  let input = W.text_input ~max_size:200 ~prompt:"Enter your name" () in
  let label = W.label ~size:10 (Player.string_of_player p1) in
  let layout = L.tower [L.resident ~w:1000 input;
                       L.resident ~w:1000 ~h:400 label] in
  let action ti l _ =
    let text = W.get_text ti in
    W.set_text l ("Hello " ^ text ^ "!") in
  let c = W.connect input label action Trigger.[text_input; key_down] in

  let board = Bogue.make [c] [layout] in
  Bogue.run board

let _ = main ();
  Draw.quit ()
