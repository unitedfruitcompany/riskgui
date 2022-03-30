open OUnit2
open Lib
open Testdata
open Country
open CountryData
open MovePhase
open Attack


(**[move_test name input_move input_troops input_st expected_output] 
    constructs an OUnit test named [name] that asserts the quality 
    of [expected_output] with [move_phase input_move input_troops input_st]*)
   let move_test
   (name : string)
   (input_move : country_info*country_info)
   (input_troops : int)
   (input_st : country_info list)
   (expected_output : country_info list) =
 name >:: fun _ ->
 (* the [printer] tells OUnit how to convert the output to a string *)
 assert_equal expected_output (move_phase input_move input_troops input_st)

(**[move_fail_test name input_move input_troops input_st expected_output] 
    constructs an OUnit test named [name] that the error of [expected_output] 
    raises with [move_phase input_move input_troops input_st]*)
   let move_fail_test
   (name : string)
   (input_move : country_info*country_info)
   (input_troops : int)
   (input_st : country_info list)
   (expected_output : exn) =
 name >:: fun _ -> assert_raises expected_output
 (fun _-> move_phase input_move input_troops input_st )

  let move_phase_test=
  [
    move_test "first moving test" (canada_d, america_b) 2 
    [america_b; china_b; canada_d] [america_c; china_b; canada_e];
    move_test "second moving test" (america_c, canada_c) 1
     [america_c; china_b; canada_c] [america_a; china_b; canada_d];
    move_fail_test "third moving test:too many troops" (canada_d, america_b) 3
    [america_b; china_b; canada_b] NotMovingValidTroops;
    move_fail_test "fourth moving test:0 or less troops" (canada_d, america_b) 0
    [america_b; china_b; canada_d] NotMovingValidTroops;
    move_fail_test "fifth moving test:No edge" (canada_d, australia_b) 2
    [australia_b; china_b; canada_d] NotValidMove
  ]

  let test_roll_die name num_rolls lst expected_output = 
    name >:: fun _ -> assert_equal expected_output (roll_die num_rolls lst)
  
  let test_attack_country name expected_output (self_country_id : string) (enemy_country_id : string) (country_lst : country_info list) =
    name >:: fun _ -> assert_equal expected_output (attack_country self_country_id enemy_country_id country_lst)
  
  let die_roll_tests = [
    test_roll_die "Testing rolling 3 die" 3 [] [];
    test_roll_die "Testing rolling 3 die" 0 [] [];
    test_roll_die "Testing rolling 3 die" 1 [] [];
  
  ]
  let attack_country_tests = [
    test_attack_country ("Country " ^ "Test_A" ^ "Attacks Test_B") [] "4" "5" test_all_countries;
    test_attack_country ("Country " ^ "Test_A" ^ "Attacks Test_B") [] "5" "4" test_all_countries;
    test_attack_country ("Country " ^ "Test_A" ^ "Attacks Test_B") [] "6" "7" test_all_countries;
    test_attack_country ("Country " ^ "Test_A" ^ "Attacks Test_B") [] "7" "6" test_all_countries;
  
  ] 
  
  let basic_test = [
    "Test" >:: fun _ -> assert_equal 1 1 ~printer:string_of_int
  ]

let suite =
  "Risk test suite"
  >::: List.flatten [move_phase_test; die_roll_tests; attack_country_tests; basic_test]

let _ = run_test_tt_main suite