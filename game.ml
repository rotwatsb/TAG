type room =
  { description : string;
    items : string list;
    points : int;
    exits : (string * (string * string)) list;
    treasure : string list;
  }

type item =
  { i_description : string;
    i_points : int; (* points for dropping the item in its treasure room *)
    i_trs_msg : string; (* message shown after dropping item in treasure room *)
  }

type player =
  { room : string;
    inventory : string list;
    score : int;
    turns : int;
  }

type ('k, 'v) bstree =
  | Nil
  | Node of 'k * 'v * ('k, 'v) bstree * ('k, 'v) bstree

let rec bst_lookup k = function
  | Nil -> raise Not_found
  | Node (tk, tv, tl, tr) -> if (k = tk) then tv
			     else if (k < tk) then bst_lookup k tl
			     else bst_lookup k tr

let rec bst_insert k v = function
  | Nil -> Node (k, v, Nil, Nil)
  | Node (tk, tv, tl, tr) -> if (k < tk) then Node (tk, tv, (bst_insert k v tl), tr)
			     else if (k > tk) then Node (tk, tv, tl, (bst_insert k v tr))
			     else Node (k, v, tl, tr)
				       
(* Builds and returns a list of association lists, where each association list 
 * represents a list of exits particular to a specific room, where each exit
 * in the list defined is by a "direction" -> ("room" * "unlock_item") association
 *
 * exit_lists - a json list of 'Assoc lists
 *)
let rec build_exits exit_lists =
  let rec build_rmexits exits_list =
    let open Yojson.Basic.Util in
    match exits_list with
    | h::t -> (let open Yojson.Basic.Util in
	       match h with
	       | (_,d)::(_,r)::(_,u)::_ -> ((String.uppercase (to_string d)),
					    ((String.uppercase (to_string r)),
					     (String.uppercase (to_string u))))
					   ::(build_rmexits t)
	       | _ -> [])
    | [] -> [] in
  match exit_lists with
  | h::t -> let open Yojson.Basic.Util in
	    (build_rmexits (filter_assoc h))::(build_exits t)
  | [] -> []

(* Builds and returns a binary search tree for all the items in the game, where
 * each node in the tree is a key-value pair mapping item names to item records
 * 
 * json - the json object for the game file
 *)
let build_items json =
  let open Yojson.Basic.Util in
  let rec build_items_tree items_tree items_list =
    match items_list with
    | h::t -> (match h with
	       | (_,id)::(_,d)::(_,p)::(_,m)::_ ->
		  let id' = String.uppercase (to_string id) in
		  let d' = to_string d in
		  let p' = to_int p in
		  let m' = to_string m in
		  build_items_tree (bst_insert id'
					       {i_description=d';
						i_points=p';
						i_trs_msg=m'}
					       items_tree) t
	       | _ -> Nil)
    | [] -> items_tree in
  let items_list = [json] |> filter_member "items" |> flatten |> filter_assoc in
  build_items_tree Nil items_list

(* Builds and returns a player record, setting the player's room to the
 * "start_room" defined in the json schema
 *
 * json - the json object for the game file
 *)
let build_player json =
  let open Yojson.Basic.Util in
  let r = String.uppercase ([json] |> filter_member "start_room" |> filter_string |> String.concat "") in
  let inv = List.map String.uppercase ([json] |> filter_member "start_items" |> flatten |> filter_string) in
  {room=r;inventory=inv;score=0;turns=0}

(* Builds and returns a binary search tree for all the rooms in the game, where
 * each node is a key-value pair mapping a room "id" to a room record 
 *
 * rooms - the binary search tree
 * roomids - the list of room ids
 * descriptions - the list of room descriptions
 * points - the list of points that each room is worth
 * exits - the list of exits (each a list itself) for the rooms
 * treausres - the list of treasure items for each room
 *)  
let rec build_rooms rooms roomids descriptions items points exits treasures =
  let open Yojson.Basic.Util in
  match (roomids, descriptions, items, points, exits, treasures) with
  | (r::rs, d::ds, i::is, p::ps, e::es, t::ts) ->
     let room = {description=d;
		 items=List.map String.uppercase (filter_string i);
		 points=p;
		 exits=e;
		 treasure=List.map String.uppercase (filter_string t)} in
     build_rooms (bst_insert (String.uppercase r) room rooms) rs ds is ps es ts
  | _ -> rooms

(* Builds and returns the game model, where the model is 3-tuple consisting of:
 * - a binary search tree of room records
 * - a player record
 * - a binary search tree of game items
 *)
let init_model game_file =
  let json = Yojson.Basic.from_file game_file in
  let open Yojson.Basic.Util in
  let helper = [json] |> filter_member "rooms" |> flatten in
  let roomids = helper |> filter_member "id" |> filter_string in
  let descriptions =  helper |> filter_member "description" |> filter_string in
  let items =  helper |> filter_member "items" |> filter_list in
  let points =  helper |> filter_member "points" |> filter_int in
  let exit_lists =  helper |> filter_member "exits" |> filter_list in
  let exits = build_exits exit_lists in
  let treasures =  helper |> filter_member "treasure" |> filter_list in
  (build_rooms Nil roomids descriptions items points exits treasures,
   build_items json,
   build_player json)

let inform info =
  print_string info;
  print_newline ()

(* Print the items_list and their descriptions, as found in items_tree, to the
 * console
 *)		
let inform_items items_list items_tree =
  let rec inform_items_helper items_list items_tree =
    match items_list with
    | (h::t) -> inform (String.concat " " [h; "-"; (bst_lookup h items_tree).i_description]); inform_items_helper t items_tree
    | [] -> () in
  inform_items_helper items_list items_tree
		      

let rec print_list = function
  | h::t -> (inform h); print_list t
  | [] -> ()


(* Top level function for parsing and processing user commands, returning
 * an updated game model once all commands that can be parsed have been processed.
 * The function is indirectly recursive -- specific command functions will call
 * act_on again, allowing the player to enter multiple commands at once.
 * 
 * Valid commands are:
 * LOOK - redisplay discription of current room and any items in the room
 * TAKE - transfer item from room to inventory
 * DROP - transfer tiem from inventory to room
 * INV or INVENTORY - display player's inventory
 * SCORE - display player's score
 * TURN - display 
 * 
 * command_list - a string list of user input
 * model - current state of the game
 *)
let rec act_on command_list model =
  let actions = [("LOOK", look);
		 ("TAKE", take);
		 ("DROP", drop);
		 ("INV", inv);
		 ("INVENTORY", inv);
		 ("SCORE", score);
		 ("TURN", turn);
		 ("GO", go)] in
  match command_list with
  | (h::t) -> (try ((List.assoc h actions) t model) with
		 Not_found -> (try go command_list model with
				 Not_found -> bad_command command_list model))
  | [] -> model

and look command_list model =
  match (command_list, model) with
  | (_,(rms,itms,plr)) ->
     let rm = bst_lookup plr.room rms in
     inform rm.description;
     if (List.length rm.items > 0)
     then (inform "\nThe room contains:";
	   inform_items rm.items itms;)
     else ();
     act_on command_list model


and take command_list model =
  match (command_list, model) with
  | ((h::t),(rms,itms,plr)) ->
     (* get the room the player is in *)
     (try let rm = (bst_lookup plr.room rms) in
	  (* check to see if item being asked for exists in the room *)
          let z = List.find (fun x -> x=h) rm.items in
	  (* it exists, so remove it from the room's item list *)
	  let rm_items' = List.filter (fun x -> x!=z) rm.items in
	  (* and add it to the player's inventory *)
	  let plr_inv' = z::plr.inventory in
	  (* deduct points from player if removing item from treasure room *)
	  let score' = try (let y = List.find (fun x -> x=z) rm.treasure in
			    (* and tell them they're doing so *)
			    inform "Item taken from treasure location!";
			    plr.score - (bst_lookup y itms).i_points)
		       with Not_found -> plr.score in
	  (* create updated player record *)
	  let plr' = {room=plr.room;
		      inventory=plr_inv';
		      score=score';
		      turns=plr.turns + 1} in
	  (* create updated room record *)
	  let rm' = {description=rm.description;
		     items=rm_items';
		     points=rm.points;
		     exits=rm.exits;
		     treasure=rm.treasure} in
	  (* create updated rooms tree *)
	  let rms' = (bst_insert plr.room rm' rms) in
	  inform (String.concat " " [z;"has been added to your inventory."]);
	  take t (rms',itms,plr')
	       (* if the item wasn't found, try 'taking' the concatenation of
		* the item with next command word -- might be two word item *)
      with Not_found -> (try take ((String.concat " " [h; (List.hd t)])::(List.tl t)) model
			 (* if any of that fails, just ask act_on to handle *)
			 with _ -> act_on command_list model))
  | ([],(rms,itms,plr)) -> model
     

(* drop logic is very similar to take. The code for both could possibly
 * be combined utilizing some generalized 'transfer' function. But given certain
 * specific actions in each case, like displaying the treasure message in drop,
 * and having to access 'from' and 'to' records differently for player or room, 
 * their functionality is kept independent from one another, if only for
 * ease of coding
 *)
and drop command_list model =
  match (command_list, model) with
  | ((h::t),(rms,itms,plr)) ->
     (try let rm = (bst_lookup plr.room rms) in
          let z = List.find (fun x -> x=h) plr.inventory in
	  let rm_items' = z::rm.items in
	  let plr_inv' = List.filter (fun x -> x!=z) plr.inventory in
	  let plr_score' = try (let y = List.find (fun x -> x=z) rm.treasure in
				let itm = bst_lookup y itms in
				(* make sure to show treasure message *)
				inform itm.i_trs_msg;
				plr.score + itm.i_points)
			   with Not_found -> plr.score in
	  let plr'= {room=plr.room;
		     inventory=plr_inv';
		     score=plr_score';
		     turns=plr.turns + 1} in
	  let rm' = {description=rm.description;
		     items=rm_items';
		     points=rm.points;
		     exits=rm.exits;
		     treasure=rm.treasure} in
	  let rms' = (bst_insert plr.room rm' rms) in
	  inform (String.concat " " [z; "has been dropped from your inventory."]);
	  drop t (rms',itms,plr') 
      with Not_found -> (try drop ((String.concat " " [h; (List.hd t)])::(List.tl t)) model
			 with _ -> act_on command_list model))
  | ([],_) -> model

and score command_list model =
  match (command_list, model) with
  | (_,(_,_,plr)) ->
     inform (String.concat " " ["You've earned"; string_of_int plr.score; "ponts so far."]);
     act_on command_list model

and turn command_list model =
  match (command_list, model) with
  | (_,(_,_,plr)) ->
     inform (String.concat " " ["You've taken"; string_of_int plr.turns; "turns."]);
     act_on command_list model

and inv command_list model =
  match (command_list, model) with
  | (_,(_,itms,plr)) ->
     if (List.length plr.inventory > 0)
     then inform_items plr.inventory itms
     else inform "Nothing in your inventory right now.";
     act_on command_list model

and go command_list model =
  match (command_list, model) with
  | ((h::t),(rms,itms,plr)) ->
     (* get the exits for the player's room *)
     let exts = (bst_lookup plr.room rms).exits in
     (try let (ext_rm_id,unlock_item) = List.assoc h exts in
	  let rm = bst_lookup ext_rm_id rms in
	  let rm_points = rm.points in
	  (* now that room has been reached, change its point value to 0 *)
	  let rm' = {description=rm.description;
		     items=rm.items;
		     points=0;
		     exits=rm.exits;
		     treasure=rm.treasure} in
	  (* create updated player record: change room and update score *)
	  let plr' = {room=ext_rm_id;
		      inventory=plr.inventory;
		      score=plr.score + rm_points;
		      turns=plr.turns + 1} in
	  (* create updated rooms tree *)
	  let rms' = bst_insert ext_rm_id rm' rms in
	  (* if the room isn't locked, return updated model and LOOK *)
	  if ((unlock_item = "") ||
		(List.exists (fun x -> x=unlock_item) plr.inventory)) then
	    act_on ("LOOK"::t) (rms',itms,plr')
	  else (inform "You don't have what you need to go there.";
		act_on t model)
      (* if the exit wasn't found, try to 'go' to the concatenation of
       * the current exit with next command word -- might be two word exit *)
      with Not_found -> (try go ((String.concat " " [h; (List.hd t)])::(List.tl t)) model
				(* if that fails, tell the player *)
			 with _ -> bad_command command_list model))
  | ([],_) -> model				  

and bad_command command_list model =
  (match command_list with
   | h::t -> inform ("Don't understand input '" ^ h ^ "'")
   | [] -> ());
  model

(* simple Read-Eval-Print-Loop
 * tail-recursive so no risk of stack overflow
 *)
let rec repl model =
  let s = print_newline ();
	  print_string "Now what? ... ";
	  read_line () in
  if s = "quit" then
    ()
  else
    repl (act_on (Str.split (Str.regexp "[ \t]+") (String.uppercase s)) model)

(* start repl on model built from specified game file *)
let () = let open Sys in
	 let open Array in
	 repl (inform "Standard commands include LOOK, TAKE, DROP, GO, SCORE, TURN, INV or INVENTORY.\n";
	       look [] (init_model (List.hd (List.tl (Array.to_list Sys.argv)))))
