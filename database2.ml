(*
                         ATM Account Database
                              Version 2
                          (Functional Map)
 *)

(* In this second implementation of the `Database` module type, the
concrete data structure is that used in OCaml's `Map` module, a module
for functional (no-side-effect) dictionaries. Since the `Map` module
implementation is (at least in its current version) based on balanced
trees, its performance for searching for a key is logarithmic, far
better than the linear time searching in the association list
implementation of Database.

Because OCaml `Map`s are pure data structures (no side effects), we
need to handle the storing and updating ourselves. We do so by storing
the database in a global variable `db` of `ref` type, as in Version 1
of the `Database` module.

Note that swapping in this implementation necessitates no changes to
the interface for the `Database` module, and hence no changes to the
implementation of ATMcomponents. *)

(* Customer account identifiers *)
type id = int ;;
            
(* Maps (dictionaries) with integer keys *)
module IntMap = Map.Make (struct
                           type t = int
                           let compare : int -> int -> int = compare
                         end) ;;

(* The account database itself, a reference to a map from ints to
   pairs of name and balance, initially the empty map. *)
let db : (string * int) IntMap.t ref = ref IntMap.empty ;;
                                              
(* See .mli file for documentation on individual functions *)

let create (id : id) (name : string) : unit =
  db := IntMap.add id (name, 0) !db ;;
   
let find (id : id) : string * int =
  IntMap.find id !db ;;

let exists (id : id) : bool =
  IntMap.exists (fun key _value -> key = id) !db ;;
                                                                        
let balance (id : id) : int =
  let _name, bal = find id
  in bal ;;
  
let name (id : id) : string =
  let name, _bal = find id
  in name ;;
  
let update (id : id) (value : int) : unit =
  let nam = name id in
  db := IntMap.add id (nam, value) !db ;;

let close (id : id) : unit =
  db := IntMap.remove id !db ;;

let dump () =
  !db
  |> IntMap.iter (fun i (nam, bal) ->
                  Printf.printf "[%d] %s -> %d\n" i nam bal) ;;
