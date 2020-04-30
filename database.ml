(*
                         ATM Account Database
                              Version 1
                          (Association List)
 *)
            
(* In this implementation of the `Database` module, the account
database is an "association list" mapping each id to a pair of name
and balance. The database as a whole is stored as a reference so that
it can be updated. This implementation data structure has the sole
advantage of being trivial to implement. We even make use of some
association list functions in the `List` module to help in the
implementation. However, it is grossly inefficient.

We provide two further implementations of the `Database module`. See
`database2.ml` for an implementation using OCaml's `Map` module
(basically a module for functional (no-side-effect) dictionaries). See
`database3.ml` for an implementation as a mutable hash table using
OCaml's `Hashtbl` module. *)

(* Customer account identifiers *)
type id = int ;;
  
let db : (id * (string * int)) list ref = ref [] ;;
                                              
(* See .mli file for documentation on individual functions *)

let create (id : id) (name : string) : unit =
  db := (id,  (name, 0)) :: !db ;;
   
let find (id : id) : string * int =
  List.assoc id !db ;;

let exists (id : id) : bool =
  try
    ignore (find id);
    true
  with
  | Not_found -> false ;;
                                                                        
let balance (id : id) : int =
  let _name, bal = find id
  in bal ;;
  
let name (id : id) : string =
  let name, _bal = find id
  in name ;;
  
let update (id : id) (value : int) : unit =
  let nam, _oldval = find id in
  db := (id, (nam, value)) :: List.remove_assoc id !db ;;

let close (id : id) : unit =
  db := List.remove_assoc id !db ;;

let dump () =
  !db
  |> List.iter (fun (i, (nam, bal)) ->
                  Printf.printf "[%d] %s -> %d\n" i nam bal) ;;
