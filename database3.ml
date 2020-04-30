(*
                         ATM Account Database
                              Version 3
                         (Mutable Hash Table)
 *)

(* In this third implementation of the `Database` module type, the
concrete data structure is a hash table, using OCaml's `Hashtbl` module,
a module for mutable (side-effecting) dictionaries. Since hash tables
are mutable, we no longer need to store the database as a `ref`.

Hash tables provide constant-time insertion and searching, but at the
cost of using a mutable data structure. Since the accounts database is
required (by `ATMcomponents`) to be mutable anyway, there is little
downside to using a mutable hash table.

Again, swapping in this implementation necessitates no changes to the
interface for the `Database` module, and hence no changes to the
implementation of `ATMcomponents`. *)

(* Customer account identifiers *)
type id = int ;;

(* cDBSIZE -- Initial size of the database hash table *)
let cDBSIZE = 100 ;;
  
(* The account database itself, a hash table mapping ids to pairs of
   name and balance, initially an empty hash table. *)
let db : (int, (string * int)) Hashtbl.t =
  Hashtbl.create cDBSIZE ;;
                                              
(* See .mli file for documentation on individual functions *)

let create (id : id) (name : string) : unit =
  Hashtbl.add db id (name, 0) ;;
   
let find (id : id) : string * int =
  Hashtbl.find db id ;;

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
  let nam = name id in
  Hashtbl.replace db id (nam, value) ;;

let close (id : id) : unit =
  Hashtbl.remove db id ;;

let dump () =
  db
  |> Hashtbl.iter (fun i (nam, bal) ->
                   Printf.printf "[%d] %s -> %d\n" i nam bal) ;;
