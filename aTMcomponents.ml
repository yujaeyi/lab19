(*
                Component Behaviors of an ATM Machine
 *)
(*
                       REMARKS ON THE SOLUTION

This implementation of the `ATMcomponents` interface itself splits off
all dealings with the database of accounts by referencing a separate
`Database` module. That module's interface can be found in
`database.mli`, and provides lower-level manipulations of the database
independent of any particular concrete OCaml data structure
implementing the database. To demonstrate the
implementation-independence made possible by the abstraction barrier
from the module's signature, we provide three different
implementations of the database module in `database.ml`,
`database2.ml`, and `database3.ml`. (See those files for further
information.)

Because the signature for `ATMcomponents` returns a unit for many
functions that implicitly modify the database (`initialize` and
`update` in particular), the implementation of the database from
within `ATMcomponents` must allow for modification by side effect. A
purely functional implementation is not possible. We pass that
requirement on to the `Database` module as well. Its interface also is
imperative rather than functional. An alternative would be to have a
functional database module and do all of the storing and side effects
within `ATMcomponents`.

Another reasonable alternative design would be to encapsulate all of
the database manipulation within one or more classes, for instance, an
`account` class or a `database` class or both. *)

open Printf ;;
open Scanf ;;
  
(* Make use of an account database *)
module DB = Database3 ;; 

(* Customer account identifiers *)
type id = int ;;

(* Possible actions that an ATM customer can perform *)
type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;; 

(* A specification of a customer name and initial balance *)
type account_spec = {name : string; id : id; balance : int} ;;

(*....................................................................
  ATM component behaviors. See .mli file for further documentation.
 *)

let initialize (initial : account_spec list) : unit =
  initial
  |> List.iter (fun {name; id; balance}
                -> DB.create id name;
                   DB.update id balance) ;;

let rec acquire_id () : id =
  printf "Enter customer id: "; 
  try
    let id = read_int () in
    ignore (DB.exists id); id
  with
  | Not_found 
  | Failure _ -> printf "Invalid id \n";
                 acquire_id () ;;
                  
let rec acquire_amount () : int =
  printf "Enter amount: ";
  try
    let amount = read_int () in
    if amount <= 0 then raise (Failure "amount is non-positive");
    amount
  with
  | Failure _ -> printf "Invalid amount \n";
                 acquire_amount () ;;
  
let rec acquire_act () : action =
  printf "Enter action: (B) Balance (-) Withdraw (+) Deposit \
          (=) Done (X) Exit: %!";
  scanf " %c"
        (fun char -> match char with
                     | 'b' | 'B'        -> Balance
                     | '/' | 'x' | 'X'  -> Finished
                     | '='              -> Next
                     | 'w' | 'W' | '-'  -> Withdraw (acquire_amount ())
                     | 'd' | 'D' | '+'  -> Deposit (acquire_amount ())
                     | _                -> printf "  invalid choice\n";
                                           acquire_act () ) ;;

let get_balance : id -> int = DB.balance ;;

let get_name : id -> string = DB.name ;;

let update_balance : id -> int -> unit = DB.update ;;

let present_message (msg : string) : unit = 
  printf "%s\n%!" msg ;;
  
let deliver_cash (amount : int) : unit =
  printf "Here's your cash: ";
  (* dispense some "20's" *)
  for _i = 1 to (amount / 20) do
    printf "[20 @ 20]"
  done;
  (* dispense the rest of the cash *)
  printf " and %d more\n" (amount mod 20) ;;

