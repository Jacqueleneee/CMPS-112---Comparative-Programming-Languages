(* Michael Le | mjle@ucsc.edu
 * 1314870     *)
(* $Id: maindc.ml,v 1.4 2012-02-16 17:47:43-08 - - $ *)

include Scanner
include Bigint

open Bigint
open Printf
open Scanner

type stack_t = Bigint.bigint Stack.t
let push = Stack.push
let pop = Stack.pop
let regs = Hashtbl.create 10

let ord thechar = int_of_char thechar
type binop_t = bigint -> bigint -> bigint

let print_number number = 
    let number' = string_of_bigint number in
    let length  = (String.length number') in
    let rec print_number' index =
        let num_left = length - index in
            if (num_left >= 69) then
                (printf "%s\\\n%!" (String.sub number' index 69);
                 print_number' (index + 69))
            else printf "%s\n%!" (String.sub number' index num_left)
    in print_number' 0

let print_stackempty () = eprintf "ocamldc: stack empty\n%!"

let executereg (thestack: stack_t) (oper: char) (reg: int) =
  try match oper with
    | 'l' -> 
      if (Hashtbl.mem regs reg)
      then push (Hashtbl.find regs reg) thestack
      else printf "ocamldc: register '%c' (0%o) is empty\n%!"
        (char_of_int reg) reg
    | 's' ->
      Hashtbl.replace regs reg (pop thestack)
    | _   -> printf "0%o 0%o is unimplemented\n%!" (ord oper) reg
      with Stack.Empty -> print_stackempty()

let executebinop (thestack: stack_t) (oper: binop_t) =
    try let right = pop thestack
        in  try let left = pop thestack
                in  push (oper left right) thestack
            with Stack.Empty -> (print_stackempty ();
                                 push right thestack)
    with Stack.Empty -> print_stackempty ()

let execute (thestack: stack_t) (oper: char) =
    try match oper with
        | '+'  -> executebinop thestack Bigint.add
        | '-'  -> executebinop thestack Bigint.sub
        | '*'  -> executebinop thestack Bigint.mul
        | '/'  -> executebinop thestack Bigint.div
        | '%'  -> executebinop thestack Bigint.rem
        | '^'  -> executebinop thestack Bigint.pow
        | 'c'  -> Stack.clear thestack
        | 'd'  -> push (Stack.top thestack) thestack
        | 'f'  -> Stack.iter print_number thestack
        | 'l'  -> failwith "operator l scanned with no register"
        | 'p'  -> print_number (Stack.top thestack)
        | 's'  -> failwith "operator s scanned with no register"
        | '\n' -> ()
        | ' '  -> ()
        | _    -> printf "0%o is unimplemented\n%!" (ord oper)
    with Stack.Empty -> print_stackempty()

let toploop (thestack: stack_t) inputchannel =
    let scanbuf = Lexing.from_channel inputchannel in
    let rec toploop () = 
        try  let nexttoken = Scanner.scanner scanbuf
             in  (match nexttoken with
                 | Number number       -> push number thestack
                 | Regoper (oper, reg) -> executereg thestack oper reg
                 | Operator oper       -> execute thestack oper
                 );
             toploop ()
        with End_of_file -> printf "%!";
    in  toploop ()

let readfiles () =
    let thestack : bigint Stack.t = Stack.create ()
    in  ((if Array.length Sys.argv > 1 
         then try  let thefile = open_in Sys.argv.(1)
                   in  toploop thestack thefile
              with Sys_error message -> (
                   printf "%s: %s\n%!" Sys.argv.(0) message;
                   exit 1));
        toploop thestack stdin)

let interact () =
    let thestack : bigint Stack.t = Stack.create ()
    in  toploop thestack stdin

let _ = if not !Sys.interactive then readfiles ()
