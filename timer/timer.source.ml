open Pic;;
(* PIC clock speed-up *)
set_bit IRCF1;; set_bit IRCF0;; set_bit PLLEN;; (* PIC/LCD display connection configuration *)
module Disp = Lcd.Connect (
struct
  let bus_size = Lcd.Four
  let e  = LATD0
  let rs = LATD2
  let rw = LATD1
  let bus = PORTC
end
);;

(* Display initialization and configuration *)
Disp.init ();;
Disp.config ();;
(* Counter (reference to an Int32) *)
let counter = ref 0l;;
(* Display utility *)
let print_02d n =
  if n < 10 then Disp.print_char '0';
  Disp.print_int n;
;;

(* Displays counter’s value *)

let print () =
  let t = !counter in
  let t60 = Int32.div t 60l in
  let s = Int32.to_int (Int32.rem t 60l) in
  let m = Int32.to_int (Int32.rem t60 60l) in
  let h = Int32.to_int (Int32.div t60 60l) in
    Disp.moveto 0 0;
    print_02d h;
    Disp.print_char ':';
    print_02d m;
    Disp.print_char ':';
    print_02d s;
;;

(* Overflow utilities *)

let is_null () = !counter <= 0l and is_full () = !counter >= 359999l;;

(* Increments the counter by n *)

let incr n =
  counter := Int32.add !counter (Int32.of_int n);
  if is_full () then counter := 359999l;
;;

(* Decrements the counter by n *)

let decr n =
  counter := Int32.sub !counter (Int32.of_int n);
  if is_null () then counter := 0l;
;;

(* Increments/Decrements the counter by 1 *)

let decr1 () =
  if not (is_null ()) then counter := Int32.pred !counter;;
let incr1 () =
  if not (is_full ()) then counter := Int32.succ !counter;;

(* Speed-up functions for time setting *)

let step n = n / 32 * n / 16 * n + n / 16 * n / 16 + n / 8 + 1;;

(* Small states machine *)

let rec run n =
  if is_null () || test_bit RB0 then stop ()
  else if test_bit RB1 then ( incr_start () ; run 0 )
  else if test_bit RB2 then ( decr_start () ; run 0 )
  else if n = 79 then ( Sys.sleep 7 ; decr1 () ; print () ; run 0 )
  else ( Sys.sleep 10 ; run (succ n) )
and pause () =
  Sys.sleep 10;
  if test_bit RB0 && not (is_null ()) then start ()
  else if test_bit RB1 then ( incr_start () ; pause () )
  else if test_bit RB2 then ( decr_start () ; pause () )
  else pause ();
and start () =
  set_bit RB3;
  Sys.sleep 10;
  if test_bit RB0 then start ()
  else run 0;
and stop () =
  clear_bit RB3;
  Sys.sleep 10;
  if test_bit RB0 then stop ()
  else pause ();
and incr_start () =
  incr1 ();
  print ();
  Sys.sleep 300;
  incr_loop 1;
and decr_start () =
  decr1 ();
  print ();
  Sys.sleep 300;
  decr_loop 1;
and incr_loop n =
  if test_bit RB1 then (
    incr (step n);
    print ();
    Sys.sleep 100;
    if not (is_full ()) then incr_loop (succ n);
  )
and decr_loop n =
  if test_bit RB2 then (
    decr (step n);
    print ();
    Sys.sleep 100;
    if not (is_null ()) then decr_loop (succ n);
  )
in
  write_reg TRISB 0b00000111; (* Configure button pins as input *)
  print ();                   (* Display 00:00:00 *)
  stop ();                    (* Entry point of the state machine *)

