open Pic;;
module Disp = Lcd.Connect (
struct (* LCD/PIC connection configuration: *)
  let bus_size = Lcd.Four         (* Bus size *)
  let e  = LATD0                  (* Validation pin: D0 *)
  let rs = LATD2                  (* Instruction/data pin: D2 *)
  let rw = LATD1                  (* Read/write pin: D1 *)
  let bus = PORTB                 (* Bus: port B *)
end
);;

Disp.init ();;                    (* Display initialization *)
Disp.config ();;                  (* Display configuration *)
Disp.print_string "Hello world";; (* Display a string *)
