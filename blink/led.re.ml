open Pic; /* Module containing write_reg, set_bit, RB0, ... */

{
  write_reg TRISB 0; /* Configure the B port as output */
  while true {
    set_bit RB0; /* Turn on the LED */
    Sys.sleep 500; /* Wait 0,5s */
    clear_bit RB0; /* Turn off the LED */
    Sys.sleep 500 /* Wait 0,5s */
  }
};
