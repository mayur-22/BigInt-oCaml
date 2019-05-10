# BigInt-oCaml
This repository contains ocaml file for basic calculations of big integers

This repository contains an ocmal file which can perform basic calculations on big integers.
Input is taken in form of a string of integers with/without sign.

bigint here is of the format (sign,int list)
where sign can be Neg/NonNeg.
Neg for negative numbers and NonNeg for positive numbers

Other than helper functions it contains are:
```oCaml
type sign = Neg | NonNeg
type bigint = sign * int list

(*converts the string into bigint*)
val mk_big : string -> sign * int list = <fun>

(*returns back the string from bigint*)
val give_num : sign * int list -> string = <fun>

(*unary operations on bigint*)
val abs : 'a * 'b -> sign * 'b = <fun>
val minus : sign * int list -> sign * int list = <fun>

(*comparison operations on bigint*)
val gte : 'a list -> 'a list -> bool = <fun>
val geq : sign * 'a list -> sign * 'a list -> bool = <fun>
val gt : sign * 'a list -> sign * 'a list -> bool = <fun>
val lte : 'a list -> 'a list -> bool = <fun>
val leq : sign * 'a list -> sign * 'a list -> bool = <fun>
val eq : 'a * 'b list -> 'a * 'b list -> bool = <fun>

(*binary operations on bigint*)
val add : sign * int list -> sign * int list -> sign * int list = <fun>
val sub : sign * int list -> sign * int list -> sign * int list = <fun>
val mult : 'a * int list -> 'a * int list -> sign * int list = <fun>
val div : 'a * int list -> 'a * int list -> sign * int list = <fun>
val rem : 'a * int list -> 'a * int list -> sign * int list = <fun>







```
