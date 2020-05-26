(* 
    This is a simple functional style OCAML implementation of
    the Extended Euclidian Algorithm along with a composed modular inverse
    calculator. Ocaml is chosen to display the concise nature of functional
    programming paradigms. Some notable features are the lack of return statements,
    simply the last evaluated value of a function is considered to be its output, and 
    the lack of variables as used in more common languages. We can reference primitives
    using the ref statement, but ultimately we define "x" to be a function
    that outputs some value, even something trivial like an int or string. These
    "variables" are also immutable in the sense that we do not reassign their values
    at any point, although we can modify what they return. 

    Author: Dustin Ray
    TCSS 581
    Spring 2020
*)

print_string "\nOCAML Multiplicative Inverse Calculator v1.0 \n\n";;
(* Here we accept as input a and b. Given 1 as input for a, we return 1. For
 all other a and for b, we define a recursive function eval which accepts 4 inputs
 a b, which are abritrary integers, and x and y, which are resolutions of the bezout identity. *)
let multiplicative_inverse a = function 1 -> 1 
| b -> 
    let rec ex_euclid a b x y = 
        
        (* If a is LTOET 1, then output y, else proceed below *)
        if a <= 1 then y else
        (* Recurse to eval with b, a mod b, y minus (a divided by b) multiplied by x, and x *)
        ex_euclid b (a mod b) (y - (a / b) * x) x

    (*  "in" defines scope of expression. We say that the output of the ex_euclid function
        is evaluated within the scope of what we define x to be with the let statement below. *)
    in
    
    (*  define the output of x to be the output of ex_euclid function given the inputs
        a, b, 0 and 1 *)
    let x = ex_euclid a b 0 1 in
    (* Resolve negative x by adding b, output x otherwise*) 
    if x < 0 then x + b else x;;


print_string "Enter an Integer (a): \n";;
    let a = read_int();;

print_string "Enter an integer (b) to mod a by: \n";;
    let b = read_int();;

print_string "Results as follows:\n\n";;

(*  Given a and b defined above, match the output of modular_inverse with
   m as defined below. *)
match multiplicative_inverse a b with
    (m) -> Printf.printf "Multiplicative inverse is: \n\n%d\n\n" m
    