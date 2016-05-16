use "type.sml";
   
(* A term datatype with typed function arguments to allow type checking *)
type identifier = string;
datatype TE =
    Variable of identifier
  | Literal of int 
  | Plus of TE * TE 
  | Lambda of identifier * typ * TE
  | App of TE * TE 
  | Bool of bool 
  | IsZero of TE
  | IfThenElse of TE * TE * TE ;

exception Unimplemented
exception TypeError

(* 
 *  I have included the code for AST_NUM and AST_IF; you must complete
 *  the other cases!
 *)

(* typeOf : env -> TE -> typ *)
fun typeOf env (Literal x)       = INT
  | typeOf env (Variable x)      = env x
  | typeOf env (Plus(x, y))      = 
        val t1 = typeOf env x
        val t2 = typeOf env y
    in
        if t1 = INT andalso t2 = INT then INT else raise TypeError
    end
    
  | typeOf env (Lambda(i,t,e))   = 
        let val env2 = update env i t
        in
            val t2 = typeOf env2 e
            in ARROW(t,t2)
        end
  | typeOf env (Lett(I,e1,e2)) = let 
        val t1  = typeOf env e1
        val t2  = typeOf env l t1
    in
        typeOf t2 e2
    end
  | typeOf env (App(e1,e2))      = let
        val t1 = typeOf env e1
        val t2 = typeOf env e2
    in case t1 of 
            ARROW(a,b)=>b | _ => raise TypeError
    end
  | typeOf env(Rec(i,t,e)) = let
    val t1 = update env i t
    val t2 = typeOf t1 e
                             in 
                               if t1 = t
                               then t1
                            else raise TypeError
                             end
  | typeOf env (Bool x)          = BOOL
  | typeOf env (IsZero(x))       = let
    val t1 = typeOf env x
                                   in
                                     if t1 = INT
                                     then BOOL
                                    else raise TypeError
                                   end 
  | typeOf env (IfThenElse(e1,e2,e3)) =
        let val t1 = typeOf env e1
            val t2 = typeOf env e2
            val t3 = typeOf env e3
        in
            if (t1 = BOOL) andalso (t2 = t3)
            then t3
            else raise TypeError
       end;

(*
Some sample functions translated into abstract syntax for you to test
your typechecker on:
*)

(* fn f => fn x => f (f x) *)
val test1 = Lambda("f", ARROW(VAR "a", VAR "a"),
                Lambda("x", VAR "a",
                    App(Variable "f",
                        App(Variable "f", Variable "x"))));

(* fn f => fn g => fn x => f (g x) *)
val test2 = Lambda("f", ARROW(VAR "b", VAR "c"),
                Lambda("g", ARROW(VAR "a", VAR "b"),
                    Lambda("x", VAR "a",
                        App(Variable "f",
                            App(Variable "g", Variable "x")))));

(* fn b => if b then 1 else 0 *)
val test3 = Lambda("b", BOOL,
                IfThenElse(Variable "b", Literal 1, Literal 0));



