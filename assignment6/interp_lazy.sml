(*  Here's a skeleton file to help you get started on Interpreter 1.
 * Original version by Geoffrey Smith - http://users.cs.fiu.edu/~smithg/
 *)

use "parser.sml";


(* Here is a result datatype *)
datatype result =
    RES_ERROR of string
  | RES_NUM   of int
  | RES_BOOL  of bool
  | RES_SUCC
  | RES_PRED
  | RES_ISZERO
  | RES_FUN   of (string * term)
  | RES_CLOSURE of (string * term * env) and env = Env of (string * result) list;  

(* Here is a basic environment implementation *)
exception not_found;


fun new_env() = Env(nil);
fun extend_env (Env(oldenv), id, value) = Env( (id, value):: oldenv);
fun extend_env_all (Env(oldenv), id_value_list) = Env(id_value_list @ oldenv);
fun lookup_env (Env(nil), id) = (print("Free Var!! "^id); raise not_found)
   |lookup_env (Env((id1,value1)::b), id) =  
        if (id1 = id) 
        then value1
      else lookup_env(Env(b), id) ;

(*  Here's a partial skeleton of interp : (term * environment) -> result.
    I've done the first case for you
*)


fun interp_lazy (exp, env) = 

  case exp of
    AST_ERROR s                 => RES_ERROR s
  | AST_NUM  x                  => RES_NUM x 
  | AST_BOOL b                  => RES_BOOL b 
  | AST_SUCC                    => RES_SUCC 
  | AST_PRED                    => RES_PRED 
  | AST_ISZERO                  => RES_ISZERO 
  | AST_ID name                 => lookup_env(env, name)
  | AST_FUN (var, exp)          => RES_FUN(var, exp)
  | AST_IF (exp1, exp2, exp3)   =>  let val bool = interp_lazy (exp1, env)  
                  in 
                    if bool = RES_BOOL(true)
                      then interp_lazy (exp2, env)
                    else if bool = RES_BOOL(false)
                      then interp_lazy (exp3, env)
                    else RES_ERROR "Error: boolean error"
                  end
  | AST_APP (exp1, exp2)        =>  case (exp1, exp2) of
                    (AST_ISZERO, AST_NUM x)   => if x = 0
                                    then RES_BOOL(true)
                                    else RES_BOOL(false)   
                    |(AST_SUCC, AST_NUM x)   => RES_NUM(x+1)  
                    | (AST_PRED, AST_NUM x)   => if x = 0
                                    then RES_NUM 0 
                                    else RES_NUM(x-1)
                    | (AST_FUN(var, exp), exp2)     
                                  => let  
                                      val new_lazy_env = extend_env(env, var, interp_lazy(exp2, env)) 
                                    in
                                      interp_lazy(exp, new_lazy_env)
                                    end
                    | (_, _)      => RES_ERROR "Error: not a functional application"