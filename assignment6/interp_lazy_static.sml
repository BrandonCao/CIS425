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
  | RES_REC   of (string * term)
  | RES_CLOSURE of (string * term * env) 
and env = Env of (string * result) list;  

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
fun interp_lazy_static (exp, env) = 

   case exp of
    AST_ERROR s                 => RES_ERROR s
  | AST_NUM  x                  => RES_NUM x
  | AST_BOOL b                  => RES_BOOL b
  | AST_SUCC                    => RES_SUCC
  | AST_PRED                    => RES_PRED
  | AST_ISZERO                  => RES_ISZERO
  | AST_IF (exp1, exp2, exp3)   => (case interp_lazy_static(exp1, env) of
          RES_BOOL(true) => interp_lazy_static(exp2, env)
          | RES_BOOL(false) => interp_lazy_static(exp3, env)
          | _ => RES_ERROR "Error: in if expression result not boolean")
  | AST_APP (exp1, exp2)        => (case interp_lazy_static(exp1, env) of
           RES_SUCC => (case interp_lazy_static(exp2, env) of
                        RES_NUM x => RES_NUM (x+1)
                        |_ => RES_ERROR "Error: Type Error"
          )
          | RES_PRED => (case interp_lazy_static(exp2, env) of
                        RES_NUM x => (case x of
                              0 => RES_NUM 0
                              | _ => RES_NUM (x-1))
                        |_ => RES_ERROR "Error: Type Error"
          )
          | RES_ISZERO => (case interp_lazy_static(exp2, env) of 
                        RES_NUM x => if x = 0 
                            then RES_BOOL true 
                            else RES_BOOL false
                        |_ => RES_ERROR "Error: Type Error"
          )
          | (RES_CLOSURE(var, exp, env_lazy_static), closure_list)     
                                  => let  
                                      val new_lazy_static_env = extend_env(env_lazy_static, var, closure_list) 
                                    in
                                      interp_lazy_static(exp, new_lazy_static_env)
                                    end          
          |_ => RES_ERROR "Error: Type Error")
  		  | AST_ID name                 => interp_lazy_static(lookup_env (env, name))
  		  | AST_FUN  (var, exp)         => RES_CLOSURE (var, exp, env) 
  	  	  | AST_REC  (var, exp)         => RES_ERROR("Error: Type Error")

(*  Once you have defined interp_lazy_static, you can try out simple examples by
      interp_lazy_static (parsestr "succ (succ 7)"), new_env());
    and you can try out larger examples by
      interp_lazy_static (parsefile "your-file-here", new_env());
*)