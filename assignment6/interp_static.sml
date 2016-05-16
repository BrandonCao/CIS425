use "parser.sml";



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


exception not_found;



fun new_env() = Env(nil);
fun extend_env (Env(oldenv), id, value) = Env( (id, value):: oldenv);
fun extend_env_all (Env(oldenv), id_value_list) = Env(id_value_list @ oldenv);
fun lookup_env (Env(nil), id) = (print("Free Var!! "^id); raise not_found)
   |lookup_env (Env((id1,value1)::b), id) =  
        if (id1 = id) 
        then value1
	    else lookup_env(Env(b), id) ;


fun interp_static (exp, env) = 

  case exp of
    AST_ERROR s                 => RES_ERROR s
  | AST_NUM  x                  => RES_NUM x 
  | AST_BOOL b                  => RES_BOOL b 
  | AST_SUCC                    => RES_SUCC 
  | AST_PRED                    => RES_PRED 
  | AST_ISZERO                  => RES_ISZERO 
  | AST_FUN (var, exp)          => RES_CLOSURE(var, exp, env)
  | AST_IF (exp1, exp2, exp3)   =>  let 
                    val bool = interp_static (exp1, env)
                    val outcome1 = interp_static (exp2, env)
                    val outcome2 = interp_static(exp3,env)
                  in 
                    if bool = RES_BOOL(true)
                      then outcome1
                    else if bool = RES_BOOL(false)
                      then outcome2
                    else RES_ERROR "boolean error"
                  end
  | AST_ID name                 => lookup_env(env, name)
  | AST_APP (exp1, exp2)        =>  case (interp_static(exp1,env), interp_static(exp2,env)) of
                    (RES_ERROR str, _)      => RES_ERROR str
                    | (RES_SUCC, RES_NUM x)   => RES_NUM(x+1)  
                    | (RES_ISZERO, RES_NUM x)   => if x = 0
                                    then RES_BOOL(true)
                                    else RES_BOOL(false)                    
                    | (RES_PRED, RES_NUM x)   => if x = 0
                                    then RES_NUM 0 
                                    else RES_NUM(x-1)
                    | (RES_CLOSURE(var, exp, env_static), closure_list)     
                                  => let  
                                      val new_static_env = extend_env(env_static, var, closure_list) 
                                    in
                                      interp_static(exp, new_static_env)
                                    end
                    | (RES_FUN(str, exp), list) => let  
                                      val new_static_env = extend_env(env, str, list) 
                                    in
                                      interp(exp, new_static_env)
                                    end                   
                    | (_, _)      => RES_ERROR "Error: not a functional application"
  