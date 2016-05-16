(*
Here is some sample input/output for your interpreter:

In a call-by-value statically-scoped interpreter:

interp(parsestr "1", new_env());
val it = RES_NUM 1 : result

interp(parsestr("succ 1"), new_env());
val it = RES_NUM 2 : result

interp(parsestr("let x = 2 in succ x end"), new_env());
val it = RES_NUM 3 : result

interp(parsestr("let f = (fn x => succ x) in (f 4) end"), new_env());
val it = RES_NUM 5 : result

interp_static(parsestr "(fn x => ((fn y => succ x) 0)) 7", new_env());
val it = RES_NUM 8 : result

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In a dynamically-scoped interpreter:

interp(parsestr ("let x = 21 in let foo = fn y => x in let x = 13 in foo 0 end end end"), new_env());
val it = RES_NUM 13 : result

But in a statically-scoped interpreter:

interp(parsestr ("let x = 21 in let foo = fn y => x in let x = 13 in foo 0 end end end"), new_env());
val it = RES_NUM 21 : result

Do you see why?

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In any call-by-value interpreter:

interp(parsestr ("(fn x => 34) (iszero true)"), new_env());
val it = RES_ERROR "some complaint about applying true to iszero..." : result

But in a call-by-name, or call-by-need interpreter:

interp(parsestr ("(fn x => 34) (iszero true)"), new_env());
val it = RES_NUM 34 : result

Do you see why?

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)
