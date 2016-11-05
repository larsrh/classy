theory Test
imports Classy
begin

section \<open>Parsing and printing types\<close>

ML\<open>
fun check t =
  let
    val t' = ML_Types.read_ml_type t |> ML_Types.pretty |> Pretty.string_of |> YXML.content_of
  in
    if t <> t' then
      error t'
    else
      ()
  end
\<close>

ML\<open>check "'a"\<close>
ML\<open>check "'a * 'b"\<close>
ML\<open>check "'a * 'b * 'c"\<close>
ML\<open>check "('a * 'b) * 'c"\<close>
ML\<open>check "'a * ('b * 'c)"\<close>
ML\<open>check "'a * 'b -> 'c"\<close>
ML\<open>check "'a * 'b -> 'a * 'b"\<close>
ML\<open>check "('a -> 'b) * ('a -> 'b)"\<close>
ML\<open>check "('a, 'b) foo"\<close>
ML\<open>check "'a foo"\<close>
ML\<open>check "unit foo"\<close>
ML\<open>check "unit * 'a"\<close>
ML\<open>check "'a foo * 'b foo bar"\<close>
ML\<open>check "('a, unit) foo"\<close>
ML\<open>check "('a * 'b) foo"\<close>
ML\<open>check "('a -> 'b) foo"\<close>
ML\<open>check "('a -> 'b, 'c) foo"\<close>
ML\<open>check "('a -> 'b, 'c * 'd) foo"\<close>
ML\<open>check "'a foo bar baz"\<close>
ML\<open>check "('a foo, 'b) bar Long.ident.baz"\<close>
ML\<open>check "{a: int}"\<close>
ML\<open>check "{a: int, b: float}"\<close>
ML\<open>check "{a: int, b: 'a -> 'b}"\<close>

section \<open>Instance resolution\<close>

ML\<open>@{ML.resolve \<open>string\<close> :: pretty}\<close>
ML\<open>@{ML.resolve \<open>string list\<close> :: pretty}\<close>
ML\<open>@{ML.resolve \<open>string list list\<close> :: pretty}\<close>
ML\<open>@{ML.resolve \<open>Pretty.T\<close> :: pretty}\<close>

end