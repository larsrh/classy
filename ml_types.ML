signature ML_TYPES = sig
  datatype ml_type =
    Var of string |
    Con of string * ml_type list |
    Tuple of ml_type list |
    Fun of ml_type * ml_type |
    Record of (string * ml_type) list

  val unit: ml_type
  val canonicalize: ml_type -> ml_type

  val read_ml_type: string -> ml_type
  val ml_type_of: Proof.context -> string -> ml_type

  val pretty: ml_type -> Pretty.T

  type env = (string * ml_type) list
  val subst: env -> ml_type -> ml_type
  val match: ml_type -> ml_type -> env -> env option

  val strip_fun: ml_type -> ml_type list * ml_type
end

structure ML_Types: ML_TYPES = struct

datatype ml_type =
  Var of string |
  Con of string * ml_type list |
  Tuple of ml_type list |
  Fun of ml_type * ml_type |
  Record of (string * ml_type) list

val unit = Con ("unit", [])

type env = (string * ml_type) list

fun subst env (Con (name, ts)) = Con (name, map (subst env) ts)
  | subst env (Var name) = the_default (Var name) (AList.lookup op = env name)
  | subst env (Tuple ts) = Tuple (map (subst env) ts)
  | subst env (Fun (x, y)) = Fun (subst env x, subst env y)
  | subst env (Record fs) = Record (map (apsnd (subst env)) fs)

fun match (Var name) t env =
      (case AList.lookup op = env name of
        NONE => SOME ((name, t) :: env)
      | SOME t' => if t = t' then SOME env else NONE)
  | match (Fun (x1, y1)) (Fun (x2, y2)) env =
      Option.mapPartial (match y1 y2) (match x1 x2 env)
  | match (Con (name1, ts1)) (Con (name2, ts2)) env =
      if name1 = name2 then match_list ts1 ts2 env else NONE
  | match (Tuple ts1) (Tuple ts2) env = match_list ts1 ts2 env
  | match (Record fs1) (Record fs2) env =
      if map fst fs1 = map fst fs2 then
        match_list (map snd fs1) (map snd fs2) env
      else
        NONE
  | match _ _ _ = NONE
and match_list [] [] env = SOME env
  | match_list (t :: ts) (u :: us) env =Option.mapPartial (match t u) (match_list ts us env)
  | match_list _ _ _ = NONE

fun strip_fun (Fun (x, y)) = let val (args, res) = strip_fun y in (x :: args, res) end
  | strip_fun t = ([], t)

fun canonicalize (Var name) = Var name
  | canonicalize (Con (name, ts)) = Con (name, map canonicalize ts)
  | canonicalize (Tuple []) = unit
  | canonicalize (Tuple [t]) = canonicalize t
  | canonicalize (Tuple ts) = Tuple (map canonicalize ts)
  | canonicalize (Fun (t, u)) = Fun (canonicalize t, canonicalize u)
  | canonicalize (Record []) = unit
  | canonicalize (Record es) = Record (sort_wrt fst (map (apsnd canonicalize) es))

val pretty =
  let
    datatype shape = SVar | SCon | STuple | SFun_Left | SRoot

    fun prec SVar _ = error "impossible"
      | prec SRoot _ = true
      | prec _ (Var _) = true
      | prec _ (Record _) = true
      | prec STuple (Con _) = true
      | prec STuple _ = false
      | prec SFun_Left (Con _) = true
      | prec SFun_Left (Tuple _) = true
      | prec SFun_Left (Fun _) = false
      | prec SCon (Con _) = true
      | prec SCon _ = false

    fun par n t p = if prec n t then p else Pretty.block [Pretty.str "(", p, Pretty.str ")"]

    fun aux s t =
      (case t of
        Var id => [Pretty.str id]
      | Tuple ts => Pretty.separate " *" (map (aux STuple) ts)
      | Fun (arg, res) => 
          [aux SFun_Left arg, Pretty.brk 1, Pretty.str "->", Pretty.brk 1, aux SRoot res]
      | Con (id, [t]) =>
          [aux SCon t, Pretty.brk 1, Pretty.str id]
      | Con (id, []) =>
          [Pretty.str id]
      | Con (id, ts) =>
          [Pretty.block (Pretty.str "(" :: Pretty.separate "," (map (aux SRoot) ts) @ [Pretty.str ")"]), Pretty.brk 1, Pretty.str id]
      | Record es =>
          Pretty.str "{" :: Pretty.separate "," (map (fn (name, t) => Pretty.block [Pretty.str name, Pretty.str ":", Pretty.brk 1, aux SRoot t]) es) @ [Pretty.str "}"])
      |> Pretty.block |> par s t
  in
    aux SRoot o canonicalize
  end

type token = ML_Lex.token_kind * string

type 'a parser = token list -> 'a * token list

fun kind k: string parser = Scan.one (equal k o fst) >> snd
fun tok t: unit parser = Scan.one (equal t) >> K ()
fun keyword k: unit parser = tok (ML_Lex.Keyword, k)
val open_parenthesis: unit parser = keyword "("
val closed_parenthesis: unit parser = keyword ")"
val open_brace: unit parser = keyword "{"
val closed_brace: unit parser = keyword "}"
val colon: unit parser = keyword ":"
val comma: unit parser = keyword ","
val arrow: unit parser = keyword "->"
val asterisk: unit parser = tok (ML_Lex.Ident, "*")

val ident: string parser =
  kind ML_Lex.Long_Ident ||
  Scan.one (fn (k, c) => k = ML_Lex.Ident andalso c <> "*") >> snd

fun intersep p sep =
  (p ::: Scan.repeat (sep |-- p)) || Scan.succeed []

val typ =
  let
    (* code partly lifted from Spec_Check *)
    fun make_con [] = raise Empty
      | make_con [c] = c
      | make_con (Con (s, _) :: cl) = Con (s, [make_con cl]);

    fun typ s = (func || tuple || typ_single) s
    and typ_arg s = (tuple || typ_single) s
    and typ_single s = (con || typ_basic) s
    and typ_basic s =
      (var
      || open_brace |-- record --| closed_brace
      || open_parenthesis |-- typ --| closed_parenthesis) s
    and list s = (open_parenthesis |-- typ -- Scan.repeat1 (comma |-- typ) --| closed_parenthesis >> op ::) s
    and var s = (kind ML_Lex.Type_Var >> Var) s
    and con s = ((con_nest
      || typ_basic -- con_nest >> (fn (b, Con (t, _) :: tl) => Con (t, [b]) :: tl)
      || list -- con_nest >> (fn (l, Con (t, _) :: tl) => Con (t, l) :: tl))
      >> (make_con o rev)) s
    and con_nest s = Scan.unless var (Scan.repeat1 (ident >> (Con o rpair []))) s
    and func s = (typ_arg --| arrow -- typ >> Fun) s
    and record s = (intersep (kind ML_Lex.Ident -- (colon |-- typ)) comma >> Record) s
    and tuple s = (typ_single -- Scan.repeat1 (asterisk |-- typ_single)
      >> (fn (t, tl) => Tuple (t :: tl))) s
  in typ end

fun read_binding s =
  let
    val colon = (ML_Lex.Keyword, ":")
    val semicolon = (ML_Lex.Keyword, ";")
    fun unpack_tok tok = (ML_Lex.kind_of tok, ML_Lex.content_of tok)
    val toks = filter (not o equal ML_Lex.Space o fst) (map unpack_tok (ML_Lex.tokenize s))
    val junk = (Scan.many (not o equal colon) -- tok colon) >> K ()
    val stopper = Scan.stopper (K semicolon) (equal semicolon)
    val all = junk |-- Scan.finite stopper typ
    val (typ, rest) = all toks
  in
    if null rest then
      typ
    else
      error "Could not fully parse type"
  end

fun read_ml_type s =
  let
    (* FIXME deduplicate *)
    val semicolon = (ML_Lex.Keyword, ";")
    fun unpack_tok tok = (ML_Lex.kind_of tok, ML_Lex.content_of tok)
    val toks = filter (not o equal ML_Lex.Space o fst) (map unpack_tok (ML_Lex.tokenize s))
    val stopper = Scan.stopper (K semicolon) (equal semicolon)
    val all = Scan.finite stopper typ
    val (typ, rest) = all toks
  in
    if null rest then
      typ
    else
      error "Could not fully parse type"
  end

fun ml_type_of ctxt s =
  let
    (* code partly lifted from Spec_Check *)
    val return = Unsynchronized.ref NONE
    val s = "(fn () => (" ^ s ^ "))"
    val use_context: use_context =
     {tune_source = #tune_source ML_Env.local_context,
      name_space = #name_space ML_Env.local_context,
      str_of_pos = #str_of_pos ML_Env.local_context,
      print = fn r => return := SOME r,
      error = #error ML_Env.local_context}
    val _ =
      Context.setmp_thread_data (SOME (Context.Proof ctxt))
        (fn () => Secure.use_text use_context (0, "generated code") true s) ()
    val (Fun (Con ("unit", []), typ)) = read_binding (the (! return))
  in
    typ
  end

end

type ml_type = ML_Types.ml_type
