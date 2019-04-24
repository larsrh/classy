signature CLASSY = sig
  val register_class: binding -> ML_Types.ml_type -> local_theory -> local_theory
  val register_class_cmd: binding -> string -> local_theory -> local_theory

  val register_instance_cmd: xstring * Position.T -> binding -> Input.source -> local_theory -> local_theory

  val check_class: Context.generic -> string * Position.T -> string
  val print_classes: Context.generic -> unit

  val resolve: string -> ML_Types.ml_type -> Context.generic -> string

  val setup: theory -> theory
end

structure Classy: CLASSY = struct

type class_table = (ml_type * (ml_type * string) Name_Space.table) Name_Space.table

exception DUP

structure Classes = Generic_Data
(
  type T = class_table
  val empty = Name_Space.empty_table "ML class"
  val merge = Name_Space.join_tables (fn _ => raise DUP) (* FIXME consistency check *)
  val extend = I
)

fun list_comb (term: string) (args: string list) =
  space_implode " " (enclose "(" ")" term :: map (enclose "(" ")") args)

fun product [] = [[]]
  | product (xs :: xss) =
      maps (fn xs' => map (fn x => x :: xs') xs) (product xss)


fun solve entries problem =
  let
    fun find (typ, term) =
      let
        val (args, candidate) = ML_Types.strip_fun typ
      in
        case ML_Types.match candidate problem [] of
          SOME env =>
            map (ML_Types.subst env) args
            |> map (solve entries)
            |> product
            |> map (list_comb term)
        | NONE => []
      end
  in
    maps find entries
  end

fun resolve class typ context =
  let
    val classes = Classes.get context
    val (constructor, table) = Name_Space.get classes class
    val ML_Types.Con (name, [ML_Types.Var _]) = constructor
    val entries = map snd (Name_Space.extern_table true (Context.proof_of context) table)
    val problem = ML_Types.Con (name, [typ])
    val solutions = solve entries problem
  in
    case solutions of
      [] => error "no solutions"
    | [solution] => enclose "(" ")" solution
    | _ => error "too many solutions"
  end

fun check_class context raw_binding =
  fst (Name_Space.check context (Classes.get context) raw_binding)

val antiquote_setup =
  ML_Antiquotation.inline (Binding.qualify true "ML" @{binding class})
    (Scan.state -- Scan.lift (Parse.position Args.name) >>
      (fn (context, binding) => quote (check_class context binding))) #>
  ML_Antiquotation.inline (Binding.qualify true "ML" @{binding resolve})
    (Scan.state -- Scan.lift (Parse.ML_source --| @{keyword "::"} -- Parse.position Args.name) >>
      (fn (context, (source, binding)) =>
         resolve (check_class context binding) (ML_Types.read_ml_type (input_source_content source)) context))

fun pretty_classes context =
  let
    val table = Classes.get context
    val ctxt = Context.proof_of context
    val space = Name_Space.space_of_table table
    val entries = Name_Space.extern_table true ctxt table

    fun pretty_class ((class, _), (typ, sub_table)) =
      let
        val header = Pretty.block [Name_Space.pretty ctxt space class, Pretty.str ":", Pretty.brk 1, ML_Types.pretty typ]

        val sub_space = Name_Space.space_of_table sub_table
        val sub_entries = Name_Space.extern_table true ctxt sub_table

        fun pretty_instance ((instance, _), (typ, _)) =
          Pretty.item [Name_Space.pretty ctxt sub_space instance, Pretty.str ":", Pretty.brk 1, ML_Types.pretty typ]

        val instances = map pretty_instance sub_entries
      in
        Pretty.item (Pretty.fbreaks (header :: instances))
      end
  in
    Pretty.big_list "Classes" (map pretty_class entries)
  end

val print_classes =
  Pretty.writeln o pretty_classes

fun register_class binding typ =
  let
    val name =
      (case typ of
        ML_Types.Con (name, [ML_Types.Var _]) => name
      | _ => error "Malformed type")
    fun decl _ context =
      let
        val classes = Classes.get context
        val table = Name_Space.empty_table ("ML instances for " ^ name)
        val (_, classes') = Name_Space.define context true (binding, (typ, table)) classes
      in
        Classes.put classes' context
      end
  in
    Local_Theory.declaration {syntax = false, pervasive = false} decl
  end

fun register_class_cmd binding typ =
  register_class binding (ML_Types.read_ml_type typ)

fun register_instance_cmd raw_binding binding source lthy =
  let
    val typ = ML_Types.ml_type_of lthy (input_source_content source)
    (* doesn't have any effect except for markup *)
    val _ = ML_Context.eval_source_in (SOME lthy) ML_Compiler.flags source

    (* FIXME check correct type *)

    fun decl _ context =
      let
        val classes = Classes.get context
        val (key, _) = Name_Space.check context classes raw_binding
        val upd = Name_Space.define context true (binding, (typ, input_source_content source)) #> snd
        val classes' = Name_Space.map_table_entry key (apsnd upd) classes
      in Classes.put classes' context end
  in
    Local_Theory.declaration {syntax = false, pervasive = false} decl lthy
  end

val _ =
  Outer_Syntax.local_theory @{command_keyword "ML.class"} "register new type class"
    (Parse.binding --| @{keyword "="} -- Parse.cartouche
      >> (fn (binding, typ) => register_class_cmd binding typ))

val _ =
  let
    val opt_binding =
      Parse.binding --| @{keyword "="} ||
        Parse.position (Scan.succeed ()) >>
          (fn ((), pos) => Binding.make ("instance" ^ Value.print_int (serial ()), pos))
  in
    Outer_Syntax.local_theory @{command_keyword "ML.instance"} "register new type class instance"
      (opt_binding -- (Parse.ML_source --| @{keyword "::"} -- Parse.position Args.name)
        >> (fn (binding, (source, name)) => register_instance_cmd name binding source))
  end

val _ =
  Outer_Syntax.command @{command_keyword "ML.print_classes"} "print all registered classes"
    (Scan.succeed (Toplevel.keep (print_classes o Toplevel.generic_theory_of)))

val setup = antiquote_setup

end