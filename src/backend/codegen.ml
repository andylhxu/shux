open Core
open Llvm
open Exceptions
(*open Ast
open Sast*)
(* Hashtbl... *)

let context = global_context ()
let the_module = create_module context "shux codegen"
let builder = builder context
(* define any intermediate data structures here, such as hash tables *)

(* TODO: check in the future, we might not need all of these *)
let i32_t = i32_type context;;
let i8_t = i8_type context;;
let f_t = double_type context;;
let i1_t = i1_type context;;
let str_t = pointer_type i8_t;;
let i64_t = i64_type context;;
let void_t = void_type context;;

let debug = fun s ->  
    print_endline ("`````````````````````````````````````"^s);
    dump_module the_module;
    print_endline ("`````````````````````````````````````"^s);
    ()

(* Use as template for our print function builder *)
(*
and codegen_print el llbuilder = 
    let printf = func_lookup "printf" in
    let tmp_count = ref 0 in
    let incr_tmp = fun x -> incr tmp_count in

    let map_expr_to_printfexpr expr = 
        let exprType = Analyzer.get_type_from_sexpr expr in
        match exprType with 
        Datatype(Bool_t) ->
            incr_tmp ();
            let tmp_var = "tmp" ^ (string_of_int !tmp_count) in
            let trueStr = SString_Lit("true") in
            let falseStr = SString_Lit("false") in
            let id = SId(tmp_var, str_type) in 
            ignore(codegen_stmt llbuilder (SLocal(str_type, tmp_var, SNoexpr)));
            ignore(codegen_stmt llbuilder (SIf(expr, 
                                            SExpr(SAssign(id, trueStr, str_type), str_type), 
                                            SExpr(SAssign(id, falseStr, str_type), str_type)
                                        )));
            codegen_sexpr llbuilder id
        | _ -> codegen_sexpr llbuilder expr
    in

    let params = List.map map_expr_to_printfexpr el in
    let param_types = List.map (Analyzer.get_type_from_sexpr) el in 

    let map_param_to_string = function 
            Arraytype(Char_t, 1)    -> "%s"
        |   Datatype(Int_t)         -> "%d"
        |   Datatype(Float_t)       -> "%f"
        |   Datatype(Bool_t)        -> "%s"
        |   Datatype(Char_t)        -> "%c"
        |   _                       -> raise (Exceptions.InvalidTypePassedToPrintf)
    in 
    let const_str = List.fold_left (fun s t -> s ^ map_param_to_string t) "" param_types in
    let s = codegen_sexpr llbuilder (SString_Lit(const_str)) in
    let zero = const_int i32_t 0 in 
    let s = build_in_bounds_gep s [| zero |] "tmp" llbuilder in
    build_call printf (Array.of_list (s :: params)) "tmp" llbuilder
*)

let codegen_library_functions () = 
    (* TODO: modify these... C Std lib functions *)
    let printf_ty = var_arg_function_type i32_t [| pointer_type i8_t |] in
    let _ = declare_function "printf" printf_ty the_module in

let codegen_main main = 
    (* clear hashtables *)
    let fty = function_type i32_t [||] in
    let f = define_function "main" fty the_module in
    let llbuilder = builder_at_end context (entry_block f) in

    (* let _ = codegen_stmt llbuilder (SBlock (main.sbody)) in *)

    build_ret (const_int i32_t 0) llbuilder

let linker filename = 
    let llctx = Llvm.global_context () in
    (*
    let llmem = Llvm.MemoryBuffer.of_file filename in
    *)
    let llm = Llvm_bitreader.parse_bitcode llctx in (*llmem in*)
    ignore(Llvm_linker.link_modules the_module llm)

let translate ns globals functions =
    (* let main = find_exn functions TODO *)
    let _ = codegen_library_functions () in
    let _ = codegen_main main in
    let _ = linker "bindings.bc" in
    the_module
