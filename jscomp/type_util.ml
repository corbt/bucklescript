(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)








(* Given a module type path, find its signatures, used in js FFI
    when calling javascript functions, assume it's a path type
*)

(* exception Path_not_found  *)

let rec query (v : Path.t) (env : Env.t) : Types.signature option =
  match Env.find_modtype_expansion v env with 
  | Mty_alias v1 
  | Mty_ident  v1 -> query v1 env 
  | Mty_functor  _ -> assert false  (* not handled in FFI Yet*)
  | Mty_signature s ->  Some s 
  | exception _ -> None 


let name_of_signature_item (x : Types.signature_item )=
  match x with 
  | Sig_value (i,_) 
  | Sig_module (i,_,_) -> i 
  | Sig_typext (i,_,_) -> i 
  | Sig_modtype(i,_) -> i 
  | Sig_class (i,_,_) -> i 
  | Sig_class_type(i,_,_) -> i 
  | Sig_type(i,_,_) -> i  

(** Used in [Pgetglobal] *)
let get_name  (serializable_sigs : Types.signature) (pos : int) = 
  let ident = 
    name_of_signature_item @@ List.nth  serializable_sigs  pos
  in ident.name

let string_of_value_description id = 
  Format.asprintf "%a" (Printtyp.value_description id)

(** It should be safe to replace Pervasives[], 
    we should test cases  like module Pervasives = List *)

let filter_serializable_signatures (signature : Types.signature)
  : Types.signature = 
  List.filter (fun x ->
      match (x : Types.signature_item) with 
      | Sig_value(_, {val_kind = Val_prim _}) -> false
      | Sig_typext _ 
      | Sig_module _
      | Sig_class _ 
      | Sig_value _ -> true
      | _ -> false) signature

(* Input path is a global module 
    TODO: it should be fine for local module
*)
let find_serializable_signatures_by_path (v : Path.t) (env : Env.t) 
  : Types.signature option = 
  match Env.find_module v env with 
  | exception Not_found -> None 
  | {md_type = Mty_signature signature; _} -> 
    Some (filter_serializable_signatures signature)
  (** TODO: refine *)
  | _ -> Ext_log.err __LOC__  "@[impossible path %s@]@." (Path.name v) ; assert false 

let rec dump_summary fmt (x : Env.summary) = 
  match x with 
  | Env_empty -> ()
  | Env_value(s,id,value_description) -> 
    dump_summary fmt s ;
    Printtyp.value_description id fmt value_description
  | _ -> ()

let query_type (id : Ident.t) (env : Env.t) = 
  (* (Js_output.string_of_fmt dump_summary ) (Env.summary env) *)
  match Env.find_value (Pident id) env with
  | exception Not_found -> ""
  | { val_type ; }  ->
    (* string_of_value_description id *) 
    Format.asprintf "%a"
      !Oprint.out_type 
      (Printtyp.tree_of_type_scheme val_type)

