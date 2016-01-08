(* OCamlScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)



module I = Lambda.IdentSet

let remove export_idents (rest : Lam_util.group list) : Lam_util.group list  = 
  let ident_free_vars = Hashtbl.create 17 in

  let initial_idents =
    Ext_list.flat_map (fun (x : Lam_util.group) ->
      match x with
      | Single(kind, id,lam) -> (* assert false *)
          begin
            Hashtbl.add ident_free_vars id (Lambda.free_variables lam);
            match kind with
            | Alias | StrictOpt -> []
            | Strict | Variable -> [id]
          end
      | Recursive bindings ->
          begin
            bindings |> Ext_list.flat_map (fun (id,lam) ->
              begin
                Hashtbl.add ident_free_vars id (Lambda.free_variables lam);
                match (lam : Lambda.lambda) with
                | Lfunction _ -> []
                | _ -> [id]
              end)
          end
      | Nop lam ->
          if Lam_util.no_side_effects lam then []
          else 
            (** its free varaibles here will be defined above *)
            I.elements ( Lambda.free_variables lam)) rest  @ export_idents
  in
  let current_ident_sets = 
    Idents_analysis.calculate_used_idents ident_free_vars 
      initial_idents in


  rest |> Ext_list.filter_map (fun ( x : Lam_util.group) ->
    match x with 
    | Single(_,id,_) -> 
        if I.mem id current_ident_sets then 
          Some x else None
    | Nop _ -> Some x 
    | Recursive bindings -> 
        let b = bindings
    |> Ext_list.filter_map (fun  ((id,_) as v) ->
        if I.mem id current_ident_sets then 
          Some v 
        else None
                         )
        in
        match b with 
        | [] -> None 
        | _ -> Some (Recursive b)) 