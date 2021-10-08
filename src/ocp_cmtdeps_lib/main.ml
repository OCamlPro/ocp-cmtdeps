(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Cmt_format
open Cmi_format
open EzCompat

let use_filenames = ref false

let graph = Ez_dot.V1.create "deps" []


type module_ = {
  name : string ;
  mutable filenames : string list ;
  node : Ez_toposort.V1.node ;
  mutable deps : module_ StringMap.t ;
  mutable dot : Ez_dot.V1.node option ;
}

module TOPOSORT = Ez_toposort.V1.MAKE(struct
    type t = module_
    let node t = t.node
    let iter_edges f t =
      StringMap.iter (fun _ t -> f t ) t.deps
    let name t = t.name
  end)

let modules = Hashtbl.create 111

let source_modules = ref []

let find_module modname =
  match Hashtbl.find modules modname with
  | exception Not_found ->
      let m = {
        name = modname ;
        filenames = [] ;
        node = Ez_toposort.V1.new_node () ;
        deps = StringMap.empty ;
        dot = None ;
      } in
      Hashtbl.add modules modname m ;
      m
  | m -> m

let main () =

  let format = ref "pdf" in
  let use_cmis = ref false in
  let all_links = ref false in
  Arg.parse
    [
      "--filenames", Arg.Set use_filenames,
      " Display filenames instead of modnames in deps.pdf";

      "--format", Arg.String (fun s -> format := s),
      "FORMAT Generate deps.FORMAT instead of deps.pdf (using FORMAT encoding)";

      "--cmi", Arg.Set use_cmis, " Use .cmi files";
      "--all", Arg.Set all_links, " Keep all links";
    ]
    (fun s ->
       Printf.eprintf "Error: unexpected argument %S\n%!" s;
       exit 2)
    "ocp-cmtdeps: computes dependencies between compilation units from .cmt files";

  let add_source filename =
    Printf.eprintf "Source: %S\n%!" filename ;
  in
  let f filename =
    Printf.eprintf "Binary: %S\n%!" filename ;
    let cmi, cmt = Cmt_format.read filename in
    begin
      match cmt with
      | Some cmt ->
          let m = find_module cmt.cmt_modname in
          begin
            match cmt.cmt_sourcefile with
            | None -> ()
            | Some filename ->
                match m.dot with
                | Some _ -> ()
                | None ->
                    m.dot <- Some ( Ez_dot.V1.node graph
                                      (if !use_filenames then
                                         filename
                                       else
                                         m.name ) [] );
                    m.filenames <- filename :: m.filenames ;
                    source_modules := m :: !source_modules
          end;
          let add_dep name =
            Printf.eprintf "   add_dep %S\n%!" name;
            if name <> m.name
            && EzString.chop_prefix ~prefix:( name ^ "__" ) m.name = None
            then
              m.deps <- StringMap.add name (find_module name) m.deps
          in
          List.iter (fun (name, _crc_opt) -> add_dep name) cmt.cmt_imports ;
          ()
      | None ->
          match cmi with
          | None -> ()
          | Some cmi ->
              if !use_cmis then
                let m = find_module cmi.cmi_name in
                begin
                  match m.dot with
                  | Some _ -> ()
                  | None ->
                      m.dot <- Some ( Ez_dot.V1.node graph m.name []);
                      source_modules := m :: !source_modules
                end;
                List.iter (fun (name, _crc_opt) ->
                    if name <> m.name
                    && EzString.chop_prefix ~prefix:( name ^ "__" ) m.name = None
                    then
                      m.deps <- StringMap.add name (find_module name) m.deps
                  ) cmi.cmi_crcs ;
                ()

    end
  in
  begin
    let glob = if !use_cmis then "*.cmi" else "*.cmt*" in
    EzFile.iter_dir "." ~f
      ~select: ( EzFile.select ~deep:true ~glob () ) ;
  end;
  EzFile.iter_dir "." ~f:add_source
    ~select: ( EzFile.select ~deep:true ~glob:"*.ml*" ~ignore:"_*" () ) ;

  let ( sorted, cycles, _others ) = TOPOSORT.sort !source_modules in

  List.iter (fun ( m, _incoming, _outgoing ) ->
      Printf.eprintf "Cycle: %s\n%!" m.name;
    ) cycles ;

  if not !all_links then
    List.iter (fun m ->
        StringMap.iter (fun _ m2 ->
            match m2.dot with
            | None -> ()
            | Some _ ->
                StringMap.iter (fun name _ ->
                    if StringMap.mem name m.deps then
                      m.deps <- StringMap.remove name m.deps
                  ) m2.deps
          ) m.deps ;
      ) ( List.rev sorted ) ;

  List.iter (fun m ->
      match m.dot with
      | None -> ()
      | Some dot ->
          Printf.printf "%s\n    deps: " m.name ;
          StringMap.iter (fun _ m2 ->
              match m2.dot with
              | None -> ()
              | Some dot2 ->
                  Ez_dot.V1.add_edge dot dot2 [];
                  Printf.printf "%s " m2.name ;
            ) m.deps ;
          Printf.printf "\n" ;
          List.iter (fun filename ->
              Printf.printf "    file: %s\n%!" filename
            ) m.filenames ;

    ) sorted ;

  Ez_dot.V1.save graph "deps.dot" ;
  let outfile = "deps." ^ !format in
  Ez_dot.V1.dot2file ~dotfile:"deps.dot" ~format:!format ~outfile;
  Printf.eprintf "Generated deps.dot and %S\n%!" outfile;
  ()
