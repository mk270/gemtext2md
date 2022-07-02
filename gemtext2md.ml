(* gemtext2md, A gemtext to markdown converter, by Martin Keegan

   To the extent (if any) permissible by law, Copyright (C) 2022  Martin Keegan

   This programme is free software; you may redistribute and/or modify it under
   the terms of the Apache Software Licence v2.0. *)

(* Usage: as a commandline filter. The program reads Gemtext on stdin and
   outputs Markdown, hopefully in CommonMark format.

   Compile:

     $ ocamlc -o gemtext2md gemtext2md.ml

   Use:

     $ gemtext2md < myfile.gemini > myfile.md

   or, for the intensely lazy:

     $ ocaml gemtext2md.ml < myfile.gemini > myfile.md                    *)

(* The author is perfectly aware that this code is unidiomatic,
   inefficient, inelegant, undocumented, and depends on loading the
   whole input stream into memory, etc, etc. It is simply not worth
   polishing it. *)

type heading_level = H1 | H2 | H3

type line =
  | PreformattedL of string list
  | ParaL of string
  | LinkL of (string * string option)
  | HeadingL of (heading_level * string)
  | BlankL

exception Malformed_link
exception Malformed_heading

let heading_chars = function
  | H1 -> "#"
  | H2 -> "##"
  | H3 -> "###"

let string_of_link url tag = 
  let string_of_link' = function
    | None     -> ["* [";  url;  "](";  url;  ")"]
    | Some tag -> ["* [";  tag;  "](";  url;  ")"]
  in
    tag |> string_of_link' |> String.concat ""

(* Generate a string in CommonMark format representing the Gemtext line type.
   Sometimes append a newline. *)
let string_of_line = function
  | BlankL           -> ""
  | ParaL s          -> s ^ "\n"
  | LinkL (url, tag) -> string_of_link url tag
  | HeadingL (h, s)  -> [(heading_chars h);  " ";  s;  "\n"] |> String.concat ""
  | PreformattedL ss -> "```\n" ^ (String.concat "\n" ss) ^ "\n```"

let remove_blanks = List.filter (function
  | BlankL -> false
  | _     -> true
)

let link_of_line line =
  match String.split_on_char ' ' line with
  | [ "=>"; ""  ]       -> raise Malformed_link
  | [ "=>"; url ]       -> LinkL (url, None)
  | "=>" :: url :: tag  -> LinkL (url, Some (tag |> String.concat " "))
  | _                   -> raise Malformed_link

let trim s = s (* TODO: should strip leading/trailing whitespace *)

let make_heading s level offset =
  let after_hashes s n = String.sub s n (String.length s - n) in
    HeadingL (level, after_hashes (trim s) offset)

let line_of_string s =
  let line_of_string' = function
    (* links *)
    | ['=';   '>']                     -> raise Malformed_link
    |  '=' :: '>' :: ' ' :: _tl        -> link_of_line s
    |  '=' :: '>' ::  _tl              -> raise Malformed_link

    (* headings *)
    | ['#';   '#';   '#']              -> raise Malformed_heading
    | ['#';   '#';   '#';   ' ']       -> raise Malformed_heading
    |  '#' :: '#' :: '#' :: ' ' :: _tl -> make_heading s H3 4
    |  '#' :: '#' :: '#' :: _tl        -> make_heading s H3 3

    | ['#';   '#';    _]               -> raise Malformed_heading
    | ['#';   '#']                     -> raise Malformed_heading
    |  '#' :: '#' :: ' ' :: _tl        -> make_heading s H2 3
    |  '#' :: '#' :: _tl               -> raise Malformed_heading

    | ['#';   ' ']                     -> raise Malformed_heading
    | ['#']                            -> raise Malformed_heading
    |  '#' :: ' ' :: _tl               -> make_heading s H1 2
    |  '#' :: _tl                      -> raise Malformed_heading

    (* paragraphs / blanks *)
    | []                               -> BlankL
    | _                                -> ParaL (trim s)
  in
    String.to_seq s |> List.of_seq |> line_of_string'

let gather_preformatted lines =
  (* Return a list of tuples representing each line, and whether the
     line occurs within a preformatted block.
     The tuples are returned in reverse order. *)

  let prefix s =
    (* Get the first three characters of input, or return None *)
    try Some (String.sub s 0 3)
    with Invalid_argument _ -> None
  in

  let rec gather_preformatted' (pref : bool) acc = function
    | [] -> acc
    | hd :: tl ->
       (match prefix hd with
         | Some "```" -> gather_preformatted' (not pref) acc tl
         | None
         | Some _ -> (pref, hd) :: gather_preformatted' pref acc tl
       )
  in
    gather_preformatted' false [] lines

let decode_lines lines =
  (* pref_acc is a temporary accumulator to gather preformatted lines
     before the end of the preformatted block *)
  let rec decode_lines' pref_acc acc lines =
    (* if pref_acc is non-empty, then the previous line was preformatted *)
    match pref_acc, lines with
    | [], [] ->
       (* end of file, but not during a preformatted block *)
       acc

    | pls, [] ->
       (* end of file during preformatted block *)
       PreformattedL pls :: acc

    | [], (true, s) :: tl ->
       (* first line of a preformatted block *)
       decode_lines' [s] acc tl

    | pls, (true, s) :: tl ->
       (* in a preformatted block, but not the first line of that block *)
       decode_lines' (s :: pls) acc tl

    | [], (false, s) :: tl ->
       (* a non-preformatted line, after another non-preformatted line *)
       let l = line_of_string s in
         decode_lines' [] (l :: acc) tl

    | pls, (false, s) :: tl ->
       (* first line after a preformatted block *)
       let l = line_of_string s in
         decode_lines' [] (l :: PreformattedL pls :: acc) tl
  in
    decode_lines' [] [] lines

let dump stream =
  List.map string_of_line stream |>
  List.map print_endline

let get_lines () =
  let rec get_lines' acc =
    try let line = read_line () in get_lines' (line :: acc)
    with End_of_file -> acc
  in get_lines' []

let main () =
  () |>
  get_lines |>
  gather_preformatted |>
  decode_lines |>
  remove_blanks |>
  dump |>
  ignore

let () =
  main ()
