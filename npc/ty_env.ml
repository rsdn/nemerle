open Ast
open Util

type t = ty Imap.t

let empty = Imap.empty

let global = ref empty

let rec find e tv =
  if Imap.mem tv.tv_id e then
    Some (Imap.find tv.tv_id e)
  else
    if e != !global then
      find !global tv
    else
      None

let add e tv t =
  match find e tv with
  | None -> Imap.add tv.tv_id t e
  | Some _ -> assert false

let expand e tb = 
  let rec f n = function
    | T_var b as t ->
      begin
        match find e (binding b) with
        | Some t -> if n < 10 then f (n + 1) t else t
        | None -> t
      end
    | t -> t
  in f 0 tb

let join e1 e2 =
  let add k v s =
    assert (not (Imap.mem k s));
    Imap.add k v s
  in
  Imap.fold add e1 e2

let join_with_global e =
  global := join !global e
