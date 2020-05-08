module String = struct
  include String

  let implode xs =
    let buf = Buffer.create (List.length xs) in
    List.iter (Buffer.add_char buf) xs;
    Buffer.contents buf
  ;;

  let fold_left f init str =
    let len = length str in
    let rec loop i accum = if i = len then accum else loop (i + 1) (f accum str.[i]) in
    loop 0 init
  ;;
end

module List = struct
  include List

  (* From Base
     https://github.com/janestreet/base/blob/f86e72ee3b59ff5315e20a8392b81fb2f5237a25/src/ppx_compare_lib.ml

     The MIT License

     Copyright (c) 2016--2020 Jane Street Group, LLC <opensource@janestreet.com>

     Permission is hereby granted, free of charge, to any person obtaining a copy
     of this software and associated documentation files (the "Software"), to deal
     in the Software without restriction, including without limitation the rights
     to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
     copies of the Software, and to permit persons to whom the Software is
     furnished to do so, subject to the following conditions:

     The above copyright notice and this permission notice shall be included in all
     copies or substantial portions of the Software.

     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
     IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
     FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
     AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
     LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
     OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
     SOFTWARE.
  *)
  let rec compare compare_elt a b =
    match a, b with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x :: xs, y :: ys ->
      let res = compare_elt x y in
      if res <> 0 then res else compare compare_elt xs ys
  ;;

  (* The default List.map isn't tail recursive so we replace it with one that is *)
  let map f xs = List.rev_map f xs |> List.rev
end

let compare_bool = Bool.compare
let compare_float = Float.compare
let compare_int = Int.compare
let compare_int32 = Int32.compare
let compare_list = List.compare
let compare_option = Option.compare
let compare_string = String.compare
