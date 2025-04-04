---
layout: post
title: Query compilers
published: false
---

## OCaml Meta programming

My simplest code generator. There is a scarcity of tutorials that explain OCaml PPX and its variants.
It is a pity as I believe there exist some good code generation usecases. 

{% highlight ocaml%}

let function_generator ~ctxt _input _is_private =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let expr = [%expr [1; 2; 3]] in
  let pattern = ppat_var ~loc { txt = "Pattern_test"; loc } in
  let match_expr =
    [%expr
      match [%e expr] with
      | [%p pattern] -> [%e evar ~loc "Pattern_test"]
    ]
  in
  let code =
  [[Ast_builder.Default.value_binding ~loc ~pat:pattern ~expr:match_expr] |> pstr_value ~loc Nonrecursive ]
  in
  List.iter (fun item ->
    Format.printf "Generated Code: %a@." Pprintast.structure_item item
  ) code;
  code

let generator () = Deriving.Generator.V2.make (args ()) function_generator
let _ = Deriving.add "name" ~str_type_decl:(generator ())

{% endhighlight %}
