##Erlsom example

```ruby
8> example1:run().
** exception throw: {'EXIT',
                        {undef,
                            [{erlsom_compile,compile,
                                 [<<"<!-- XSD  for erlsom example1 -->\n<xsd:schema xmlns:xsd=\"http://www.w3.org/2001/"...>>,
                                  []],
                                 []},
                             {erlsom,compile_xsd,2,
                                 [{file,
                                      "/home/kamil/Documents/erlang_lab/pollution_loader/_build/default/lib/erlsom/src/erlsom.erl"},
                                  {line,142}]},
                             {example1,test_erlsom,1,
                                 [{file,"example1.erl"},{line,27}]},
                             {erl_eval,do_apply,6,
                                 [{file,"erl_eval.erl"},{line,674}]},
                             {shell,exprs,7,[{file,"shell.erl"},{line,687}]},
                             {shell,eval_exprs,7,
                                 [{file,"shell.erl"},{line,642}]},
                             {shell,eval_loop,3,
                                 [{file,"shell.erl"},{line,627}]}]}}
     in function  erlsom:compile_xsd/2 (/home/kamil/Documents/erlang_lab/pollution_loader/_build/default/lib/erlsom/src/erlsom.erl, line 144)
     in call from example1:test_erlsom/1 (example1.erl, line 27)
```
