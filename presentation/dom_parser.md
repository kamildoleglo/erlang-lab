##DOM (Document Object Model) mode

```ruby
2> erlsom:simple_form(Xml).        
{ok,{"{urn:books}books",[],
     [{"book",
       [{"id","bk001"}],
       [{"author",[],["Writer"]},
        {"title",[],["The First Book"]},
        {"genre",[],["Fiction"]},
        {"price",[],["44.95"]},
        {"pub_date",[],["2000-10-01"]},
        {"review",[],["An amazing story of nothing."]}]},
      {"book",
       [{"id","bk002"}],
       [{"author",[],["Poet"]},
        {"title",[],["The Poet's First Poem"]},
        {"genre",[],["Poem"]},
        {"price",[],["24.95"]},
        {"pub_date",[],["1990-10-01"]},
        {"review",[],["Least poetic poems."]}]},
      {"book",
       [{"id","bk003"}],
       [{"author",[],["Poet"]},
        {"title",[],["The Poet's Second Poem"]},
        {"genre",[],["Poem"]},
        {"price",[],["25.95"]},
        {"pub_date",[],["1991-10-01"]},
        {"review",[],["Least poetic poems."]}]},
   ...
    []}

```