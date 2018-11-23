## 'data-binder' mode

```ruby
3> {ok, Model} = erlsom:compile_xsd_file("books.xsd").
{ok,{model,[{type…
```
```ruby
4> {ok, Result, _} = erlsom:scan(Xml, Model).
{ok,{'BooksForm',[],
              [{'BookForm',[],"bk001","Writer","The First Book","Fiction",
                           "44.95","2000-10-01","An amazing story of nothing."},
               {'BookForm',[],"bk002","Poet","The Poet's First Poem",
                           "Poem","24.95","1990-10-01","Least poetic poems."},
               {'BookForm',[],"bk003","Poet","The Poet's Second Poem",
                           "Poem","25.95","1991-10-01","Least poetic poems."},
               {'BookForm',[],"bk004","Poet","The Poet's Third Poem",
                           "Poem","14.95","1994-10-01","Least poetic poems."},
               {'BookForm',[],"bk005","Poet","The Poet's Fourth Poem",
                           "Poem","34.95","1995-10-01","Least poetic poems."},
               {'BookForm',[],"bk006","Poet","The Poet's Fifth Poem",
                           "Poem","64.95","1999-10-01","Least poetic poems."}]},
,"\r\n"}
```
```css
Books = Result#BooksForm.book,
[Book1 | _] = Books,
Book1_title = Book1#BookForm.title.
```