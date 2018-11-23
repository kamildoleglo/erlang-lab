##SAX (Simple API for XML) mode

```ruby
1> erlsom:parse_sax(Xml, [], fun(Event, Acc) -> io:format("~p~n", [Event]), Acc end).
startDocument
{processingInstruction,"xml"," version=\"1.0\""}
{startPrefixMapping,"x","urn:books"}
{startElement,"urn:books","books","x",[]}
{ignorableWhitespace,"\n    "}
{startElement,[],"book",[],[{attribute,"id",[],[],"bk001"}]}
{ignorableWhitespace,"\n        "}
{startElement,[],"author",[],[]}
{characters,"Writer"}
{endElement,[],"author",[]}
{ignorableWhitespace,"\n        "}
{startElement,[],"title",[],[]}
{characters,"The First Book"}
{endElement,[],"title",[]}
{ignorableWhitespace,"\n        "}
{startElement,[],"genre",[],[]}
{characters,"Fiction"}
{endElement,[],"genre",[]}
{ignorableWhitespace,"\n        "}
{startElement,[],"price",[],[]}
{characters,"44.95"}
{endElement,[],"price",[]}
{ignorableWhitespace,"\n        "}
{startElement,[],"pub_date",[],[]}
{characters,"2000-10-01"}
...
{endElement,"urn:books","books","x"}
{endPrefixMapping,"x"}
endDocument
{ok,[],[]}

```