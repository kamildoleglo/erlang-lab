%%%-------------------------------------------------------------------
%%% @author fudalinm
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jun 2018 13:11
%%%-------------------------------------------------------------------
-module(es).
-author("fudalinm").
-include("books.hrl").

%% API
-export([fileSax/0,count/0,simply/0,escan/0,eloadToModel/0,operatingOnStruct/0, directory/0]).


%PARSE_SAX
    %proste wypisanie dokumentu
    fileSax() ->
      {ok, Xml} = file:read_file("books.xml"),
      erlsom:parse_sax(
        Xml,
        [],
        fun(Event, Acc) ->
          io:format("~p~n", [Event]),
          Acc
        end
      )
    .

    %proste zliczenie precision
    count() ->
      {ok, Xml} = file:read_file("books.xml"),
      erlsom:parse_sax(
        Xml,
        0,
        fun(Event, Acc) ->
          case Event of
            {startElement, _, "book", _, _}-> Acc + 1;
              _  -> Acc
          end
        end
      )
    .
%END PARSE_SAX

%SIMPLE FORM
simply()->
  {ok, Xml} = file:read_file("books.xml"),
  erlsom:simple_form(Xml)
.
%end SIMPLE FORM


%pozyskanie struktury/modelu po .xsd
escan()->
  erlsom:compile_xsd_file("books.xsd")
%{ok, Model} = erlsom:compile_xsd_file("books.xsd").
.


%wczytujemy do uzyskanej struktury
eloadToModel()->
  {ok, Xml} = file:read_file("books.xml"),            %otworzenie pliku xml
  {ok, Model} = erlsom:compile_xsd_file("books.xsd"), %tworzenie modelu
  {ok, Result, _} = erlsom:scan(Xml, Model),          %pobranie wyniku
  {Result,Model}                                              %zwrocenie wyniku
  %
.

operatingOnStruct() ->
  {R,_} = eloadToModel(),
  B1 = R#'BooksForm'.book,
  [B2 | _] = B1,
  B3 = B2#'BookForm'.title,
  io:format("tytul: ~s~n",[B3])
.

directory() ->
	file:get_cwd().



%otrzymujemy rekord i mozemy korzystac z jego zalet np
%struktura zapisana w hrl
%1> erlsom:write_xsd_hrl_file("product.xsd", "product.hrl", []).
%2> rr("product.hrl").
%3> product:parse("export.xml").
%#'Export'{anyAttribs = [],
%'Product' = #'Product'{anyAttribs = [],
%'SKU' = "403276",
%'ItemName' = "Trivet",
%'CollectionNo' = 0,
%'Pages' = 0}}























