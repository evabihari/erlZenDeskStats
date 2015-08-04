-module(erlZenDeskStats_funs).

-compile(export_all).

-include("records.hrl").
-include("mnesia.hrl").

read_web(Url) ->
    ssl:start(),
    inets:start(),
    ContentType = "text/json",
    Headers = [auth_header(?USER, ?PWD),{"Content-Type",ContentType},
               {"User-Agent","Jable"}],
    ok = httpc:set_options([{max_keep_alive_length, 0}, 
                            {max_pipeline_length, 0}, {max_sessions, 0}]),    
    case httpc:request(get, 
                       {Url, Headers}, 
                       [{timeout,timer:seconds(30)}
                       ], []) of
        {ok,Answer} -> {success,Answer};
        {error, socket_closed_remotely} -> 
            error_logger:error_report("socket_closed_remotely",{url,Url}),
            {{error,socket_closed_remotely},error};
                   %%     error_logger:error_report("socket_closed_remotely",[]),
                   %%    {{error,socket_closed_remotely},error};
                   %% error:R -> {{error,R},error};
        Error  ->
            io:format("read_web(~p) Error=~p~n",[Url,Error]),
            {error,Error}
              % end
    end.

auth_header(User, Pass) ->
    Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
    {"Authorization","Basic " ++ Encoded}.

check(Header) ->
    case proplists:get_value("status", Header) of
	"200 OK" ->
	    ok;
	_Other ->
	    error
    end.

store_to_db(_Db, Data) ->
     mnesia:dirty_write( Data).

tokenize_dates(undefined) ->
    undefined;
tokenize_dates(DateStr) ->
    [Y,M,D|_]=string:tokens(DateStr,"- :+TZ"),
    {list_to_integer(Y),list_to_integer(M),list_to_integer(D)}.

week_number(Y,M,D)->
    calendar:iso_week_number({Y,M,D}).

clear_counters() ->
    mnesia:clear_table(monthly_stat_tickets_created),
    mnesia:clear_table(monthly_stat_tickets_solved),
    mnesia:clear_table(monthly_stat_tickets_commented),
    mnesia:clear_table(weekly_stat_tickets_created),
    mnesia:clear_table(weekly_stat_tickets_solved),
    mnesia:clear_table(weekly_stat_tickets_commented).

compare_dates(Date1,Date2) ->
    [Y1,M1,D1,H1,Min1,S1|_]=string:tokens(Date1,"- :+TZ"),
    [Y2,M2,D2,H2,Min2,S2|_]=string:tokens(Date2,"- :+TZ"),
    compare([Y1,M1,D1,H1,Min1,S1],[Y2,M2,D2,H2,Min2,S2],equal).

compare([],[],equal) ->
    equal;
compare([D1|Data1],[D2|Data2],equal) ->
    V1=list_to_integer(D1),
    V2=list_to_integer(D2),
    R = if 
            V1>V2 -> bigger;
            V1<V2 -> smaller;
            true -> equal
        end,
    compare(Data1,Data2,R);
compare(_,_,Result) ->
    Result.

                     
dump_table(IoDevice, Table_name) ->
    mnesia:dirty_first(Table_name),
    write_header_line(IoDevice, Table_name),
    Dump_to_file = fun(Rec,IO) ->
                           pretty_print(Rec,IO),
                           io:format(IO, " ~n",[]),
                           IO
                   end,
    case mnesia:is_transaction() of
        true -> mnesia:foldl(Dump_to_file,IoDevice,Table_name);
        false -> 
            Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, IoDevice,Tab) end,
            mnesia:activity(transaction,Exec,[{Dump_to_file,Table_name}],mnesia_frag)
    end,
    io:format("~p table was dumped to ~p~n",[Table_name, IoDevice]),
    IoDevice.

write_header_line(IoDevice, Table_name) ->
    %check the type of teh table key -> if it's a tuple 
    write_field_names(IoDevice,mnesia:table_info(Table_name, attributes)),
    io:format(IoDevice, "~n",[]).

write_field_names(_IO,[]) ->
    ok;
write_field_names(IO,[Field|Fields]) ->
    io:format(IO,"~p ,",[Field]),
    write_field_names(IO,Fields).

    
is_space_in_element(Item) ->
    Pos = string:chr(Item, $,),
    if Pos > 0 ->
             true;
       true ->
            false
    end.


pretty_print(Rec,IO) ->
    [_RecType|List] = tuple_to_list(Rec),
    pretty_io_list(List,IO).

pretty_io_list([],_IO) ->
    ok;
pretty_io_list([Element|List],IO) when is_integer(Element) ->
    io:format(IO, "~p,",[Element]),
    pretty_io_list(List,IO);
pretty_io_list([Element|List],IO) when is_tuple(Element) ->
    % dashed_list(tuple_to_list(Element),IO),
    S=format_tuple(lists:flatten(io_lib:format("~p",[Element]))),
    % S=lists:flatten(io_lib:format("~p",[Element])),
     io:format(IO,"~p," , [S]),
    pretty_io_list(List,IO);
pretty_io_list([null|List],IO) ->
    io:format(IO, ",",[]),
    pretty_io_list(List,IO);

pretty_io_list([Item|List],IO) when is_atom(Item) ->
    io:format(IO, " ~p,",[Item]),
    pretty_io_list(List,IO);

pretty_io_list([Element|List],IO) when is_list (Element)->
    case is_space_in_element(Element) of
           true -> 
               % io:format(IO, "~p,",[replace_comma_with_space(Element)]);
                io:format(IO, "~p,",[Element]);
           _ ->
                io:format(IO, "~p,",[Element])
    end,
    pretty_io_list(List,IO).

dashed_list([A],IO) when is_tuple(A) -> % key can be {org,{year,week}}
    dashed_list(tuple_to_list(A),IO);
dashed_list([A],IO) ->
    io:format(IO,"~p",[A]);
dashed_list([A|List],IO)->
    io:format(IO,"~p-",[A]),
    dashed_list(List,IO).

replace_comma_with_space(String) ->
    re:replace(String,","," ",[{return,list}]).

%% dirty_update_counter not working for 
%% A Counter is an Oid being {CounterTab, CounterName}

dirty_update_counter({Tab, Key}, Incr) ->
    dirty_update_counter(Tab, Key, Incr);
dirty_update_counter(Counter, _Incr) ->
    mnesia:abort({bad_type, Counter}).

dirty_update_counter(Tab, Key, Incr) ->
    do_dirty_update_counter(async_dirty, Tab, Key, Incr).

do_dirty_update_counter(SyncMode, Tab, Key, Incr)
  when is_atom(Tab), Tab /= schema, is_integer(Incr) ->
    case ?catch_val({Tab, record_validation}) of
        {RecName, 3, Type} when Type==set; Type==ordered_set->
            Oid = {Tab, Key},
            mnesia_tm:dirty(SyncMode, {Oid, {RecName, Incr}, update_counter});
        _ ->
            mnesia:abort({combine_error, Tab, update_counter})
    end;
do_dirty_update_counter(_SyncMode, Tab, _Key, Incr) ->
    mnesia:abort({bad_type, Tab, Incr}).

format_tuple(T) ->
    case re:replace(T,"\"","",[{return,list}]) of
        T ->
            T;
        NewTuple ->
            format_tuple(NewTuple)
    end.
