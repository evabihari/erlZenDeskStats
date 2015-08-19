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
            error_logger:error_report(["socket_closed_remotely",{url,Url}]),
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
                                                % mnesia:clear_table(monthly_stat_tickets_commented),
                                                % mnesia:clear_table(weekly_stat_tickets_commented),
    mnesia:clear_table(weekly_stat_tickets_created),
    mnesia:clear_table(weekly_stat_tickets_solved).

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
                                                % io:format("~p table was dumped to ~p~n",[Table_name, IoDevice]),
    IoDevice.

write_header_line(IoDevice, Table_name) ->
                                                %check the type of the table key -> if it's a tuple 
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

pretty_io_list([[]|List],IO) ->
    io:format(IO, "~s,",["N/A"]),
    pretty_io_list(List,IO);

pretty_io_list([Element|List],IO) when is_list (Element)->
    case is_space_in_element(Element) of
        true -> 
                                                % io:format(IO, "~p,",[replace_comma_with_space(Element)]);
            io:format(IO, "~s,",[Element]);
        _ ->
            io:format(IO, "~s,",[Element])
    end,
    pretty_io_list(List,IO).

dashed_list([A],IO) when is_tuple(A) -> % key can be {org,{year,week}}
    dashed_list(tuple_to_list(A),IO);
dashed_list([A],IO) ->
    io:format(IO,"~p",[A]);
dashed_list([A|List],IO)->
    io:format(IO,"~p-",[A]),
    dashed_list(List,IO).

remove_space([]) ->
    [];
remove_space(String) ->
    case re:replace(String," ","_",[{return,list}]) of
        String ->
            String;
        NewString ->
            remove_space(NewString)
    end.
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

merge_stats(Type) when is_atom(Type) ->
    merge_stats(atom_to_list(Type));
merge_stats(Type) ->
    Stats_tables = [T || T <- mnesia:system_info(tables), string:str(atom_to_list(T),Type++"_stat_tickets")>0],
                                                % Stats tables contains records with {attributes,[key, counter]}  
    Table=list_to_atom(Type++"_stats"),
    mnesia:delete_table(Table),
    mnesia:create_table(Table,
                        [{disc_copies,[node()]},
                         {type, ordered_set},
                         {attributes, record_info(fields,stats)},
                         {record_name, stats}]),
    merge_tables(Stats_tables, Table).


merge_tables(InTables, OutTable) ->
    {AllKeys,AllOrg} = collect_all_keys(InTables,{[],[]}),
    create_initial_records(OutTable, AllOrg),
    merge_tables(InTables, OutTable, AllKeys),
    include_sum_records(OutTable, AllKeys).

collect_all_keys([],{Keys,Orgs}) ->
    {Keys,Orgs};
collect_all_keys([Table|Tables],{Keys,Orgs}) ->
    Keys_in_this_table=mnesia:dirty_all_keys(Table),
    AllKeys = lists:umerge([Keys_in_this_table, Keys]),
    AllOrgs = collect_organisations(AllKeys,Orgs),
    collect_all_keys(Tables, {AllKeys,AllOrgs}).

collect_organisations([],Orgs) ->
    Orgs;
collect_organisations([{Org,_}|Keys],Orgs) ->
    AllOrgs =  lists:umerge([[Org], Orgs]),
    collect_organisations(Keys, AllOrgs).

merge_tables(_InTables, _OutTable, []) ->
    ok;
merge_tables(InTables, OutTable, [Key|Keys]) ->
    Obj = create_object(InTables, Key),
    ok=mnesia:dirty_write(OutTable, Obj),
                                                %include_to_sum_table(OutTable,NewObj),
    merge_tables(InTables, OutTable, Keys).

create_object(InTables, Key) ->
    Org= element(1, Key),
    {Year,M_or_W} = element(2, Key),
    create_object(InTables, Key, #stats{key=Key,
                                        organization = Org,
                                        year=Year,
                                        month_or_week=M_or_W,
                                        year_and_period=integer_to_list(Year) ++ "/" ++ integer_to_list(M_or_W),
                                        tickets_created=0,
                                        tickets_solved=0,
                                        tickets_commented=0}).

create_object([], _Key, StatsObj) ->
    StatsObj;
create_object([InTable|InTables], Key, StatsObj) ->
    In_name=atom_to_list(InTable), % ex. weekly_stat_tickets_commented
    Postfix=list_to_atom(lists:last(string:tokens(In_name,"_"))), % ex. commented
    NewObj = case mnesia:dirty_read(InTable, Key) of
                 [Obj|_] ->
                     Counter = Obj#stat_counter.counter,
                     case Postfix of
                         created ->
                             StatsObj#stats{tickets_created = Counter};
                         solved ->
                             StatsObj#stats{tickets_solved = Counter};
                         commented ->
                             StatsObj#stats{tickets_commented = Counter}
                     end;
                 _ -> StatsObj
             end,
    create_object(InTables, Key, NewObj).

include_sum_records(_OutTable, [])->
    ok;
include_sum_records(OutTable, [Key|Keys]) ->
    {_Org,Date_part} = Key,
    {Year,M_or_W}=Date_part,
    MatchRecord = #stats{key={'_',Date_part}, _ = '_'},
    StatList = ets:match_object(OutTable, MatchRecord),
    Init_record = #stats{key={"SUM",Date_part},
                         organization = "SUM",
                         year=Year,
                         month_or_week=M_or_W,
                         year_and_period=integer_to_list(Year) ++ "/" ++ integer_to_list(M_or_W),                                                            tickets_created=0,
                         tickets_solved=0,
                         tickets_commented=0},         
    Result_record =sum_values(StatList,Init_record),
    mnesia:dirty_write(OutTable,Result_record),
    include_sum_records(OutTable, Keys).    

sum_values([],Record) ->
    Record;
sum_values([Obj|List],Record) ->
    R_tickets_created=Record#stats.tickets_created,
    R_tickets_solved=Record#stats.tickets_solved,
    R_tickets_commented=Record#stats.tickets_commented,
    NewRecord = case Obj#stats.organization of
                    "SUM" -> Record;
                    _ ->
                        Record#stats{
                          tickets_created=R_tickets_created + Obj#stats.tickets_created,
                          tickets_solved=R_tickets_solved + Obj#stats.tickets_solved,
                          tickets_commented=R_tickets_commented + Obj#stats.tickets_commented}
                end,
    sum_values(List,NewRecord).

gen_gnuplot_input_files(Type,Dir) when is_atom(Type) ->
    gen_gnuplot_input_files(atom_to_list(Type),Dir);
gen_gnuplot_input_files(Type,Dir) ->
    Table=list_to_atom(Type++"_stats"),
    Keys=mnesia:dirty_all_keys(Table),
    Orgs=collect_orgs(Keys,[]),
    create_org_files(Orgs,Type,Dir).

collect_orgs([],Orgs) ->
    Orgs;
collect_orgs([Key|Keys],Orgs) ->
    {Org,_Date} = Key,
    collect_orgs(Keys,lists:umerge([Org],Orgs)).

create_org_files([],_Type,_Dir) ->
    ok;
create_org_files(Orgs,Type,Dir) when is_atom(Type) ->
    create_org_files(Orgs,atom_to_list(Type),Dir);
create_org_files([Org|Orgs],Type,Dir) ->
    Table=list_to_atom(Type++"_stats"),
                                                %MatchRecord = #stats{key={'_',Date_part}, _ = '_'},

    Match_record = #stats{key={Org,'_'}, _ = '_'},
    ObjList = ets:match_object(Table, Match_record),
    ok=filelib:ensure_dir(Dir++"/"),
    FileName = case Org of
                   [] -> Dir++"/Non_org_"++Type++".csv";
                   OName -> Dir++"/"++OName++"_"++Type++".csv"
               end,
    {ok, IoDevice} = file:open(FileName,[write]),
    write_header_line(IoDevice, monthly_stats),
    store_objects(ObjList, IoDevice),
    file:close(IoDevice),
    create_org_files(Orgs, Type,Dir).

store_objects([],_IoDevice) ->
    ok;
store_objects([Obj|ObjList],IO) ->
    pretty_print(Obj,IO),
    io:format(IO, " ~n",[]),
    store_objects(ObjList, IO).

%% gen_gnuplot_reports(graph, Dir) ->
%%     gen_gnuplot_reports("my_csv2gnuplot.sh", Dir);
%% gen_gnuplot_reports(histogram, Dir) ->
%%     gen_gnuplot_reports("histogram_csv2gnuplot.sh", Dir);

gen_gnuplot_reports(Type, Dir) when is_atom(Type) ->
    gen_gnuplot_reports(atom_to_list(Type), Dir);
gen_gnuplot_reports(Type, Dir) ->              
     generate_gnuplot_reports("generate_reports_gnuplot.sh",Type, Dir).

generate_gnuplot_reports(Script,Type, Dir) ->
    case erlZenDeskStatsI:get_last_check() of
        {error, Reason} ->
            {error, Reason};
        never ->
            {error, "ZenDesk was not parsed yet"};
        Value ->
            case check_difference(Value) of
                not_ok ->
                    erlZenDeskStatsI:start_new_round(),
                    {error, "Parse Zendesk before generating reports"};
                ok ->
                    ok=erlZenDeskStatsI:merge_stats_tables(),
                    ok=erlZenDeskStatsI:dump_all_tables(Dir),
                    ok=erlZenDeskStats_funs:gen_gnuplot_input_files(monthly,Dir),
                    {ok,Current_dir}=file:get_cwd(),
                    ok=file:set_cwd(Dir),
                    Cmd="cp ../scripts/"++Script++" .",
                    os:cmd(Cmd),
                    Cmd2="./"++Script++" "++Type,
                    Result=os:cmd(Cmd2),
                    ok=file:set_cwd(Current_dir),
                    ok
            end
    end.

check_difference({{Y,M,D},{H,Min,Sec}}) ->
    case calendar:time_difference
        ({{Y,M,D},{H,Min,Sec}},
         calendar:local_time() ) of
        {0,_} ->
            ok;
        _-> not_ok
    end.

 create_initial_records(Table,Orgs) ->
    Start_date = {2013,10},
    {{EY,EM,_},_} = calendar:local_time(),
    % End_date = integer_to_list(EY) ++ "/" ++ integer_to_list(EM),
    End_date = {EY,EM},
    insert_records(Table,Orgs,{Start_date,End_date}).

 insert_records(_Table,[],_)->
    ok;
 insert_records(Table,[Org|Orgs],{Start_date,End_date}) ->
    insert_records_for_organization(Table,Org,{Start_date,End_date}),
    insert_records(Table, Orgs,{Start_date,End_date}).

 insert_records_for_organization(Table,Org,{End,End}) ->
    insert_rec_for_organization(Table,Org,End);
 insert_records_for_organization(Table,Org,{Date, End}) ->
    {Year,M_W} = Date,
    insert_rec_for_organization(Table,Org,{Year,M_W}),
    NewDate = case {Table,(M_W +1)} of
        {monthly_stats,NewMonth} when NewMonth < 13 -> {Year,NewMonth};
        {weekly_stats,NewWeek} when NewWeek < 53 -> {Year, NewWeek};
        _NewMonth ->
                       {Year+1, 1}
              end,
   insert_records_for_organization(Table,Org,{NewDate, End}).
             
insert_rec_for_organization(Table,Org,Date) ->
    {Year, Month} = Date,
    Record = #stats{key={Org,{Year,Month}},
                    organization = Org,
                    month_or_week=Month,
                    year_and_period=integer_to_list(Year) ++ "/" ++ integer_to_list(Month),
                    tickets_created=0,
                    tickets_solved=0,
                    tickets_commented=0},
    mnesia:dirty_write(Table,Record).
