-module(erlZenDeskStatsI).
-export([get_status/0,
         get_last_check/0,
         get_counters/0,
         get_counter/1,
         get_counters/1,
         start_new_round/0,
         write_table_to_csv/2,
         dump_all_tables/1,
         merge_stats_tables/0,
         gen_gnuplot_input_files/0,
         gen_gnuplot_input_files/1,
         gen_gnuplot_reports/1,
         gen_gnuplot_reports/2,
         reset_status/0]).

get_status() ->
    try
        gen_server:call(erlZenDeskStats_worker,{get_status}, 5)
    catch
        exit:{timeout,_Other} -> {erro,"Parsing in progress, please try it later"};
        Error:Reason ->
            {error,{Error, Reason}}
    end.

reset_status() ->
    try
        gen_server:call(erlZenDeskStats_worker,{reset_status}, 5)
    catch
        exit:{timeout,_Other} -> {erro,"Parsing in progress, please try it later"};
        Error:Reason ->
            {error,{Error, Reason}}
    end.    


get_last_check() ->
    try
        gen_server:call(erlZenDeskStats_worker,{get_last_check}, 5)
    catch
        exit:{timeout,_Other} -> {erro,"Parsing in progress, please try it later"};
        Error:Reason ->
            {error,{Error, Reason}}
    end.

get_counters() ->
    get_counters([no_of_tickets,no_of_closed_tickets,no_of_pending_tickets,no_of_open_tickets,no_of_solved_tickets,all_tickets,all_comments]).

get_counters(CList) ->
    get_counters(CList,[]).
get_counters([],Result)->
    Result;
get_counters([Counter|CList],Result)->
    R=get_counter(Counter),
    get_counters(CList,[R|Result]).

get_counter(Counter) ->
    try 
        gen_server:call(erlZenDeskStats_worker,{get_counter, Counter}, 5)
    catch
        exit:{timeout,_Other} -> "Parsing in progress, please try it later";
        Error:Reason ->
            {Error, Reason}
    end.

start_new_round() ->
    gen_server:cast(erlZenDeskStats_worker,{start_walktrough, self()}).

write_table_to_csv(Table,FileName) ->
    try 
        gen_server:call(erlZenDeskStats_worker,{store_table_to_csv,Table,FileName}, 5000)
    catch
        exit:{timeout,_Other} ->  
            io:format("Other = ~p~n",[_Other]),
            "Samething has happend";
        Error:Reason ->
            {Error, Reason}
    end.    

dump_all_tables(FileNamePrefix) ->
    Tables = mnesia:system_info(tables),
    dump_tables(Tables,FileNamePrefix).

dump_tables([],_) ->
    ok;
dump_tables([schema|Tables],Directory) ->
    io:format("schema skipped ~n",[]),
    dump_tables(Tables,Directory);
dump_tables([TableName|Tables],Directory) ->
    ok=filelib:ensure_dir(Directory++"/"),
    FileName = Directory++"/"++atom_to_list(TableName)++".csv",
    write_table_to_csv(TableName,FileName),
    io:format("~p dumped ~n",[FileName]),
    dump_tables(Tables,Directory).

merge_stats_tables() ->
    erlZenDeskStats_funs:merge_stats(weekly),
    erlZenDeskStats_funs:merge_stats(monthly).

gen_gnuplot_input_files() ->
    gen_gnuplot_input_files("tests").

gen_gnuplot_input_files(Dir) ->
    erlZenDeskStats_funs:gen_gnuplot_input_files(monthly,Dir),
    erlZenDeskStats_funs:gen_gnuplot_input_files(weekly,Dir).

gen_gnuplot_reports(Dir) ->
    erlZenDeskStats_funs:gen_gnuplot_reports( Dir, [{type,histogram},{freq,monthly}] ).

gen_gnuplot_reports(Dir,Params) -> 
    erlZenDeskStats_funs:gen_gnuplot_reports(Dir, Params).




