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

-include("records.hrl").

-spec get_status() -> {ok,#state{}} | {error, term()}.
%% @doc get status information ex: {state,{{Year,Month,Date},{Hours,Min,Sec}},false,No_of_tickets,Closed,Pending,Open,Solved}.
get_status() ->
    try
        {ok,gen_server:call(erlZenDeskStats_worker,{get_status}, 5)}
    catch
        exit:{timeout,_Other} -> {error,"Parsing in progress, please try it later"};
        Error:Reason ->
            {error,{Error, Reason}}
    end.

-spec reset_status() -> {ok,#state{}} | {error, term()}.
%% @doc reset state - prevent no parsing was done yet: {state,never,false,No_of_tickets,Closed,Pending,Open,Solved}
reset_status() ->
    try
        {ok,gen_server:call(erlZenDeskStats_worker,{reset_status}, 5)}
    catch
        exit:{timeout,_Other} -> {error,"Parsing in progress, please try it later"};
        Error:Reason ->
            {error,{Error, Reason}}
    end.    

%% @spec get_last_check() -> {string(),string()} | never | {error, term()}
%% @doc get_last_check - get timestamp parsing was done last time {ok,{Year,Month,Date},{Hours,Min,Sec}}}|{ok,never}
get_last_check() ->
    try
        gen_server:call(erlZenDeskStats_worker,{get_last_check}, 5)
    catch
        exit:{timeout,_Other} -> {error,"Parsing in progress, please try it later"};
        Error:Reason ->
            {error,{Error, Reason}}
    end.

%% @spec get_counters() -> {ok,[{term(),int()}]}
%% @doc get_counters - get counter values for [all_comments, all_tickets, no_of_solved_tickets, no_of_open_tickets, no_of_pending_tickets, no_of_closed_tickets, no_of_tickets]
get_counters() ->
    {ok,get_counters([no_of_tickets,no_of_closed_tickets,no_of_pending_tickets,no_of_open_tickets,no_of_solved_tickets,all_tickets,all_comments])}.

%% @spec get_counters([term()]) -> [{term(),int()}]
%% @doc get_counters - get the value of for the requested counters
get_counters(CList) ->
    get_counters(CList,[]).
get_counters([],Result)->
    Result;
get_counters([Counter|CList],Result)->
    R=get_counter(Counter),
    get_counters(CList,[R|Result]).

%% @spec get_counter([term()]) -> {term(),int()}
%% @doc get_counter - get the value of for the requested counter
get_counter(Counter) ->
    try 
        gen_server:call(erlZenDeskStats_worker,{get_counter, Counter}, 5)
    catch
        exit:{timeout,_Other} -> "Parsing in progress, please try it later";
        Error:Reason ->
            {Error, Reason}
    end.

%% @spec start_new_round() -> ok
%% @doc start_new_round() - start a new parsing round to include new tickets/comments into the database
start_new_round() ->
    gen_server:cast(erlZenDeskStats_worker,{start_walktrough, self()}).

%% @spec write_table_to_csv(term(),term()) -> ok | {error,any()}
%% @doc  write_table_to_csv dums the given table to the File the user provided as 2nd parameter
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

%% @spec dump_all_tables(string()) -> [ok | {error,any()}]
%% @doc Dumps all tables as CSV file into the provided directory 
dump_all_tables(Directory) ->
    Tables = mnesia:system_info(tables),
    dump_tables(Tables,Directory).

dump_tables([],_) ->
    ok;
dump_tables([schema|Tables],Directory) ->
    % io:format("schema skipped ~n",[]),
    dump_tables(Tables,Directory);
dump_tables([TableName|Tables],Directory) ->
    ok=filelib:ensure_dir(Directory++"/"),
    FileName = Directory++"/"++atom_to_list(TableName)++".csv",
    write_table_to_csv(TableName,FileName),
    io:format("~p dumped ~n",[FileName]),
    dump_tables(Tables,Directory).

%% @spec merge_stats_tables() -> ok
%% @doc Merges data for customers into a SUM table, calculates statistics
merge_stats_tables() ->
    erlZenDeskStats_funs:merge_stats(weekly),
    erlZenDeskStats_funs:merge_stats(monthly).

%% @spec gen_gnuplot_input_files() -> ok
%% @doc gen_gnuplot_input_files - converts CSV files to input format which is understandable for GNUPlot; the CSV files nees to be in tehe "tests" directory
gen_gnuplot_input_files() ->
    gen_gnuplot_input_files("tests").

%% @spec gen_gnuplot_input_files(string()) -> ok | {error,term()}
%% @doc gen_gnuplot_input_files - converts CSV files to input format which is understandable for GNUPlot; the CSV files nees to be in the given directory
gen_gnuplot_input_files(Dir) ->
    erlZenDeskStats_funs:gen_gnuplot_input_files(monthly,Dir),
    erlZenDeskStats_funs:gen_gnuplot_input_files(weekly,Dir).

%% @spec gen_gnuplot_reports(string()) -> ok | {error,term()}
%% @doc gen_gnuplot_reports - generates pdf reports to the given directory (frequency=monthly; type=histogram)  
gen_gnuplot_reports(Dir) ->
    erlZenDeskStats_funs:gen_gnuplot_reports( Dir, [{type,histogram},{freq,monthly}] ).

%% @spec gen_gnuplot_reports(string(),[{term(),term()}]) -> ok | {error,term()}
%% @doc gen_gnuplot_reports - generates pdf reports to the given directory valid Params=[{freq,[monthly|weekly]}|{type,[histogram|graph]}]
gen_gnuplot_reports(Dir,Params) -> 
    erlZenDeskStats_funs:gen_gnuplot_reports(Dir, Params).




