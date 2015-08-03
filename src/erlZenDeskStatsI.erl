-module(erlZenDeskStatsI).
-export([get_status/0,
         get_counters/0,
         get_counter/1,
         get_counters/1,
         start_new_round/0,
         write_table_to_csv/2]).


get_status() ->
    try
        gen_server:call(erlZenDeskStats_worker,{get_status}, 5)
    catch
        exit:{timeout,_Other} -> "Parsing in progress, please try it later";
        Error:Reason ->
             {Error, Reason}
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
    gen_server:cast(erlZenDeskStats_worker,{start_new_walktrough}).
    
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
