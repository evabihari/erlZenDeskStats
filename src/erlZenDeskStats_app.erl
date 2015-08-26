-module(erlZenDeskStats_app).

-behaviour(application).


%% Application callbacks
-export([start/2, stop/1]).

-include("records.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    error_logger:tty(false),
    ok=error_logger:logfile({open, log_report}),
    lager:start(),
    mnesia:create_schema([node()]),
    mnesia:start(),
                                                % mnesia:create_table(tickets,[{attributes, record_info(fields, ticket)}]),
    case mnesia:create_table(tickets, 
                             [{disc_copies,[node()]},
                              {type, ordered_set},
                              {attributes, record_info(fields, tickets)},
                              {record_name, tickets}]) of
        {atomic,ok} -> ok;
        {aborted,{already_exists,tickets}} -> 
            error_logger:info_msg("tickets table already_exists");
        Other ->
            error_logger:error_msg(["tickets table creation failed ",{reason,Other}])
    end,
    case mnesia:create_table(comments, 
                             [{disc_copies,[node()]},
                              {type, ordered_set},
                              {attributes, record_info(fields, comments)},
                              {record_name, comments}]) of
        {atomic,ok} -> ok;
        {aborted,{already_exists,comments}} -> 
            error_logger:info_msg("comments table already_exists");
        Error ->
            error_logger:error_msg(["comments table creation failed ",{error,Error}])
    end,
    error_logger:info_msg("mnesia table tickets created"),
    Type = ordered_set,
    mnesia:create_table(monthly_stat_tickets_created,
                        [{disc_copies,[node()]}, {type, Type},{attributes,record_info(fields,stat_counter)},{record_name,stat_counter}]),
    mnesia:create_table(monthly_stat_tickets_solved,
                        [{disc_copies,[node()]}, {type, Type},{attributes,record_info(fields,stat_counter)},{record_name,stat_counter}]),
    mnesia:create_table(monthly_stat_tickets_commented,
                        [{disc_copies,[node()]}, {type, Type},{attributes,record_info(fields,stat_counter)},{record_name,stat_counter}]),
    mnesia:create_table(weekly_stat_tickets_created,
                        [{disc_copies,[node()]}, {type, Type},{attributes,record_info(fields,stat_counter)},{record_name,stat_counter}]),
    mnesia:create_table(weekly_stat_tickets_solved,
                        [{disc_copies,[node()]}, {type, Type},{attributes,record_info(fields,stat_counter)},{record_name,stat_counter}]),
    mnesia:create_table(weekly_stat_tickets_commented,
                        [{disc_copies,[node()]}, {type, Type},{attributes,record_info(fields,stat_counter)},{record_name,stat_counter}]),
    % erlZenDeskStats_funs:clear_counters(),

    erlZenDeskStats_sup:start_link().

stop(_State) ->
    ok.
