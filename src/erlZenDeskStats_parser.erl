-module(erlZenDeskStats_parser).
-export([init/0,get_tickets/0, start/0]).

-include("records.hrl").

start() ->
    spawn(?MODULE, init,[]).

init() ->
    io:format("parse started, Pid=~p~n",[self()]),
    get_tickets().

get_tickets() ->
    erlZenDeskStats_funs:clear_counters(),
    Src = ?ZENDESK_URL++"/exports/tickets.json",
    Query = "?start_time="++?START_TIME,
    Url = gen_url(Src, Query),
    case erlZenDeskStats_funs:read_web(Url) of
        {success, {_Status_line,Headers, Body}} ->
            case erlZenDeskStats_funs:check(Headers) of 
		ok ->
                    {struct,Results} = mochijson:decode(Body),
                    {array, TicketList} = proplists:get_value("results",Results),
                    {Tickets_no,Closed_no,Pending_no,Open_no,Solved_no}=parse(tickets,TicketList,{0,0,0,0,0}),
                    gen_server:cast(erlZenDeskStats_worker, 
                                    {zendesk_parsed, {Tickets_no,Closed_no,Pending_no,Open_no,Solved_no}});
                error ->
                    gen_server:cast(erlZenDeskStats_worker, {error, {headers,Url}})
            end;
	{error, Reason} ->
	    gen_server:cast(erlZenDeskStats_worker, {error, {Reason,Src}})
    end.

gen_url(Src, Query) ->
    Src ++ Query.

parse(tickets,[],{Tickets_no,Closed_no,Pending_no,Open_no,Solved_no})->
    {Tickets_no,Closed_no,Pending_no,Open_no,Solved_no};
parse(tickets,[{struct,List}|Structs],{Tickets_no,Closed_no,Pending_no,Open_no,Solved_no}) ->
    Created=proplists:get_value("created_at",List),
    {CY,CM,CD}=erlZenDeskStats_funs:tokenize_dates(Created),
    CW=erlZenDeskStats_funs:week_number(CY,CM,CD),
    Solved=proplists:get_value("solved_at",List),
    Status = proplists:get_value("status",List),
    Org_name = erlZenDeskStats_funs:remove_space(proplists:get_value("organization_name",List)),
    case Status of
        "Deleted" -> ok;
        _ ->
            erlZenDeskStats_funs:dirty_update_counter(monthly_stat_tickets_created,
                                                      {Org_name, {CY,CM}},1),
            erlZenDeskStats_funs:dirty_update_counter(weekly_stat_tickets_created,
                                                      {Org_name, CW},1)
    end,

    {SY,SM,SW} = case {Solved, Status} of
                     {null,_} -> {undefined, undefined, undefined};
                     {_Date, "Deleted"} -> {undefined, undefined, undefined};
                     {Date,_} ->
                         {Y,M,D}=erlZenDeskStats_funs:tokenize_dates(Date),
                         W=erlZenDeskStats_funs:week_number(Y,M,D),
                         erlZenDeskStats_funs:dirty_update_counter(monthly_stat_tickets_solved,
                                                                   {Org_name, {Y,M}},1),
                         erlZenDeskStats_funs:dirty_update_counter(weekly_stat_tickets_solved,
                                                                   {Org_name, W},1),
                         {Y,M,W}
                 end,
    Updated_at = proplists:get_value("updated_at",List),
    Id = proplists:get_value("id",List),

    T=#tickets{id = Id,
               created_at = Created,
               creation_year = CY,
               creation_month = CM,
               creation_week = CW,
               updated_at=Updated_at,
               solved_at=Solved,
               solved_year = SY,
               solved_month = SM,
               solved_week = SW,
               organization_name = 
                   erlZenDeskStats_funs:remove_space(proplists:get_value("organization_name",List)),
               priority = proplists:get_value("priority",List), 
               reopens = proplists:get_value("reopens",List), 
               replies= proplists:get_value("replies",List), 
               req_name= lists:subtract(proplists:get_value("req_name",List), ","), 
               status= Status, 
               group_name= proplists:get_value("group_name",List),
               ticket_type= proplists:get_value("ticket_type",List), 
               via= proplists:get_value("via",List),
               product_and_version= proplists:get_value("field_23659343",List), %field_23659343
               root_cause= proplists:get_value("field_23659393",List), %field_23659393 -> Root couse
               complexity= proplists:get_value("field_23666277",List), %field_23666277 -> Complexity
               how_was_resolved= proplists:get_value("field_23671848",List), 
                                                % field_23671848 -> How was it resolved
               maximumPriority= proplists:get_value("field_24366599",List) %field_24366599 -> MaximumPriority        
              },
    Store_tickets = case mnesia:dirty_read(tickets, Id) of
                        [] -> true;
                        [Old_ticket_record] ->
                            case erlZenDeskStats_funs:compare_dates(Old_ticket_record#tickets.updated_at,
                                                                    Updated_at) of
                                smaller -> true;
                                _ -> false
                            end
                    end,
    case {Status,Store_tickets} of
        {"Deleted",_} -> ok;
        {_,true} -> 
            erlZenDeskStats_funs:store_to_db(tickets,T);
        _ -> ok
    end,

    {New_Tickets_no,New_Closed_no,New_Pending_no,
     New_Open_no,New_Solved_no} = case Status of 
                                      "Open" -> {Tickets_no+1,Closed_no,Pending_no,Open_no+1,Solved_no};
                                      "Pending" -> {Tickets_no+1,Closed_no,Pending_no+1,Open_no,Solved_no};
                                      "New" -> {Tickets_no+1,Closed_no,Pending_no,Open_no+1,Solved_no};
                                      "Closed" -> {Tickets_no+1,Closed_no+1,Pending_no,Open_no,Solved_no};
                                      "Hold" -> {Tickets_no+1,Closed_no,Pending_no+1,Open_no,Solved_no};
                                      "Solved" -> {Tickets_no+1,Closed_no,Pending_no,Open_no,Solved_no+1};
                                      _ -> {Tickets_no,Closed_no,Pending_no,Open_no,Solved_no}
                                  end,

    case {Status,T#tickets.group_name, Store_tickets} of
        {_,_,false} -> ok;
        {"Deleted",_,true} -> ok;
        {_,"Riak",true} -> parse_comments(T#tickets.id, Org_name);
        {_,"Support",true} -> parse_comments(T#tickets.id, Org_name);
        {_,_,_} -> ok
    end,
    parse(tickets,Structs,{New_Tickets_no,New_Closed_no,New_Pending_no,New_Open_no,New_Solved_no});

parse(tickets,[_Other|Structs],{Tickets_no,Closed_no,Pending_no,Open_no,New_Solved_no}) ->
    parse(tickets,Structs,{Tickets_no,Closed_no,Pending_no,Open_no,New_Solved_no});

parse(comments,[],_) ->
    ok;
parse(comments,[{struct,C}|Comments],{Ticket_id,Org_name}) ->
    Created=proplists:get_value("created_at",C),
    {CY,CM,CD}=erlZenDeskStats_funs:tokenize_dates(Created),
    CW=erlZenDeskStats_funs:week_number(CY,CM,CD),
    Comment_id=proplists:get_value("id",C),
    Comment_record=#comments{
                      ticket_id=Ticket_id,
                      organization=Org_name,
                      id=Comment_id,
                      type=proplists:get_value("comment",C),
                      created_at=Created,
                      author_id=proplists:get_value("author_id",C),
                      public=proplists:get_value("public",C)
                     },
    case mnesia:dirty_read(comments, Comment_id) of
        [] ->  erlZenDeskStats_funs:dirty_update_counter(monthly_stat_tickets_commented,
                                                         {Org_name, {CY,CM}},1),
               erlZenDeskStats_funs:dirty_update_counter(weekly_stat_tickets_commented,
                                                         {Org_name, CW},1),
               erlZenDeskStats_funs:store_to_db(comments,Comment_record);
        _ -> 
            ok
    end,
    parse(comments,Comments,{Ticket_id,Org_name});
parse(comments,[_Other|Comments],{Ticket_id,Org_name}) ->
    parse(comments,Comments,{Ticket_id,Org_name}).


parse_comments(Id,Org_name) ->
                                                % curl https://{subdomain}.zendesk.com/api/v2/tickets/{ticket_id}/comments.json \
                                                % -H "Content-Type: application/json" -v -u {email_address}:{password}

    Url = ?ZENDESK_URL++"/tickets/"++integer_to_list(Id)++"/comments.json",
    parse_comments(Id,Org_name,Url).

parse_comments(Id,Org_name,Url) ->
                                                %    io:format("parse comments, Id=~p, Url=~p~n",[Id, Url]),
    case erlZenDeskStats_funs:read_web(Url) of
        {success, {{_,200,"OK"},Headers, Body}} ->
            case erlZenDeskStats_funs:check(Headers) of 
		ok -> {struct,Results} = mochijson:decode(Body),
                      {array, CommentList} = proplists:get_value("comments",Results),
                      Next_page = proplists:get_value("next_page",Results), 
                                                % pagination might be needed! - ex. XXX case, where no of comments is > 100
                                                % ?ZENDESK_URL"/tickets/206/comments.json?page=2"
                      parse(comments,CommentList,{Id,Org_name}),
                      case Next_page of
                          null -> ok;
                          Url1 ->
                              parse_comments(Id,Org_name,Url1)
                      end;
                _Error ->
                    gen_server:cast(erlZenDeskStats_worker, {error, {headers,Url}})
            end;
        {success, {{_,Code,Reason},_,_}} ->
            error_logger:info_report(["Comments not got, Code, Reason",{Code,Reason}]),
            gen_server:cast(erlZenDeskStats_worker, {error, {http_answer,Code}});
	{error, Reason} ->
	    gen_server:cast(erlZenDeskStats_worker, {error, {Reason,Url}});
        _Error -> 
            gen_server:cast(erlZenDeskStats_worker, {error, Url})
    end.


