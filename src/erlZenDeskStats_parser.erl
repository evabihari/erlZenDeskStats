-module(erlZenDeskStats_parser).
-export([init/0,get_tickets/0, start/0]).

-include("records.hrl").

start() ->
    spawn(?MODULE, init,[]).

init() ->
    io:format("parse started, Pid=~p~n",[self()]),
    get_tickets().

get_tickets() ->
   % erlZenDeskStats_funs:clear_counters(),
    Src = ?ZENDESK_URL++"/exports/tickets.json",
    Query = "?start_time="++?START_TIME,
    Url = gen_url(Src, Query),
    case erlZenDeskStats_funs:read_web(Url) of
        {success, {_Status_line,Headers, Body}} ->
            case erlZenDeskStats_funs:check(Headers) of 
		ok ->
                    {struct,Results} = mochijson2:decode(Body),
                    TicketList = erlZenDeskStats_funs:get_value("results",Results),
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
    Group_name = erlZenDeskStats_funs:get_value("group_name", List),
    parse_ticket(Group_name,[{struct,List}|Structs],{Tickets_no,Closed_no,Pending_no,Open_no,Solved_no});
parse(tickets,[_Other|Structs],{Tickets_no,Closed_no,Pending_no,Open_no,Solved_no}) ->
    parse(tickets,Structs,{Tickets_no,Closed_no,Pending_no,Open_no,Solved_no});

parse(comments,[],_) ->
    ok;
parse(comments,[{struct,C}|Comments],{Ticket_id,Org_name}) ->
    Created=erlZenDeskStats_funs:get_value("created_at",C),
    {CY,CM,CD}=erlZenDeskStats_funs:tokenize_dates(Created),
    CW=erlZenDeskStats_funs:week_number(CY,CM,CD),
    Comment_id=erlZenDeskStats_funs:get_value("id",C),
    Comment_record=#comments{
                      ticket_id=Ticket_id,
                      organization=Org_name,
                      id=Comment_id,
                      type=erlZenDeskStats_funs:get_value("comment",C),
                      created_at=Created,
                      author_id=erlZenDeskStats_funs:get_value("author_id",C),
                      public=erlZenDeskStats_funs:get_value("public",C)
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
    case erlZenDeskStats_funs:read_web(Url) of
        {success, {{_,200,"OK"},Headers, Body}} ->
            case erlZenDeskStats_funs:check(Headers) of 
		ok -> {struct,Results} = mochijson2:decode(Body),
                      CommentList = erlZenDeskStats_funs:get_value("comments",Results),
                      Next_page = erlZenDeskStats_funs:get_value("next_page",Results), 
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

parse_ticket("Support",[{struct,List}|Structs],
             {Tickets_no,Closed_no,Pending_no,Open_no,Solved_no})->
    parse_ticket("Riak",[{struct,List}|Structs],{Tickets_no,Closed_no,Pending_no,Open_no,Solved_no});
parse_ticket("Riak",[{struct,List}|Structs],{Tickets_no,Closed_no,Pending_no,Open_no,Solved_no})->
    Created=erlZenDeskStats_funs:get_value("created_at",List),
    {CY,CM,CD}=erlZenDeskStats_funs:tokenize_dates(Created),
    CW=erlZenDeskStats_funs:week_number(CY,CM,CD),
    Solved=erlZenDeskStats_funs:get_value("solved_at",List),
    Status = erlZenDeskStats_funs:get_value("status",List),
    Org_name = erlZenDeskStats_funs:remove_space(erlZenDeskStats_funs:get_value("organization_name",List)),
    Updated_at = erlZenDeskStats_funs:get_value("updated_at",List),
    Id = erlZenDeskStats_funs:get_value("id",List),
    % set ticket state: [new | no_change | {updated,Old_record}]
    Ticket_state = case mnesia:dirty_read(tickets, Id) of
                        [] -> new;
                        [Old_ticket_record] ->
                            case erlZenDeskStats_funs:compare_dates(Old_ticket_record#tickets.updated_at,
                                                                    Updated_at) of
                                smaller -> 
                                     {updated,Old_ticket_record};
                                _ -> no_change
                            end
                    end,
    % check solution date and update solution counters if needed
    {SY,SM,SW} = case {Solved, Status, Ticket_state} of
                     {null,_,_} -> {undefined, undefined, undefined};
                     {_Date, "Deleted",_} -> {undefined, undefined, undefined};
                     {Date,_,St} when ((St==new) orelse 
                                       (is_tuple(St) andalso (element(1,St)==updated)))->
                         % increase solved counter only if the state became solved after the last update
                         Old_state=case St of
                                       new -> new;
                                       {updated, Old_rec} ->
                                           Old_rec#tickets.status
                         end,
                         {Y,M,D}=erlZenDeskStats_funs:tokenize_dates(Date),
                          W=erlZenDeskStats_funs:week_number(Y,M,D),
                         case Old_state of 
                             "Solved" -> {Y,M,W};
                             _ ->            
                                 erlZenDeskStats_funs:dirty_update_counter(monthly_stat_tickets_solved,
                                                                   {Org_name, {Y,M}},1),
                                 erlZenDeskStats_funs:dirty_update_counter(weekly_stat_tickets_solved,
                                                                   {Org_name, W},1),
                                 {Y,M,W}
                         end;
                      {Date,_,no_change} ->
                         erlZenDeskStats_funs:tokenize_dates(Date)
                 end,
    Priority = erlZenDeskStats_funs:get_value("priority",List),
    %handle MaximumPriority field  => field_24366599 -> MaximumPriority
    MaxPrio = case erlZenDeskStats_funs:get_value("field_24366599",List) of 
                  none -> Priority;
                  Value -> Value
              end,
 
    % create tickets record
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
                   erlZenDeskStats_funs:remove_space
                     (erlZenDeskStats_funs:get_value("organization_name",List)),
               priority = Priority, 
               reopens = erlZenDeskStats_funs:get_value("reopens",List), 
               replies= erlZenDeskStats_funs:get_value("replies",List), 
               req_name= lists:subtract(erlZenDeskStats_funs:get_value("req_name",List), ","), 
               status= Status, 
               group_name= erlZenDeskStats_funs:get_value("group_name",List),
               ticket_type= erlZenDeskStats_funs:get_value("ticket_type",List), 
               via= erlZenDeskStats_funs:get_value("via",List),
               product_and_version= erlZenDeskStats_funs:get_value("field_23659343",List), %field_23659343
               root_cause= erlZenDeskStats_funs:get_value("field_23659393",List), 
                                                %field_23659393 -> Root couse
               complexity= erlZenDeskStats_funs:get_value("field_23666277",List), 
                                                %field_23666277 -> Complexity
               how_was_resolved= erlZenDeskStats_funs:get_value("field_23671848",List), 
                                                % field_23671848 -> How was it resolved
               maximumPriority= MaxPrio        
              },
    % update creation counters
    case {Status,Ticket_state} of
        {"Deleted",_} -> ok;
        {_, new} ->
            erlZenDeskStats_funs:dirty_update_counter(monthly_stat_tickets_created,
                                                      {Org_name, {CY,CM}},1),
            erlZenDeskStats_funs:dirty_update_counter(weekly_stat_tickets_created,
                                                      {Org_name, CW},1);
        _ -> ok
    end,  
    % store tickets record
    % Ticket_state = [new | no_change | {updated,Old_record}]
    case {Status,Ticket_state} of
        {"Deleted",_} -> ok;
        {_, State} when ((State==new) orelse 
                         (is_tuple(State) andalso (element(1,State)==updated))) ->
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
    % parse comments
    case {Status,T#tickets.group_name,Ticket_state} of
        {"Deleted",_,_} -> ok;
        {_,_,no_change} -> ok;
        {_,Group,new} when ((Group=="Riak") orelse (Group=="Support"))
                           -> parse_comments(T#tickets.id, Org_name);
        {_,Group,{updated,_}} when ((Group==<<"Riak">>) orelse (Group=="Support"))
                           -> parse_comments(T#tickets.id, Org_name);
        _ -> ok
    end,
    parse(tickets,Structs,{New_Tickets_no,New_Closed_no,New_Pending_no,New_Open_no,New_Solved_no});
parse_ticket(_Other,[_Struct|Structs],{Tickets_no,Closed_no,Pending_no,Open_no,Solved_no}) ->
    parse(tickets,Structs,{Tickets_no,Closed_no,Pending_no,Open_no,Solved_no}).
