-record(tickets, {id, 
                 created_at,
                 creation_year,
                 creation_month,
                 creation_week, %iso_week_number, ex {2015,26}
                 updated_at,
                 solved_at,
                 solved_year,
                 solved_month,
                 solved_week,
                 organization_name, 
                 priority, 
                 reopens, 
                 replies, 
                 req_name, 
                 status, 
                 ticket_type, 
                 via,
                 group_name, %Support, Riak
                 product_and_version, %field_23659343
                 root_cause, %field_23659393 -> Root couse
                 complexity, %field_23666277 -> Complexity
                 how_was_resolved, % field_23671848 -> How was it resolved
                 maximumPriority %field_24366599 -> MaximumPriority
                 }).

-record(comments, {id,
                    ticket_id,
                    organization,
                    type,
                    created_at,
                    author_id,
                    public
                    }).

- record(stats, {key, % {org, {year, month}} or {org, {year,week}}
                 tickets_created=0,
                 tickets_solved=0,
                 no_of_comments=0}).


 - ifdef(debug1).
- define( Log(Msg,Parameters), error_logger:info_report([Msg,Parameters])).
 - else.
 - define( Log(Msg,Parameters), ok).
 - endif.

- ifdef(debug1).
- define(L(Msg,Parameters), error_logger:info_report([Msg,Parameters])).
 - else.
 - define(L(Msg,Parameters), ok).
- endif.

-define(START_TIME, "1383734680").
-define(USER, "Your_user_name").
-define(PWD, "Your password").
