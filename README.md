# erlZenDeskStats


compile the project:
--------------------

rebar get-deps
rebar compile

## Note: the following fileds are stored from ZenDesk's tickets database: ##

    *  id,
    *  created_at,
    *  organizationname,
    *  priority,
    *  reopens,
    *  replies,
    *  reqname,
    *  status,
    *  tickettype,
    *  via,
    *  field23659343 -> Product & Version
    *  field23659393 -> Root couse
    *  field23659423 -> Original Priority
    *  field23666277 -> Complexity
    *  field23671848 -> How was it resolved
    *  field24366599 -> MaximumPriority


#### The last 6 fileds are custom defined fields and at least the field numbers will differ based on your ZenDesk settings.####

Start the Erlang shell (from erlZenDeskStats root directory):
-------------------------------------------------------------

erl -pa ebin/ -pa deps/lager/ebin/ -pa deps/goldrush/ebin/

## Start the application: ##
application:start(erlZenDeskStats).


API calls:
==========
* erlZenDeskStatsI:get_status().
* erlZenDeskStatsI:get_counter(no_of_tickets).
* erlZenDeskStatsI:get_counters(). -> all counters
* erlZenDeskStatsI:get_counters([Counter|Counters]) -> get more counter values;
* erlZenDeskStatsI:write_table_to_csv(Table,FileName).
	where Table = tickets/commets
		  FileName -> the file you want to dump the information (readable CSV file, ex. "Tickets.csv")
