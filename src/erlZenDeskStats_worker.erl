%%%-------------------------------------------------------------------
%%% @author eva.bihari <evabihari@Evas-MacBook-Pro.local>
%%% @copyright (C) 2015, eva.bihari
%%% @doc
%%%
%%% @end
%%% Created : 29 Jul 2015 by eva.bihari <evabihari@Evas-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(erlZenDeskStats_worker).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {last_check=never,
                parsing_in_progress=true,
                no_of_tickets = 0,
                no_of_closed_tickets = 0,
                no_of_pending_tickets = 0,
                no_of_solved_tickets = 0,
                no_of_open_tickets=0}).

-include("records.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    error_logger:info_report("erlZenDeskStats_worker:init"),
    erlZenDeskStats_parser:start(),
    {ok, #state{parsing_in_progress=true}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_status},_From, State) ->
    {reply, State, State};
handle_call({get_counter, Counter},_From, State) ->
    Answer = case { State#state.parsing_in_progress,Counter} of
                 {true,_} -> "Parsing in progress, please try it later";
                 {false,no_of_tickets} -> State#state.no_of_tickets;
                 {false,no_of_closed_tickets} -> State#state.no_of_closed_tickets;
                 {false,no_of_pending_tickets} -> State#state.no_of_pending_tickets;
                 {false,no_of_open_tickets} -> State#state.no_of_open_tickets;
                 {false,no_of_solved_tickets} -> State#state.no_of_solved_tickets;
                 {false,all_tickets} -> mnesia:table_info(tickets,size);
                 {false,all_comments} ->  mnesia:table_info(comments,size);
                 _ -> undefined
             end,
    {reply,{Counter, Answer},State};

handle_call({store_table_to_csv,Table,FileName}, _From, State) ->
    {ok, IoDevice} = file:open(FileName,[write]),
    Reply = case erlZenDeskStats_funs:dump_table(IoDevice, Table) of
                IoDevice ->
                    ok;
                Other ->
                    {error,Other}
            end,
    file:close(IoDevice),
    {reply, Reply, State};


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({start_walktrough}, State) ->
    erlZenDeskStats_parser:start(),
    {noreply, State#state{parsing_in_progress=true}};

handle_cast({zendesk_parsed,{Tickets_no,Closed_no,Pending_no,Open_no, Solved_no}}, State) ->
    error_logger:info_report("zendesk_parsed"),
    NewState =  State#state{last_check=erlang:localtime(),
                            no_of_tickets = Tickets_no,
                            no_of_closed_tickets = Closed_no,
                            no_of_pending_tickets = Pending_no,
                            no_of_solved_tickets = Solved_no,
                            no_of_open_tickets=Open_no,
                            parsing_in_progress=false},
    ?Log("ZenDesk ticketes parsed successfully at ~p~n",[NewState#state.last_check]),
    io:format("ZenDesk tickets parsed successfully  new State is ~p~n",[NewState]),
    {noreply, NewState};
handle_cast({error,_Reason},State) ->
    ?Log("Error while getting tickets", [{reason,_Reason}]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    io:format("ZenDeskStats_worker got info msg ~p~n",[_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("ZenDeskStats_worker terminated with Reason ~p~n",[_Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

