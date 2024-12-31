-module(simulation_workflow).
-behaviour(application).
-behaviour(supervisor).
-behaviour(gen_server).

%% Application callbacks
-export([start/2, stop/1]).
%% Supervisor callbacks
-export([init/1]).
%% Gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% API
-export([start_link/0, add_stage/4, start_simulation/0, get_status/0]).

-record(stage, {
    name,
    duration,
    resources,
    dependencies = [],
    status = waiting,
    start_time,
    end_time
}).

%% Application callbacks
start(normal, _Args) ->
    simulation_workflow:start_link().

stop(_State) ->
    ok.

%% Supervisor callbacks
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    ChildSpecs = [
        #{
            id => simulation_worker,
            start => {gen_server, start_link, [{local, ?MODULE}, ?MODULE, [], []]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [?MODULE]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% Gen_server callbacks
init([]) ->
    {ok, #{
        stages => #{},
        running => false,
        completed_stages => []
    }}.

handle_call({add_stage, Name, Duration, Resources, Dependencies}, _From, State) ->
    Stage = #stage{
        name = Name,
        duration = Duration,
        resources = Resources,
        dependencies = Dependencies
    },
    NewStages = maps:put(Name, Stage, maps:get(stages, State)),
    {reply, ok, State#{stages := NewStages}};

handle_call(start_simulation, _From, State) ->
    case maps:get(running, State) of
        true ->
            {reply, {error, already_running}, State};
        false ->
            NewState = start_stages(State#{running := true}),
            {reply, ok, NewState}
    end;

handle_call(get_status, _From, State) ->
    Status = #{
        running => maps:get(running, State),
        stages => maps:get(stages, State),
        completed => maps:get(completed_stages, State)
    },
    {reply, Status, State}.

handle_cast({stage_completed, StageName}, State) ->
    Stages = maps:get(stages, State),
    CompletedStages = maps:get(completed_stages, State),
    NewCompleted = [StageName | CompletedStages],
    
    case maps:size(Stages) =:= length(NewCompleted) of
        true ->
            {noreply, State#{running := false, completed_stages := NewCompleted}};
        false ->
            NewState = start_ready_stages(State#{completed_stages := NewCompleted}),
            {noreply, NewState}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
start_stages(State) ->
    Stages = maps:get(stages, State),
    ReadyStages = get_ready_stages(Stages, []),
    lists:foreach(fun(StageName) ->
        Stage = maps:get(StageName, Stages),
        spawn_link(fun() -> execute_stage(Stage) end)
    end, ReadyStages),
    State.

get_ready_stages(Stages, CompletedStages) ->
    maps:fold(fun(Name, Stage, Acc) ->
        case is_stage_ready(Stage, CompletedStages) of
            true -> [Name | Acc];
            false -> Acc
        end
    end, [], Stages).

is_stage_ready(Stage, CompletedStages) ->
    lists:all(fun(Dep) -> lists:member(Dep, CompletedStages) end, Stage#stage.dependencies).

execute_stage(Stage) ->
    timer:sleep(Stage#stage.duration * 1000),
    gen_server:cast(?MODULE, {stage_completed, Stage#stage.name}).

%% API
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_stage(Name, Duration, Resources, Dependencies) ->
    gen_server:call(?MODULE, {add_stage, Name, Duration, Resources, Dependencies}).

start_simulation() ->
    gen_server:call(?MODULE, start_simulation).

get_status() ->
    gen_server:call(?MODULE, get_status).
-module(simulation_workflow).
-behaviour(gen_server).

%% API
-export([start_link/0, add_stage/3, simulate/0, get_results/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(stage, {
    name,
    duration,
    resources
}).

-record(state, {
    stages = [] :: [#stage{}],
    results = [] :: [{atom(), float()}]
}).

%%% API Functions %%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_stage(Name, Duration, Resources) ->
    gen_server:cast(?MODULE, {add_stage, Name, Duration, Resources}).

simulate() ->
    gen_server:call(?MODULE, simulate).

get_results() ->
    gen_server:call(?MODULE, get_results).

%%% gen_server Callbacks %%%
init([]) ->
    {ok, #state{}}.

handle_call(simulate, _From, State) ->
    Results = run_simulation(State#state.stages),
    {reply, ok, State#state{results = Results}};

handle_call(get_results, _From, State) ->
    {reply, State#state.results, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({add_stage, Name, Duration, Resources}, State) ->
    Stage = #stage{name = Name, duration = Duration, resources = Resources},
    {noreply, State#state{stages = [Stage | State#state.stages]}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Helper Functions %%%
run_simulation(Stages) ->
    %% Simulate the workflow and calculate results
    lists:map(fun(Stage) ->
        {Stage#stage.name, Stage#stage.duration * Stage#stage.resources}
    end, Stages).