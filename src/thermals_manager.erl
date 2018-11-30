-module(thermals_manager).
-author("Brian Kubisiak").
-behavior(supervisor).
-export([start_child/2, start_link/0, start_link/2]).
-export([init/1]).

-type thermals_class() :: fault | hwmon_temp.

-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(Type :: thermals_class(), Params :: term()) ->
			 {ok, pid()} | {error, Reason :: term()}.
start_child(Type, Params) ->
    supervisor:start_child(?MODULE, [Type, Params]).

-spec start_link(Type :: thermals_class(), Params :: term()) ->
			{ok, pid()} | {error, unrecognized}.
start_link(Type, Params) ->
    case Type of
	fault ->
	    thermals_class_fault:start_link(Params);
	hwmon_temp ->
	    thermals_class_hwmon_temp:start_link(Params);
	_ ->
	    {error, unrecognized}
    end.

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => simple_one_for_one},
	  [#{id => thermals_class,
	     start => {thermals_manager, start_link, []},
	     type => worker}]}}.
