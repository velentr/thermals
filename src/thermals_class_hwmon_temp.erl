-module(thermals_class_hwmon_temp).
-author("Brian Kubisiak").
-behavior(gen_server).
-export([start_link/1]).
-export([handle_call/3, handle_cast/2, init/1]).

-record(state, {path :: string(),
		scale :: integer(),
                min :: float(),
                max :: float()}).

start_link(Params) ->
    gen_server:start_link(?MODULE, Params, []).

handle_call(thermals_read, _From, State) ->
    #state{path=Path, scale=Scale, min=Min, max=Max} = State,
    Reply = case file:read_file(Path) of
		{ok, SValue} ->
		    Value = float(binary_to_integer(SValue)) / Scale,
		    {ok, if
			     Max > Value -> 1.0;
			     Value < Min -> 0.0;
			     true -> (Value - Min) / (Max - Min)
			 end};
		{error, Reason} ->
		    {error, Reason}
	    end,
    {reply, Reply, State}.

handle_cast(_Request, State) -> {noreply, State}.

init(Params) ->
    {max, Max} = proplists:lookup(max, Params),
    {min, Min} = proplists:lookup(min, Params),
    {path, Path} = proplists:lookup(path, Params),
    {index, Index} = proplists:lookup(index, Params),
    Scale = proplists:get_value(scale, Params, 1000),
    Group = proplists:lookup(group, Params),
    ok = case Group of
	     {group, Group} ->
		 thermals:join_inputs(Group);
	     none ->
		 thermals:join_inputs()
	 end,
    {ok, #state{path=lists:flatten([Path,
				    "/temp",
				    integer_to_list(Index),
				    "_input"]),
		scale=Scale,
		min=Min,
		max=Max}}.
