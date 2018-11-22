-module(thermals_class_fault).
-author("Brian Kubisiak").
-behavior(gen_server).
-export([start_link/1]).
-export([handle_call/3, handle_cast/2, init/1]).

start_link(Params) ->
    gen_server:start_link(?MODULE, Params, []).

handle_call(thermals_read, _From, []) ->
    {reply, {error, "simulated input fault"}, []};
handle_call({thermals_write, _Status}, _From, []) ->
    {reply, {error, "simulated output fault"}, []}.

handle_cast(_Request, []) -> {noreply, []}.

init(Params) ->
    UseAsInput = proplists:get_bool(input, Params),
    UseAsOutput = proplists:get_bool(output, Params),
    Group = proplists:lookup(group, Params),

    Args = case Group of
	       {group, Group} ->
		   [Group];
	       none ->
		   []
	   end,

    ok = if UseAsInput ->
		 apply(thermals, join_inputs, Args);
	    true -> ok
	 end,
    ok = if UseAsOutput ->
		 apply(thermals, join_outputs, Args);
	    true -> ok
	 end,
    {ok, []}.
