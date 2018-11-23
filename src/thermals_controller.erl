-module(thermals_controller).
-author("Brian Kubisiak").
-behavior(gen_statem).
-export([start_link/1]).
-export([callback_mode/0, init/1, hi/3, lo/3]).

-record(data, {group :: atom(),
               hi :: number(),
               lo :: number()}).

start_link(Params) when is_map(Params) ->
    gen_statem:start_link(?MODULE, Params, []).

saturate(Val) ->
    if Val < 0.0 ->
	    0.0;
       1.0 < Val ->
	    1.0;
       true ->
	    Val
    end.

read_all(Group) ->
    Inputs = pg2:get_members({Group, inputs}),
    lists:foldl(fun(Pid, {Status, Errs}) ->
			case gen_server:call(Pid, thermals_read) of
			    {ok, S} -> {max(saturate(S), Status), Errs};
			    {error, Reason} -> {1.0, [Reason | Errs]}
			end
		end, {0.0, []}, Inputs).

write_all(Status, Group) ->
    Outputs = pg2:get_members({Group, outputs}),
    lists:foldl(fun(Pid, Errs) ->
			case gen_server:call(Pid, {thermals_write, Status}) of
			    ok -> Errs;
			    {error, Reason} -> [Reason | Errs]
			end
		end, [], Outputs).

callback_mode() -> state_functions.

handle_poll(Group) ->
    {Status, IErrs} = read_all(Group),
    OErrs = write_all(Status, Group),
    Fault = fun(Err) ->
		    thermals:fault(Group, Err)
	    end,
    ok = lists:foreach(Fault, IErrs),
    ok = lists:foreach(Fault, OErrs),
    Status.

init(Params=#{group := Group, interval := Interval}) ->
    %% if no hi or lo thresholds are provided, we just set them to
    %% values that will never happen under normal circumstances
    %%
    %% XXX modify the controller algorithm depending on whether or not
    %% these are provided
    Hi = case maps:find(hi, Params) of
	     {ok, HV} -> HV;
	     error -> 1.1
	 end,
    Lo = case maps:find(lo, Params) of
	     {ok, LV} -> LV;
	     error -> -0.1
	 end,
    %% Interval timers are linked to the calling process, so we don't
    %% need to save the tref here
    {ok, _TRef} = timer:send_interval(Interval, poll),
    {ok, lo, #data{group=Group, hi=Hi, lo=Lo}}.

lo({call, From}, _Contents, _Data) ->
    {keep_state_and_data, {reply, From, notimplemented}};
lo(cast, _Contents, _Data) ->
    keep_state_and_data;
lo(info, poll, Data=#data{group=Group, hi=Hi}) ->
    Status = handle_poll(Group),
    if Status =< Hi ->
	    keep_state_and_data;
       Status > Hi ->
	    thermals:hi(Group, Status),
	    {next_state, hi, Data}
    end.

hi({call, From}, _Contents, _Data) ->
    {keep_state_and_data, {reply, From, notimplemented}};
hi(cast, _Contents, _Data) ->
    keep_state_and_data;
hi(info, poll, Data=#data{group=Group, lo=Lo}) ->
    Status = handle_poll(Group),
    if Status >= Lo ->
	    thermals:hi(Group, Status),
	    keep_state_and_data;
       Status < Lo ->
	    thermals:lo(Group, Status),
	    {next_state, lo, Data}
    end.
