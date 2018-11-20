-module(thermals_controller).
-author("Brian Kubisiak").
-behavior(gen_server).
-export([start_link/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, init/1]).

-record(state, {group :: atom()}).

start_link(Params) when is_map(Params) ->
  gen_server:start_link(?MODULE, Params, []).

read_all(Group) ->
  Inputs = pg2:get_members({Group, inputs}),
  lists:foldl(fun(Pid, {Status, Errs}) ->
                  case gen_server:call(Pid, thermals_read) of
                    {ok, S} -> {max(S, Status), Errs};
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

handle_call(_Request, _From, State) -> {reply, notimplemented, State}.
handle_cast(_Request, State) -> {noreply, State}.

handle_info(poll, State=#state{group=Group}) ->
  {Status, IErrs} = read_all(Group),
  OErrs = write_all(Status, Group),
  Fault = fun(Err) ->
              thermals:fault(Group, Err)
          end,
  ok = lists:foreach(Fault, IErrs),
  ok = lists:foreach(Fault, OErrs),
  {noreply, State}.

init(#{group := Group, interval := Interval}) ->
  % Interval timers are linked to the calling process, so we don't need to save
  % the tref here
  {ok, _TRef} = timer:send_interval(Interval, poll),
  {ok, #state{group=Group}}.

