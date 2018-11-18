-module(thermals).
-author("Brian Kubisiak").
-behavior(application).
-export([fault/2, join_inputs/0, join_inputs/1, join_inputs/2, join_outputs/0,
         join_outputs/1, join_outputs/2, spawn/2, watch/2, watch_sup/2]).
-export([start/2, stop/1]).

-define(DEFAULT_GROUP, global_thermals).

fault(Group, Reason) ->
  ok = gen_event:notify(thermals_evt, {fault, Group, Reason}).

join_inputs(Group, Pid) ->
  pg2:join({Group, inputs}, Pid).
join_inputs(Group) ->
  join_inputs(Group, self()).
join_inputs() ->
  join_inputs(?DEFAULT_GROUP).

join_outputs(Group, Pid) ->
  pg2:join({Group, outputs}, Pid).
join_outputs(Group) ->
  join_outputs(Group, self()).
join_outputs() ->
  join_outputs(?DEFAULT_GROUP).

spawn(Type, Args) ->
  thermals_manager:start_child(Type, Args).

watch(Handler, Args) ->
  gen_event:add_handler(thermals_evt, Handler, Args).
watch_sup(Handler, Args) ->
  gen_event:add_sup_handler(thermals_evt, Handler, Args).

start(_, _) ->
  Group = application:get_env(?MODULE, group, ?DEFAULT_GROUP),
  Interval = application:get_env(?MODULE, interval, 1000),
  ok = pg2:create({Group, outputs}),
  ok = pg2:create({Group, inputs}),
  thermals_sup:start_link(#{group => Group, interval => Interval}).

stop([]) -> ok.

