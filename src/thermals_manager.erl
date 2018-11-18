-module(thermals_manager).
-author("Brian Kubisiak").
-behavior(supervisor).
-export([start_child/2, start_link/0, start_link/2]).
-export([init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Type, Params) ->
  supervisor:start_child(?MODULE, [Type, Params]).

start_link(Type, Params) ->
  case Type of
    fault ->
      thermals_class_fault:start_link(Params);
    _ ->
      {error, unrecognized}
  end.

init([]) ->
  {ok, {#{strategy => simple_one_for_one},
        [#{id => thermals_class,
           start => {thermals_manager, start_link, []},
           type => worker}]}}.
