-module(thermals_sup).
-author("Brian Kubisiak").
-behavior(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(Params) when is_map(Params) ->
  supervisor:start_link(?MODULE, Params).

init(Params) ->
  {ok, {#{strategy => one_for_one},
        [#{id => thermals_controller,
           start => {thermals_controller, start_link, [Params]},
           type => worker},
         #{id => thermals_evtmgr,
           start => {gen_event, start_link, [{local, thermals_evt}]},
           type => worker,
           modules => dynamic},
         #{id => thermals_manager,
           start => {thermals_manager, start_link, []},
           type => supervisor}]}}.

