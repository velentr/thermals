-module(thermals_SUITE).
-author("Brian Kubisiak").

-include_lib("common_test/include/ct.hrl").

% this module acts as a gen_event handler to read thermals events
-behavior(gen_event).

-export([all/0, end_per_testcase/2, init_per_testcase/2]).
-export([check_fault/1]).
-export([handle_call/2, handle_event/2, init/1]).

all() -> [check_fault].

init_per_testcase(_, Config) ->
  {ok, _Started} = application:ensure_all_started(thermals),
  Config.

end_per_testcase(_, Config) ->
  ok = application:stop(thermals),
  Config.

check_fault(_Config) ->
  ok = thermals:watch_sup(?MODULE, self()),
  {ok, _Pid} = thermals:spawn(fault, [input, output]),
  ok = receive
         fault -> ok
       after 5000 ->
               {error, timeout}
       end.


handle_event({fault, _Group, _Reason}, Pid) ->
  Pid ! fault,
  {ok, Pid}.

handle_call(_Request, State) -> {ok, notimplemented, State}.
init(Pid) -> {ok, Pid}.
