-module(thermals_SUITE).
-author("Brian Kubisiak").

% this module acts as a gen_event handler to read thermals events
-behavior(gen_event).

-export([all/0, end_per_testcase/2, init_per_testcase/2]).
-export([absent_hwmon_temp/1, check_fault/1]).
-export([handle_call/2, handle_event/2, init/1]).

all() -> [absent_hwmon_temp, check_fault].

init_per_testcase(_, Config) ->
  {ok, _Started} = application:ensure_all_started(thermals),
  Config.

end_per_testcase(_, Config) ->
  ok = application:stop(thermals),
  Config.

absent_hwmon_temp(_Config) ->
  ok = thermals:watch_sup(?MODULE, self()),
  {ok, _Pid} = thermals:spawn(hwmon_temp,
                              [{max, 1}, {min, 0.0},
                               {path, "imaginary_path"},
                               {index, 1}]),
  ok = wait_for_fault(5000).

check_fault(_Config) ->
  ok = thermals:watch_sup(?MODULE, self()),
  {ok, _Pid} = thermals:spawn(fault, [input, output]),
  ok = wait_for_fault(5000).

handle_event({fault, _Group, _Reason}, Pid) ->
  Pid ! fault,
  {ok, Pid}.

handle_call(_Request, State) -> {ok, notimplemented, State}.
init(Pid) -> {ok, Pid}.

wait_for_fault(Timeout) ->
  receive
    fault -> ok
  after Timeout ->
          {error, timeout}
  end.
