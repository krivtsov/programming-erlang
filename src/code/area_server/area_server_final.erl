-module(area_server_final).
-export([start/0, area/2]).

start() -> spawn(fun loop/0).

area(Pid, What) -> rpc(Pid, What).

rpc(Pid, Request) -> 
    Pid ! {self(), Request},
    receive
      {Pid, Response} ->
        Response
    end.

loop() -> 
  receive
    {From, {rectangle, Width, Ht}} ->
      From ! {self(), Width * Ht},
      loop();
    {From, {circle, R}} -> 
      From ! {self(), 3.14159 * R * R},
      loop();
    {From, Other} ->
      From ! {self(), {error, Other}},
      loop()
    end.

% 2> c(area_server_final).
% {ok,area_server_final}
% 4> Pid = area_server_final:start().
% <0.85.0>
% 6> area_server_final:area(Pid, {rectangle, 10,3}).
% 30
% 7> area_server_final:area(Pid, {circle,3}).
% 28.274309999999996
% 8> area_server_final:area(Pid, {circl,3}).
% {error,{circl,3}}

