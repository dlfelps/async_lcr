-module(async_lcr).
-export([disconnected/0]).


connected(Neighbor, Hub) ->
  receive
  {vote, UID} -> 
    if 
      UID > self() -> Neighbor ! {vote, UID};
      UID =:= self() -> Hub ! {leader_found}; % leader status
      true -> true
    end;    
  {Real_hub, start} -> 
    Neighbor ! {vote, self()},
    connected(Neighbor, Real_hub);
  true -> true
  end,
  connected(Neighbor, Hub).


disconnected() ->
  receive
  {connect, Neighbor} -> connected(Neighbor, true)
  end.


