-module(client_side).
-import(string,[substr/3]).
-import(lists, [merge/1]).
-import(string,[concat/2, equal/2]).
-export([send_request/1, bossActor/1]).

%compares and generates hashes of random strings and send sends it to server to print
clientMining() ->                   %added one more parameter for the number of zeroes
  receive
    {cal, N, Pid, EndValue, Server_Node} ->
      Name = concat("r.bhairagond;", randomString()),
      Hash = io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, Name))]),
      SubstringOfZeroes = string:substr(Hash, 1, N),  %create a substring of first N digits in the Hash
      ListOfZeroes = lists:duplicate(N, "0"),        %create list of number of zeroes. e.g if N = 4 then list is [0,0,0,0]
      IfZeroesMatching = equal([SubstringOfZeroes], [ListOfZeroes]),  %returns the true or false value by comparing two strings
      if
        IfZeroesMatching ->
          {printstr, Server_Node} ! {print,Name,Hash};
        true ->
          continue
      end,
      if
        EndValue-1 == 0 ->
          waiter ! {exiting, Pid},
          exit("end");
        true ->
          Pid ! {cal, N, Pid, EndValue-1, Server_Node}
      end,
      clientMining()
  after 2000 ->
    erlang:exit(kill)
  end.

%"the end", executes when all the worker actors exited. base case of wait_for_exit_client(Pids)
wait_for_exit_client([]) ->
  io:format("Client side mining Done~n");

%removes exited worker pid from the list of worker pids
wait_for_exit_client(Pids) ->
  receive
    {exiting, Pid} ->
      NewPids = lists:delete(Pid, Pids),
      wait_for_exit_client(NewPids)
  end.

%generate random string of length 15
randomString() ->
  lists:foldl(fun(_, Acc) -> [lists:nth(rand:uniform(36), "abcdefghijklmnopqrstuvwxyz1234567890")] ++ Acc
              end, [], lists:seq(1, 15)).

%message passing and creation of worker actors
bossActor(Server_Node) ->
  {clienthandler, Server_Node} ! {client,self()},
  receive
    {clienthandler, N} ->
      NumOfCoresClient = erlang:system_info(logical_processors_online),
      WorkerPidsClient = createClientWorker(NumOfCoresClient * 250),
      [Pid ! {cal, N, Pid, N*1000, Server_Node} || Pid <- WorkerPidsClient],
      WaitPids = spawn(fun() -> wait_for_exit_client(WorkerPidsClient) end),
      register(waiter, WaitPids),
      bossActor(Server_Node)
  end.

%base case of createClientWorker(NUM)
createClientWorker(0) ->
  PID=spawn(fun() -> clientMining() end),
  [PID];

%spawns worker actors
createClientWorker(NUM) ->
  PID = spawn(fun() -> clientMining() end),
  merge([createClientWorker(NUM-1), [PID]]).

%spawns boss actor
send_request(Server_Node) ->
  spawn(fun() -> bossActor(Server_Node) end).