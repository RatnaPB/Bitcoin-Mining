-module(server).
-author("Admin").
-import(string,[concat/2, equal/2]).
-import(lists,[merge/1]).
-export([start/1, printstr/0,  clienthandler/1]).

%function to generate random string, it's hashes and comapare the number of leading zeroes
loop() ->
  receive
    {cal, N, Pid, EndValue} ->
      Name = concat("vsanpurkar;", randomString()),
      Hash = io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, Name))]),
      SubstringOfZeroes = string:substr(Hash, 1, N),  %create a substring of first N digits in the Hash
      ListOfZeroes = lists:duplicate(N, "0"),        %create list of number of zeroes. e.g if N = 4 then list is [0,0,0,0]
      IfZeroesMatching = equal([SubstringOfZeroes], [ListOfZeroes]),  %returns the true or false value by comparing two strings
      if
        IfZeroesMatching ->
%%          io:format("~s\t~s\n",[Name,Hash]);
            printstr ! {print, Name, Hash};
        true ->
          continue
      end,
      if
        EndValue-1 == 0 ->
          wait ! {exiting, Pid},
          exit("end");
        true ->
          Pid ! {cal, N, Pid, EndValue-1}
      end,
      loop()
  after 500 ->
    erlang:exit(kill)
  end.

%Generates the random string
randomString() ->
  lists:foldl(fun(_, Acc) -> [lists:nth(rand:uniform(36), "abcdefghijklmnopqrstuvwxyz1234567890")] ++ Acc
              end, [], lists:seq(1, 15)).

%calls the worker creation function and sends the input messages to the loop function
bossactor() ->
  receive
    {N} ->
      NUM = erlang:system_info(logical_processors_online),
      WorkerPids = create(NUM*250),
      [Pid ! {cal, N, Pid, N*1000} || Pid <- WorkerPids],
      WaitPid = spawn(fun() -> wait_for_exit(WorkerPids) end),
      register(wait, WaitPid),
      bossactor()
  after 500 ->
    erlang:exit(kill)
  end.

%creates the worker actors on the server sice
create(0) ->
%%  io:format("In last create\n"),
  PID=spawn(fun() -> loop() end),
  %PID ! {cal, N, PID, EndValue},
  [PID];

create(NUM) ->
  PID = spawn(fun() -> loop() end),
  merge([create(NUM-1), [PID]]).

%waiting till all the workers are deleted from the list
wait_for_exit([]) ->
  printer ! {printStat},
  unregister(printer),
  unregister(wait);

wait_for_exit(Pids) ->
  receive
    {exiting, Pid} ->
      NewPids = lists:delete(Pid, Pids),
      wait_for_exit(NewPids)
  end.

%print the output of server and clients
print() ->
  receive
    {printStat} ->
      io:format("mining done successfully\n"),

      {_, Time1} = statistics(runtime),   %CPU time
      CPUTime = Time1,
      {_, Time2} = statistics(wall_clock),    %Real time
      RealTime = Time2,
      io:format("CPU time: ~p\t Real Time: ~p\n",[CPUTime, RealTime]),

      Ratio = CPUTime/RealTime,
      io:format("Ratio (CPU:Real): ~p\n",[Ratio]),
      print()
  end.

%Prints the time statistics
printstr()->
  receive
    {print,OriginalString, Hash}->
      io:format("~s\t~s~n",[OriginalString, Hash]),
      printstr()
  end.

%Handles the client request
clienthandler(N) ->

  %Mainstring1 = ["varad:"]++[io_lib:format("~2.16.0B", [X]) || <<X>> <= crypto:strong_rand_bytes(18) ],

  receive
    finished ->
      io:format("ClientSide finished~n", []);

    {client,  Client_PID} ->
      Client_PID ! {clienthandler,N},
      clienthandler(N)
  end.

%Starts the client and spawns the boss actor, clienthandler, print processes
start(N) ->
  PID = spawn(fun() -> bossactor() end),

  register(clienthandler, spawn(server, clienthandler, [N])),
  register(printstr, spawn(server, printstr,[])),

  PrintPid = spawn(fun() -> print() end),     %this for printing time stats
  register(printer, PrintPid),

  PID ! {N}.


