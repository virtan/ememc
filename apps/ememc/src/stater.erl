-module(stater).
-export([
         child_spec/0,
         init/1,
         stat_event/0,
         stat_report/1,
         report/1,
         done/0
        ]).


-include_lib("vutil/include/gensrv.hrl").

child_spec() ->
   {test, {gensrv, start_link, [#gensrv{
       register = {local, ?MODULE},
       init = fun init/1,
       handle_info = fun(stat_event, _) ->
                             stat_event();
                        ({report, Data}, _) ->
                             stat_report(Data)
                     end
                                  }]},
    permanent, infinity, worker, dynamic}.

timer() ->
    {ok, Interval} = application:get_env(stat_interval),
    erlang:send_after(Interval, self(), stat_event).

init(_) ->
    erlang:process_flag(priority, max),
    file:delete("stater.dat"),
    file:delete("reports.dat"),
    file:delete("result.plt"),
    stat_event().

stat_event() ->
    timer(),
    {Mega, Secs, Usec} = now(),
    Tm = round(Mega *1000000000 + Secs *1000 + Usec/1000),
    case get(init_time) of
        undefined -> put(init_time, Tm);
        _ -> do_nothing
    end,
    Tmr = Tm - get(init_time),
    {FragWeighted, Weights} = lists:foldl(
               fun({R, B}, {S1, S2}) -> {S1 + R*B, S2 + B} end,
               {0, 0},
               [
                {proplists:get_value(mbcs_usage, D),
                 proplists:get_value(mbcs_carriers_size, D)}
                || {_, D} <- recon_alloc:fragmentation(current)]
              ),
    Fragmentation = FragWeighted/Weights,
    Mem = erlang:memory(),
    MemTotal = proplists:get_value(total, Mem),
    MemProcesses = proplists:get_value(processes, Mem),
    MemSystem = proplists:get_value(system, Mem),
    MemBinary = proplists:get_value(binary, Mem),
    MemEts = proplists:get_value(ets, Mem),
    Procs = erlang:system_info(process_count),
    MemSys = [], %% memsup:get_system_memory_data(),
    MemSysErlangTotal = proplists:get_value(total_memory, MemSys, 0),
    MemSysTotal = proplists:get_value(system_total_memory, MemSys, 0),
    MemSysErlangFree = proplists:get_value(free_memory, MemSys, 0),
    MemSysBuf = proplists:get_value(buffered_memory, MemSys, 0),
    MemSysCached = proplists:get_value(cached_memory, MemSys, 0),
    {OSMemVirt, OSMemRSS} = list_to_tuple(lists:map(
        fun(X) -> list_to_integer(X) * 1024 end,
        string:tokens(string:strip(os:cmd(
            "ps -o vsz,rss " ++ os:getpid() ++ " | tail -n 1"
        )), " \n"))),
    Line = io_lib:format("~b ~f ~b ~b ~b ~b ~b ~b ~b ~b ~b ~b ~b ~b ~b~n",
                  [Tmr, Fragmentation, MemTotal,
                   MemProcesses, MemSystem, MemBinary, MemEts,
                   Procs, MemSysErlangTotal, MemSysTotal, MemSysErlangFree, MemSysBuf,
                   MemSysCached, OSMemVirt, OSMemRSS]),
    ok = file:write_file("stater.dat", Line, [write, append]),
    ignore.

report(S) ->
    ?MODULE ! {report, S},
    ok.

stat_report(S) ->
    {Mega, Secs, Usec} = now(),
    Tm = round(Mega *1000000000 + Secs *1000 + Usec/1000),
    case get(init_time) of
        undefined -> put(init_time, Tm);
        _ -> do_nothing
    end,
    Tmr = Tm - get(init_time),
    Line = io_lib:format("~b ~ts~n", [Tmr, unicode:characters_to_binary(S)]),
    LineB = unicode:characters_to_binary(Line),
    io:format("~ts", [LineB]),
    ok = file:write_file("reports.dat", LineB, [write, append]),
    ignore.

done() ->
    ok = file:write_file("result.plt",
"
set term aqua size 1000 750 font 'Helvetica,12' dashed
#set logscale y
#set size 1,0.5
#set xrange [0:80]
set yrange [-1000:]
#set y2range [-0.1:1.2]
set xlabel 'Time (sec)'
set ylabel 'Memory (Mb)'
set y2label 'Fragmentation (used/allocated)'
set format y '%g'
set format y2 '%g'
set grid x y2
set key center bottom
set ytics nomirror
set y2tics
#set y2tics
#set ytic 20
#set label 'cpp 1bl' at 850, 25 left tc rgb 'dark-green'
#set label '1thread' at 50, 260 left tc rgb 'dark-green'
#set label '4threads' at 60, 360 left tc rgb 'blue'
#set label '8threads' at 70, 460 left tc rgb 'brown'
", [write, append]),
    %% #set label 12 "generating"   at 100,-900 rotate by 90 left
    {ok, RepFile} = file:read_file("reports.dat"),
    Reports = string:tokens(unicode:characters_to_list(RepFile), "\n"),
    ReportsMap = lists:map(fun(X) -> lists:splitwith(fun($ ) -> false; (_) -> true end, X) end, Reports),
    [file:write_file("result.plt", "set label \"" ++ Text ++ "\" at " ++ float_to_list(list_to_integer(Pos)/1000) ++ ", -900 rotate by 90 left tc rgb 'gray'\n", [write, append]) || {Pos, Text} <- ReportsMap],
    file:write_file("result.plt",
"
plot 'stater.dat' using ($1/1000):2 axes x1y2 lt 1 lc rgb 'dark-blue' with lines title 'fragmentation',\\
    'stater.dat' using ($1/1000):($3/1024/1024) axes x1y1 lt 1 lc rgb 'dark-green' with lines title 'erlang memory total used',\\
    'stater.dat' using ($1/1000):($4/1024/1024) axes x1y1 lt 4 lc rgb 'dark-green' with lines title 'erlang memory processes used',\\
    'stater.dat' using ($1/1000):($5/1024/1024) axes x1y1 lt 5 lc rgb 'dark-green' with lines title 'erlang memory system used',\\
    'stater.dat' using ($1/1000):($6/1024/1024) axes x1y1 lt 6 lc rgb 'dark-green' with lines title 'erlang memory binary used',\\
    'stater.dat' using ($1/1000):($7/1024/1024) axes x1y1 lt 7 lc rgb 'dark-green' with lines title 'erlang memory ets used',\\
    'stater.dat' using ($1/1000):8 axes x1y1 lt 1 lc rgb 'dark-yellow' with lines title 'processes',\\
    'stater.dat' using ($1/1000):($14/1024/1024) axes x1y1 lt 1 lc rgb 'dark-red' with lines title 'os memory virtual',\\
    'stater.dat' using ($1/1000):($15/1024/1024) axes x1y1 lt 2 lc rgb 'dark-red' with lines title 'os memory resident'
#    'stater.dat' using ($1/1000):($9/1024/1024) axes x1y1 lt 2 lc rgb 'dark-green' with lines title 'erlang memory total allocated',\\
#    'stater.dat' using ($1/1000):($10/1024/1024) axes x1y1 lt 3 lc rgb 'dark-green' with lines title 'erlang memory free',\\
#    'stater.dat' using ($1/1000):($11/1024/1024) axes x1y1 lt 1 lc rgb 'dark-magenta' with lines title 'os memory total',\\
#    'stater.dat' using ($1/1000):($12/1024/1024) axes x1y1 lt 2 lc rgb 'dark-magenta' with lines title 'os memory buffers',\\
#    'stater.dat' using ($1/1000):($13/1024/1024) axes x1y1 lt 3 lc rgb 'dark-magenta' with lines title 'os memory cached',\\
", [write, append]).
