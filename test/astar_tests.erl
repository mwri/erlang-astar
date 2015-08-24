%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2015 Michael Wright <mjw@methodanalysis.com>
%% 
%% This file is part of the Erlang A* search library called 'astar'.
%% 
%% 'astar' is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%% 
%% @doc Eunit test suite for 'astar' app.


-module(astar_tests).


-include_lib("eunit/include/eunit.hrl").


-define(
	pathfinder_test(A, B, C),
	{"pathfinder_test",
		?_test(run_pathfinder_test(A, B, C))}).


pathfinder_1_test_() -> [
	?pathfinder_test([
		"  S  ",
		"XXXX ",
		"E    "
		], 6, []),
	?pathfinder_test([
		"  S  ",
		"XXXX ",
		"E    "
		], worklimited, [{worklimit, 4}]),
	?pathfinder_test([
		"   S                ",
		"       XXXXXXXXXXX  ",
		"   XXXXX            ",
		"                    ",
		"                    ",
		"           E        "
		], 11, []),
	?pathfinder_test([
		"S                               X   XE  ",
		"XXXXXX XXXXXXXX XXXXXXXXXXXXXXXXX X XX  ",
		"            X                     X  X  ",
		"XX XXXXXXX  XXXXXXXXXXXXXXXXXXXXXXX XX  ",
		"         X                X          X  ",
		"X XXXXX XXXXXXXXXXXXXXXX XX XXXXXXXXXX  ",
		"      X X X         X     X      X      ",
		" XXXX X X X         X XXXXXXXXXXXX  XXXX",
		"   X  X X X         X   X            X  ",
		"  XX  X X X XXXXX   XXXXX  XXXXXXXXXXX  ",
		" XX   X X X     X              X        ",
		"  X X X X X XXXXXXXXXXXXXXXXXXXX        ",
		" XX X X X X                 X           ",
		"  X X     X                 X      X    ",
		"X X XXXXXXX                             ",
		"  X                          XXXX       ",
		" XXXX   XXXXXXXXXXXXXXXX        XXX     ",
		"  X                             X       ",
		"  XXXXXXXX                              ",
		"                                        "
		], 68, []),
	?pathfinder_test([
		"                                                            ",
		" S  XXX                                                     ",
		"    X                                                       ",
		" XXXX                                                       ",
		"                                                            ",
		"                                                            ",
		"              XXXX                                          ",
		"            XXXXXXXXX                                       ",
		"         XXXXXXXX                                           ",
		"      XXXXXXXX                           XX                 ",
		"                                         XX                 ",
		"                     X                   XX                 ",
		"                    XXX                  XX                 ",
		" XXXXXXXXXXXXX      XXX                  XX                 ",
		"    XXXXXX           X                   XX                 ",
		"      XX                                 XX                 ",
		"                                         XX                 ",
		"                                         XX                 ",
		"                 XXXXXX                  XX                 ",
		"                XXXXXXXXXXXX             XXX                ",
		"                    XXXXXXX         XXXXXXXX                ",
		"                                  XXXXXXXXXXX               ",
		"                                 XXXXXXXXXXXXXX             ",
		"                               XXXXXXXXXX                   ",
		"                       XXXXXXXXXXXXXXXXXXX                  ",
		"                               XXXXXXXXXXXXXX               ",
		"                                          XXXXXXXX          ",
		"                                                            ",
		"                                                            ",
		"                                                            ",
		"                                         X                  ",
		"                                         X                  ",
		"                                        XX                  ",
		"         XXX                         XXXX                   ",
		"       XXXXXXXX                XXXXXXXX                     ",
		"     XXXXXXXXXXX      XXXXXXXXXXXXXX                        ",
		"  XXXXXXXXXXXXXXXXXXXXXXXXX                                 ",
		"      XXXXXXXXXXXXXXX                                       ",
		"                                                       X    ",
		"                                                      XX    ",
		"                                                    XXX     ",
		"               XXXXXX                           XXXXXXX     ",
		"                 XXX                XXXXXXXXXXXXXXXXX       ",
		"                               XXXXXXXXXXXXXXXXXXX          ",
		"                                                            ",
		"                                                            ",
		"                     XXXXXXXXXXXXXXX                        ",
		"                 XXXXXXX        XXXX                        ",
		"             XXXXXXX              XX                        ",
		"         XXXXXXXXXXXXXX            X            XXXXXXXXXXXX",
		"           XXXXXXXXXXXXXX                                   ",
		"                                                            ",
		"                                       XXXXXXXXXXXXX        ",
		"                                      XXXXXXXXXXXXXXXXXXXX  ",
		"                                     XXXXXXXXXXXXXXX        ",
		"                                    XXXXXXXXXXXX            ",
		"                                           XXX              ",
		"                                            X               ",
		"                                                          E ",
		"                                                            "
		], 92, []),
	?pathfinder_test([
		"   S                ",
		"       XXXXXXXXXXX  ",
		"   XXXXX  E X       ",
		"      X     X       ",
		"      XXXXXXX       ",
		"                    "
		], none, [{worklimit, 5000}])
	].


run_pathfinder_test(WorldDiagram, OptimalPathLength, SearchOptions) ->
	{_, WorldCoords} = lists:foldl(
		fun(Row, {RowN,RowData}) ->
			{_, ColData} = lists:foldl(
				fun(Col, {ColN,RowColData}) ->
					{ColN+1,[{{ColN,RowN},Col}|RowColData]}
					end,
				{1, RowData},
				Row
				),
			{RowN+1, ColData}
			end,
		{1, []},
		WorldDiagram
		),
	World = ets:new(world, [set]),
	ets:insert(World, WorldCoords),
	[StartChar, EndChar, BlockChar] = "SEX",
	{Start, End = {EndX,EndY}} = lists:foldl(
		fun	({Coord, Char}, {undefined,AccEnd}) when Char == StartChar ->
				{Coord, AccEnd};
			({Coord, Char}, {AccStart,undefined}) when Char == EndChar ->
				{AccStart, Coord};
			({_, _}, Acc) ->
				Acc
			end,
		{undefined, undefined},
		WorldCoords
		),
	MaxX = length(lists:nth(1, WorldDiagram)),
	MaxY = length(WorldDiagram),
	NeighbourFn =
		fun ({X,Y}) ->
			Neighbours = lists:filter(
				fun({_Dir, Coord}) ->
					case ets:lookup(World, Coord) of
						[{Coord,BlockChar}] -> false;
						_ -> true end
					end,
				lists:filter(
					fun({_Dir, {X2,Y2}}) ->
						X2 >= 1 andalso Y2 >= 1 andalso X2 =< MaxX andalso Y2 =< MaxY
						end,
					[{{1,0}, {X+1,Y}}, {{1,1}, {X+1,Y+1}}, {{0,1}, {X,Y+1}},
							{{-1,1}, {X-1,Y+1}}, {{-1,0}, {X-1,Y}}, {{-1,-1}, {X-1,Y-1}},
							{{0,-1}, {X,Y-1}}, {{1,-1}, {X+1,Y-1}}]
					)
				),
			Neighbours
			end,
	ScoreFn = fun(Path, {X,Y}) ->
		if {X,Y} == End ->
				max;
			true ->
				- length(Path)
				- math:sqrt(abs(X-EndX)*abs(X-EndX)+abs(Y-EndY)*abs(Y-EndY))
			end
		end,
	case astar:search(Start, NeighbourFn, ScoreFn, SearchOptions) of
		{{worklimited, BestScore}, BestPathRev, _BestState} ->
			?debugFmt("Optimal path: worklimited", []),
			?assertEqual(OptimalPathLength, worklimited);
		{BestScore, BestPathRev, _BestState}
				when (BestScore == max) or (is_integer(BestScore)) ->
			BestPath = lists:reverse(BestPathRev),
			lists:foldl(
				fun({XC,YC}, {X,Y}) ->
					NewPos = {X+XC,Y+YC},
					if NewPos == End -> true;
							true -> ets:insert(World, {NewPos, 111}) end,
					NewPos
					end,
				Start,
				BestPath
				),
			BestPathDiagram = lists:foldl(
				fun(Y, YA) ->
					[lists:foldl(
						fun(X, XA) ->
							[{{X,Y}, Contents}] = ets:lookup(World, {X,Y}),
							[Contents | XA]
							end,
						"\n",
						lists:reverse(lists:seq(1, MaxX))
						) | YA]
					end,
				[],
				lists:reverse(lists:seq(1, MaxY))
				),
			?debugFmt("Optimal path:~n~s", [BestPathDiagram]),
			?assertEqual(OptimalPathLength, length(BestPath));
		none ->
			?debugFmt("Optimal path: none", []),
			?assertEqual(OptimalPathLength, none)
		end
	.


