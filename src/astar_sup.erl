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
%% @doc Top supervisor module for 'astar' app.


-module(astar_sup).


-behaviour(supervisor).


-export([start_link/0]).


-export([init/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [])
	.


init([]) ->
	RestartSpec = {one_for_one, 1, 1},
	{ok, {RestartSpec, []}}
	.


