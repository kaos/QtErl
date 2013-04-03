%%%
%% Copyright 2013 Andreas Stenius
%%
%%  Licensed under the Apache License, Version 2.0 (the "License");
%%  you may not use this file except in compliance with the License.
%%  You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%%  Unless required by applicable law or agreed to in writing, software
%%  distributed under the License is distributed on an "AS IS" BASIS,
%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%  See the License for the specific language governing permissions and
%%  limitations under the License.
%%

-record(attribute, {
  name :: string() | atom(),
  value :: [#attribute{}] | string() | number() | atom()
}).

-record(property, {
  name :: string() | atom(),
  attributes=[] :: [#attribute{}]
}).

-record(widget, {
  class :: string() | atom(),
  name :: string() | atom(),
  properties=[] :: [#property{}],
  children=[] :: [#widget{}]
}).

-record(connection, {
  sender :: string() | atom(),
  signal :: string(),
  receiver :: string() | atom(),
  slot :: string()
}).

-record(ui, {
  version="4.0" :: string(),
  widgets=[] :: [#widget{}],
  resources=[] :: list(),   %% not yet specified nor tested
  connections=[] :: [#connection{}]
}).
