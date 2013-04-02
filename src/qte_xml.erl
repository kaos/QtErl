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

-module(qte_xml).
-include("qte_xml.hrl").

%% API exports
-export([compile/1]).

-spec compile(Ui::#ui{}) -> string().


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% ------------------------------------
%%               API
%% ------------------------------------

%% compile a user interface description to XML data
compile(#ui{ version=Ver, widgets=Widgets }) ->
  lists:flatten([
    xml_header(Ver),
    compile_widgets(Widgets),
    "</ui>"
  ]).


%% ------------------------------------
%%          Implementation
%% ------------------------------------

xml_header(Ver) ->
  io_lib:format("<?xml version=\"1.0\" encoding=\"UTF-8\"?><ui version=\"~s\">", [Ver]).

compile_widgets(Ws)     -> [compile_widget(W)     || W <- Ws].
compile_properties(Ps)  -> [compile_property(P)   || P <- Ps].
compile_attributes(As)  -> [compile_attribute(A)  || A <- As].

compile_widget(#widget{ class=Class, name=Name }=W) ->
  [io_lib:format("<widget class=\"~s\" name=\"~s\">", [Class, Name]),
    compile_properties(W#widget.properties),
    compile_widgets(W#widget.children),
    "</widget>"].

compile_property(#property{ name=Name, attributes=Attrs }) ->
  [io_lib:format("<property name=\"~s\">", [Name]),
    compile_attributes(Attrs),
    "</property>"].

compile_attribute(#attribute{ name=Name, value=Value }) ->
  [io_lib:format("<~s>", [Name]),
    compile_attribute_value(Value),
    io_lib:format("</~s>", [Name])].

compile_attribute_value([#attribute{}|_]=Attrs) ->
  compile_attributes(Attrs);
compile_attribute_value(Value) when is_number(Value) ->
  io_lib:format("~b", [Value]);
compile_attribute_value(Value) when is_list(Value); is_atom(Value) ->
  io_lib:format("~s", [Value]).


-ifdef(TEST).
%% ------------------------------------
%%          Tests
%% ------------------------------------

compile_test() ->
  ?assertEqual(lists:flatten([
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      "<ui version=\"4.0\">",
        "<widget class=\"QMainWindow\" name=\"MainWindow\">",
          "<property name=\"geometry\">",
            "<rect><x>0</x><y>0</y><width>456</width><height>462</height></rect>",
          "</property>",
          "<property name=\"windowTitle\"><string>MainWindow</string></property>",
        "</widget>",
      "</ui>"]),
    compile(#ui{ widgets=[
        #widget{ class="QMainWindow", name="MainWindow", properties=[
            #property{ name=geometry, attributes=[
                #attribute{ name=rect, value=[
                  #attribute{ name=x, value=0 },
                  #attribute{ name=y, value=0 },
                  #attribute{ name=width, value=456 },
                  #attribute{ name=height, value=462 }
                ]}
              ]},
            #property{ name="windowTitle", attributes=[
                #attribute{ name=string, value="MainWindow" }
              ]}
          ]}
      ]})
  ).

-endif.
