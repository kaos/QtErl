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

-module(qte).
-include("qte_xml.hrl").

%% API exports
-export([
  start/0, start/1,
  stop/1,
  load_ui/2, load_ui/3,
  connect/3,
  invoke/4,
  compile/1
]).

%% Test exports
-export([t/0, t2/0, t3/0, t4/0]).

%% Types and specs
-type load_rsp() :: {ok, TopLevel::string()} | {error, Reason::term()}.
-type start_rsp() :: {load_rsp(), pid()} | stop.
-type connect_rsp() :: {ok, Name::string(), Signal::string()} | {error, Name::string(), Signal::string()}.
-type ui() :: string() | #ui{}.

-spec start() -> start_rsp().
-spec start(ui()) -> start_rsp().
-spec stop(pid()) -> stop.
-spec load_ui(pid(), ui()) -> load_rsp().
-spec load_ui(pid(), ui(), Parent::string()) -> load_rsp().
-spec connect(pid(), Name::string(), Signal::string()) -> connect_rsp().
-spec invoke(pid(), Name::string(), Method::string(), Args::list()) -> ok | {error, Reason::term()}.
-spec compile(#ui{}) -> string().


%% ------------------------------------
%%               API
%% ------------------------------------

%% spawn new QtErl port driver
start() -> start([]).

%% spawn new QtErl port driver and load ui from file (or xml string)
start(Ui) when is_list(Ui) ->
  case erl_ddll:load(priv(), "QtErl") of
    ok -> ok;
    {error, already_loaded} -> ok;
    E -> exit({error, {qte, could_not_load, E}})
  end,
  Self = self(),
  Pid = spawn_link(fun() -> init(Self, Ui) end),
  receive
    {Pid, {start, Rsp}} -> {Rsp, Pid}
  after
    2000 -> stop(Pid)
  end;
start(Ui) when is_record(Ui, ui) ->
  start(compile(Ui)).

%% stop driver
stop(P) when is_pid(P) ->
  P ! stop.

%% load ui
load_ui(Pid, Ui) ->
  load_ui(Pid, Ui, []).

load_ui(Pid, Ui, Parent)
  when is_pid(Pid), is_list(Ui), is_list(Parent) ->
  Pid ! {self(), load_ui, {Parent, Ui}},
  receive
    {Pid, Res} -> Res
  end;
load_ui(Pid, Ui, Parent) when is_record(Ui, ui) ->
  load_ui(Pid, compile(Ui), Parent).


%% connect to signal
connect(Pid, Name, Signal)
  when is_pid(Pid), is_list(Name), is_list(Signal) ->
  Pid ! {self(), connect, {Name, Signal}},
  receive
    {Pid, Res} -> Res
  end.

%% invoke method on object
invoke(Pid, Name, Method, Args)
  when is_pid(Pid), is_list(Name), is_list(Method), is_list(Args) ->
  Pid ! {self(), invoke, {Name, Method, Args}},
  receive
    {Pid, Res} -> Res
  end.

%% compile user interface defintion to Qt XML format
compile(Ui) when is_record(Ui, ui) ->
  qte_xml:compile(Ui).


%% ------------------------------------
%%          Implementation
%% ------------------------------------

-define(QTE_LOAD_UI, 0).
-define(QTE_CONNECT, 1).
-define(QTE_INVOKE,  2).

-record(state, {
  port,
  refs=[]
}).


%% open port and enter loop
init(Pid, Filename) ->
  Port = open_port({spawn, "QtErl " ++ Filename}, []),
  receive
    {start, _}=Rsp -> Pid ! {self(), Rsp}
  after
    2000 -> skip
  end,
  loop(#state{ port=Port }).

%% main QtErl port driver loop
loop(#state{ port=Port }=State) ->
  receive
    {Pid, load_ui, What} ->
      loop(do_load_ui(Pid, What, State));
    {Pid, connect, What} ->
      loop(do_connect(Pid, What, State));
    {Pid, invoke, What} ->
      loop(do_invoke(Pid, What, State));
    stop ->
      port_close(Port);
    {'EXIT', Port, Reason} ->
      io:format("qte: port_terminated: ~p~n", [Reason]),
      exit(port_terminated);
    {Ref, Msg} when is_reference(Ref) ->
      case proplists:get_value(Ref, State#state.refs) of
        undefined ->
          io:format("qte: unknown ref ~p in message ~p~n", [Ref, Msg]);
        Pid ->
          io:format("qte: ~p ! ~p~n", [Pid, Msg]),
          Pid ! Msg
      end,
      loop(State);
    Else ->
      io:format("qte: unhandled: ~p~n", [Else]),
      loop(State)
  end.

%%
control(Port, Command, Data) ->
  Ref = make_ref(),
  case erlang:port_control(Port, Command, term_to_binary({Ref, Data})) of
    <<>> ->
      receive
        {Ref, _}=Rsp -> Rsp
      after
        1000 -> timeout
      end;
    Err ->
      io:format("qte command failed: ~p, ~p~nstack: ~p~n", [Err, Data, erlang:get_stacktrace()]),
      failed
  end.

%%
request(From, Command, Data, #state{ port=Port }=State) ->
  Reply = case control(Port, Command, Data) of
    {_Ref, Rsp} -> Rsp;
    Else -> Else
  end,
  From ! {self(), Reply},
  State.

%%
do_load_ui(Pid, What, State) ->
  request(Pid, ?QTE_LOAD_UI, What, State).

%%
do_connect(Pid, What, #state{ port=Port }=State) ->
  case control(Port, ?QTE_CONNECT, What) of
    {Ref, Rsp} ->
      Pid ! {self(), Rsp},
      %% todo: monitor pid so we can disconnect when it dies
      State#state{ refs=[{Ref, Pid}|State#state.refs] };
    Else ->
      Pid ! {self(), Else},
      State
  end.

%%
do_invoke(Pid, What, State) ->
  request(Pid, ?QTE_INVOKE, What, State).

%%
priv() -> priv("").
priv(File) ->
  filename:join([
    filename:dirname(code:which(?MODULE)),
    "..", "priv", File]).


%%%%%%%%%%
%% test %%
%%
%% Todo: eunit'ize tests.
%% Todo: move test data into separate files and load them at run time.
%%

t() ->
  {Rsp, P} = start(priv("test.ui")),
  io:format("t: start = ~p~n", [Rsp]),
  R = connect(P, "pushButton", "clicked()"),
  io:format("t: connect = ~p~n", [R]),
  P.

t2() ->
  {Rsp, P} = start(ui()),
  io:format("t: start = ~p~n", [Rsp]),
  R = connect(P, "pushButton", "clicked()"),
  io:format("t: connect = ~p~n", [R]),
  P.

t3() ->
  P = t2(),
  R = load_ui(P, extra_ui(), "centralWidget"),
  io:format("t: load_ui = ~p~n", [R]),
  P.

t4() ->
  {Rstart, P} = start(),
  io:format("t: start = ~p~n", [Rstart]),
  Rload = load_ui(P,
    #ui{
      widgets=[
        #widget{
          class="QMainWindow",
          name="MainWindow",
          properties=[
            #property{ name=geometry, attributes=[
              #attribute{ name=rect, value=[
                #attribute{ name=x, value=0 },
                #attribute{ name=y, value=0 },
                #attribute{ name=width, value=456 },
                #attribute{ name=height, value=462 }]}
              ]},
            #property{ name="windowTitle", attributes=[
              #attribute{ name=string, value="QtErl Test Window" }]}
          ],
          children=[
            #widget{ class="QWidget", name="centralWidget",
              children=[
                #widget{ class="QPushButton", name="pushButton",
                  properties=[
                    #property{ name="geometry", attributes=[
                      #attribute{ name=rect, value=[
                        #attribute{ name=x, value=150 },
                        #attribute{ name=y, value=30 },
                        #attribute{ name=width, value=75 },
                        #attribute{ name=height, value=23 }]}
                      ]},
                    #property{ name="text", attributes=[
                      #attribute{ name=string, value="Clear" }]}
                  ]
                },
                #widget{ class="QLabel", name="label",
                  properties=[
                    #property{ name="geometry", attributes=[
                      #attribute{ name=rect, value=[
                        #attribute{ name=x, value=20 },
                        #attribute{ name=y, value=10 },
                        #attribute{ name=width, value=201 },
                        #attribute{ name=height, value=16 }]}
                      ]},
                    #property{ name="text", attributes=[
                      #attribute{ name=string, value="TextLabel" }]}
                  ]
                },
                #widget{ class="QLineEdit", name="lineEdit",
                  properties=[
                    #property{ name="geometry", attributes=[
                      #attribute{ name=rect, value=[
                        #attribute{ name=x, value=20 },
                        #attribute{ name=y, value=30 },
                        #attribute{ name=width, value=113 },
                        #attribute{ name=height, value=20 }]}
                      ]}
                  ]
                } % lineEdit
              ] % centralWidget.children
            } % centralWidget
          ] % MainWindow.children
        } % MainWindow
      ],
      connections=[
        #connection{
          sender=lineEdit, signal="textChanged(QString)",
          receiver=label, slot="setText(QString)" },
        #connection{
          sender=pushButton, signal="clicked()",
          receiver=lineEdit, slot="clear()" }
      ]
    }), % widgets, ui
  io:format("t: load_ui = ~p~n", [Rload]),
  Rconnect = connect(P, "pushButton", "clicked()"),
  io:format("t: connect = ~p~n", [Rconnect]),
  P.

extra_ui() ->
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<ui version=\"4.0\">
<widget class=\"QWidget\" name=\"centralWidget2\">
<property name=\"geometry\">
 <rect>
  <x>20</x>
  <y>60</y>
  <width>421</width>
  <height>341</height>
 </rect>
</property>
<widget class=\"QPlainTextEdit\" name=\"plainTextEdit\">
 <property name=\"geometry\">
  <rect>
   <x>0</x>
   <y>30</y>
   <width>411</width>
   <height>301</height>
  </rect>
 </property>
</widget>
<widget class=\"QRadioButton\" name=\"rbTopLevel\">
 <property name=\"geometry\">
  <rect>
   <x>0</x>
   <y>0</y>
   <width>111</width>
   <height>17</height>
  </rect>
 </property>
 <property name=\"text\">
  <string>Top level widgets</string>
 </property>
</widget>
<widget class=\"QRadioButton\" name=\"rbAll\">
 <property name=\"geometry\">
  <rect>
   <x>130</x>
   <y>0</y>
   <width>121</width>
   <height>17</height>
  </rect>
 </property>
 <property name=\"text\">
  <string>All widgets</string>
 </property>
</widget>
</widget>
</ui>".

ui() ->
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<ui version=\"4.0\">
 <widget class=\"QMainWindow\" name=\"MainWindow\">
  <property name=\"geometry\">
   <rect>
    <x>0</x>
    <y>0</y>
    <width>456</width>
    <height>462</height>
   </rect>
  </property>
  <property name=\"windowTitle\">
   <string>MainWindow</string>
  </property>
  <widget class=\"QWidget\" name=\"centralWidget\">
   <widget class=\"QPushButton\" name=\"pushButton\">
    <property name=\"geometry\">
     <rect>
      <x>150</x>
      <y>30</y>
      <width>75</width>
      <height>23</height>
     </rect>
    </property>
    <property name=\"text\">
     <string>Clear</string>
    </property>
   </widget>
   <widget class=\"QLabel\" name=\"label\">
    <property name=\"geometry\">
     <rect>
      <x>20</x>
      <y>10</y>
      <width>201</width>
      <height>16</height>
     </rect>
    </property>
    <property name=\"text\">
     <string>TextLabel</string>
    </property>
   </widget>
   <widget class=\"QLineEdit\" name=\"lineEdit\">
    <property name=\"geometry\">
     <rect>
      <x>20</x>
      <y>30</y>
      <width>113</width>
      <height>20</height>
     </rect>
    </property>
   </widget>
  </widget>
  <widget class=\"QMenuBar\" name=\"menuBar\">
   <property name=\"geometry\">
    <rect>
     <x>0</x>
     <y>0</y>
     <width>456</width>
     <height>21</height>
    </rect>
   </property>
  </widget>
  <widget class=\"QToolBar\" name=\"mainToolBar\">
   <attribute name=\"toolBarArea\">
    <enum>TopToolBarArea</enum>
   </attribute>
   <attribute name=\"toolBarBreak\">
    <bool>false</bool>
   </attribute>
  </widget>
  <widget class=\"QStatusBar\" name=\"statusBar\"/>
 </widget>
 <layoutdefault spacing=\"6\" margin=\"11\"/>
 <resources/>
 <connections>
  <connection>
   <sender>lineEdit</sender>
   <signal>textChanged(QString)</signal>
   <receiver>label</receiver>
   <slot>setText(QString)</slot>
   <hints>
    <hint type=\"sourcelabel\">
     <x>76</x>
     <y>102</y>
    </hint>
    <hint type=\"destinationlabel\">
     <x>42</x>
     <y>79</y>
    </hint>
   </hints>
  </connection>
  <connection>
   <sender>pushButton</sender>
   <signal>clicked()</signal>
   <receiver>lineEdit</receiver>
   <slot>clear()</slot>
   <hints>
    <hint type=\"sourcelabel\">
     <x>187</x>
     <y>104</y>
    </hint>
    <hint type=\"destinationlabel\">
     <x>76</x>
     <y>102</y>
    </hint>
   </hints>
  </connection>
 </connections>
</ui>
".
