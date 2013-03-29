-module(qte).
-export([start/1, stop/1, connect/3]).
-export([t/0, t2/0]).

-define(QTE_LOAD_UI, 0).
-define(QTE_CONNECT, 1).

-record(state, {
  port,
  refs=[]
}).

%% spawn new QtErl port driver and load ui from file
-spec(start(Filename::string()) -> pid()).
start(Filename) when is_list(Filename) ->
  case erl_ddll:load(".", "QtErl") of
    ok -> ok;
    {error, already_loaded} -> ok;
    E -> exit({error, {qte, could_not_load, E}})
  end,
  Self = self(),
  Pid = spawn_link(fun() -> init(Self, Filename) end),
  receive
    {Pid, {start, Rsp}} -> {Rsp, Pid}
  after
    2000 -> stop(Pid)
  end.

%%
stop(P) when is_pid(P) ->
  P ! stop.

%%
connect(Pid, Name, Signal)
  when is_pid(Pid), is_list(Name), is_list(Signal) ->
  Pid ! {self(), connect, {Name, Signal}},
  receive
    {Pid, Res} -> Res
  end.

%% open port and enter loop
init(Pid, Filename) ->
  Port = open_port({spawn, "QtErl " ++ Filename}, []),
  receive
    {start, _}=Rsp -> Pid ! {self(), Rsp}
  after
    2000 -> skip
  end,
  loop(#state{ port=Port }).

loop(#state{ port=Port }=State) ->
  receive
    {Pid, connect, What} ->
      loop(do_connect(Pid, What, State));
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

control(Port, Command, Data) ->
  Ref = make_ref(),
  <<>> = erlang:port_control(Port, Command, term_to_binary({Ref, Data})),
  receive
    {Ref, _}=Rsp -> Rsp
  after
    1000 -> timeout
  end.

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


%% test
t() ->
  {Rsp, P} = start("test.ui"),
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

ui() ->
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<ui version=\"4.0\">
 <class>MainWindow</class>
 <widget class=\"QMainWindow\" name=\"MainWindow_xml\">
  <property name=\"geometry\">
   <rect>
    <x>0</x>
    <y>0</y>
    <width>372</width>
    <height>297</height>
   </rect>
  </property>
  <property name=\"windowTitle\">
   <string>MainWindow (from XML)</string>
  </property>
  <widget class=\"QWidget\" name=\"centralWidget\">
   <widget class=\"QPushButton\" name=\"pushButton\">
    <property name=\"geometry\">
     <rect>
      <x>150</x>
      <y>60</y>
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
      <y>40</y>
      <width>46</width>
      <height>13</height>
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
      <y>60</y>
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
     <width>372</width>
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
</ui>".
