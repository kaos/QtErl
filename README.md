QtErl
=====

QtErl is a library for building erlang applications with support for graphical
user interface using the [Qt framework](http://qt-project.org/).

QtErl is still in its infancy, exploring suitable API's and design desicions.
It is too early for real adoption, but it is excellent time to get involved
if you care to influence the future direction of the project.

Released under the apache license, version 2.0.


Current state
-------------

It's a proof of concept. I am using Qt 5.0.1 on windows, the MingW version.


Download
--------

For a quick test drive, I will post download links on https://github.com/kaos/QtErl/wiki/Downloads.


TODO
----

* Make it portable. Should be able to build and run on Windows/Linux/Mac OS (and preferably on BSD and Solaris flavours too, if those platforms are supported by Qt, that is).
* Tests. Should have unit tests.
* Documenatation, and more documentation. Examples. Tutorials.
* Get more people involved (Hey, this bullet is for YOU :p)
* Trigger widget slots.
* `[done]` Restructure into an Erlang application.


Building
========

To build QtErl, use a Gnu compatible make (or test if your preferred flavour of make works).

What you need:

- Make
    Tested with: GNU Make 3.81
- QMake
    Tested with: QMake version 3.0
    Using Qt version 5.0.1 in D:\Qt\Qt5.0.1\5.0.1\mingw47_32\lib
- Erlang (obviously)
    Tested with: Erlang R15B03 (erts-5.9.3.1) (I had issues with loading the dll on R16B).

** For now, you need to tweak `QtErl.pro` to point to where you have erlang installed. **

Simply `make all` in the project root dir. It should build QtErl.dll and beam files.
Run `werl -pa ebin` to start a new erlang window. To load the `priv/test.ui` file, you can issue `qte:t().`.

Qt need a few (ok, a lot) of binaries to be found, so make sure they are on your path.
Building in a "Qt shell" makes sure your path is setup correctly.

I have `D:\Qt\5.0.1\mingw47_32\bin` and `D:\Qt\5.0.1\Tools\MinGW\bin` on my path.

If you are not using the mingw version of Qt, you may need to override the QMAKE_SPEC variable.

Building on other systems than windows is thus far not attempted. Patches welcome! :)


API
===

A brief description of the QtErl API, as it stands right now.


Types
-----

```erlang
-type load_rsp() :: {ok, TopLevel::string()} | {error, Reason::term()}.
-type start_rsp() :: {load_rsp(), pid()} | stop.
-type connect_rsp() :: {ok, Name::string(), Signal::string()} | {error, Name::string(), Signal::string()}.
-type ui() :: #ui{} | string() | filename().
```

From `qte_xml.hrl` for compiling user interfaces to the Qt XML UI format with `qte:compile/1`.

```erlang
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
```


qte:start/0
-----------

```erlang
-spec start() -> start_rsp().
```

Load the QtErl dynamic library (.dll/.so) and start a port driver process.


qte:start/1
-----------

```erlang
-spec start(ui()) -> start_rsp().
```

Load the QtErl dynamic library (.dll/.so) and start a port driver process.
During port driver start, also loads the requested user interface.


qte:stop/1
----------

```erlang
-spec stop(pid()) -> stop.
```

Stop port driver process, closing any open windows loaded from this process.


qte:load_ui/2
-------------

```erlang
-spec load_ui(pid(), ui()) -> load_rsp().
```

Load user interface. The Ui argument is either a `#ui{}` record describing the widgets to load,
or it points to the user interface description in [Qt XML format](http://qt-project.org/doc/qt-4.8/designer-ui-file-format.html),
either as a filename or directly as string data. (see `qte:t/0` and `qte:t2/0`).


qte:load_ui/3
-------------

```erlang
-spec load_ui(pid(), ui(), Parent::string()) -> load_rsp().
```

Load user interface with given parent.

**Note** I have not yet figured out how to get the newly loaded parent widget into its correct position.
When running `qte:t3()` the loaded `centralWidget2` gets postion `0,0` thus overlapping the already existing widgets.


qte:connect/3
-------------

```erlang
-spec connect(pid(), Name::string(), Signal::string()) -> connect_rsp().
```

Connect to a widget signal. When a connected signal is emitted, it is sent as a message to the process that called `qte:connect/3`.
`Name` to be defined. It will support a path like specification to target specific widget(s) unambigously.
`Signal` should match the string you would put into the Qt `SIGNAL` macro. E.g. `"clicked()"` or `"textChanged(QString)"` etc.


qte:compile/1
-------------

```erlang
-spec compile(#ui{}) -> string().
```

Take a user interface struct and compile it to the XML format understood by the Qt form builder.
