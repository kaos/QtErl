QtErl
=====

QtErl is a library for building erlang applications with support for graphical
user interface using the Qt framework.

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


API
===

A brief description of the QtErl API, as it stands right now.

Types
-----

```erlang
-type load_rsp() :: {ok, TopLevel::string()} | {error, Reason::term()}.
-type start_rsp() :: {load_rsp(), pid()} | stop.
-type connect_rsp() :: {ok, Name::string(), Signal::string()} | {error, Name::string(), Signal::string()}.
```

start/0
-------

```erlang
-spec start() -> start_rsp().
```

Load the QtErl dynamic library (.dll/.so) and start a port driver process.


start/1
-------

```erlang
-spec start(Ui::string()) -> start_rsp().
```

Load the QtErl dynamic library (.dll/.so) and start a port driver process.
During port driver start, also loads the requested user interface.


stop/1
------

```erlang
-spec stop(pid()) -> stop.
```

Stop port driver process, closing any open windows loaded from this process.


load_ui/2
---------

```erlang
-spec load_ui(pid(), Ui::string()) -> load_rsp().
```

Load user interface. The Ui argument should point to the user interface description in [Qt XML format](http://qt-project.org/doc/qt-4.8/designer-ui-file-format.html),
either as a filename or directly as string data. (see `qte:t/0` and `qte:t2/0`).


load_ui/3
---------

```erlang
-spec load_ui(pid(), Ui::string(), Parent::string()) -> load_rsp().
```

**Not yet implemented**

Load user interface with given parent.


connect/3
---------

```erlang
-spec connect(pid(), Name::string(), Signal::string()) -> connect_rsp().
```

Connect to a widget signal. When a connected signal is emitted, it is sent as a message to the process that called `qte:connect/3`.
`Name` to be defined. It will support a path like specification to target specific widget(s) unambigously.
`Signal` should match the string you would put into the Qt `SIGNAL` macro. E.g. `"clicked()"` or `"textChanged(QString)"` etc.
