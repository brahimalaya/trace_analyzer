# Trace Analyzer
An Erlang library for analyzing trace messages

## rec
Trace Analyzer uses [rec] [2], to get dependencies run:

`./rebar get-deps`

## Build
Trace Analyzer uses rebar as build tool. To build run:

`./rebar compile`

## Tests
Trace Analyzer uses common test. There is a shellscript called ct which should be used to run the tests, it does not run any tests in the deps folders.

`./ct`

## Contribute
If you find any issues with Trace Analyzer or have any comments please [create an issue] [1].

## License
Trace Analyzer is licensed under:
Apache License
Version 2.0, January 2004
http://www.apache.org/licenses/

[1]: https://github.com/devR2/Trace-Analyzer/issues "Trace Analyzer issues"
[2]: https://github.com/devR2/rec
