gridlock
========

Erlang rebuild of my bombvacum project

I want this to be a learning project where I rebuild my classic
multiplayer mindesweeper project of "bombvacum"

### Starting the madness
- rebar get-deps
- rebar compile
- the following in a terminal at the root of the project...
```
erl -pa ./ebin \
-pa ./deps/cowboy/ebin/ \
-pa ./deps/cowlib/ebin/ \
-pa ./deps/ranch/ebin/
```
- In the erlang shell `application:start(gridlock).`
