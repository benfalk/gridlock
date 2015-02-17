#!/bin/bash
CUR=$( pwd )
DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
cd $DIR
rebar compile
erl -pa ebin/ deps/*/ebin -s gridlock
cd $CUR
