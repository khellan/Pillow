#!/bin/sh
# start.sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/webmachine/deps/mochiweb/ebin $PWD/deps/webmachine/ebin $PWD/deps/ibrowse/ebin -boot start_sasl -s pillow
