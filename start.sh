#!/bin/sh
# start.sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/webmachine/deps/mochiweb/ebin $PWD/webmachine/ebin -boot start_sasl -s pillow
