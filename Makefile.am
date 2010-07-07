## Licensed under the Apache License, Version 2.0 (the "License"); you may not
## use this file except in compliance with the License. You may obtain a copy of
## the License at
##
##   http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
## WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
## License for the specific language governing permissions and limitations under
## the License.

SUBDIRS = bin etc src deps priv test

localdoc_DATA = \
    INSTALL.gz \
    LICENSE.gz \
    README.gz

DISTCLEANFILES = $(localdoc_DATA)

EXTRA_DIST = \
    INSTALL \
    LICENSE \
    README

INSTALL.gz: $(top_srcdir)/INSTALL
        -gzip -9 < $< > $@

LICENSE.gz: $(top_srcdir)/LICENSE
        -gzip -9 < $< > $@

README.gz: $(top_srcdir)/README
        -gzip -9 < $< > $@

check: dev
        $(top_builddir)/test/run

cover: dev
        rm -f cover/*.coverdata
        COVER=1 COVER_BIN=./src/ $(top_builddir)/test/run
        SRC=./src/ \
            $(ERL) -noshell \
                -pa deps/etap \
                -eval 'etap_report:create()' \
            -s init stop > /dev/null 2>&1
