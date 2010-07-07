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

pillowlibdir = $(localerlanglibdir)/pillow-$(version)
pillowincludedir = $(pillowlibdir)/include
pillowebindir = $(pillowlibdir)/ebin

pillowinclude_DATA = couch_db.hrl
pillowebin_DATA = $(compiled_files)

# dist_devdoc_DATA = $(doc_base) $(doc_modules)

CLEANFILES = $(compiled_files) $(doc_base)

source_files = \
    couch_config.erl \
    couch_config_writer.erl \
    couch_util.erl \
    pillow_app.erl \
    pillow_couch.erl \
    pillow.erl \
    pillow_monitor.erl \
    pillow_reducer.erl \
    pillow_router.erl \
    pillow_routing_table.erl \
    pillow_status.erl \
    pillow_sup.erl \
    pillow_util.erl \
    reducers.erl \
    static_resource.erl

EXTRA_DIST = $(source_files) couch_db.hrl

compiled_files = \
    couch_config.beam \
    couch_config_writer.beam \
    couch_util.beam \
    pillow.app \
    pillow_app.beam \
    pillow_couch.beam \
    pillow.beam \
    pillow_monitor.beam \
    pillow_reducer.beam \
    pillow_router.beam \
    pillow_routing_table.beam \
    pillow_status.beam \
    pillow_sup.beam \
    pillow_util.beam \
    reducers.beam \
    static_resource.beam


if WINDOWS
pillow.app: pillow.app.tpl
        modules=`find . -name "pillow*.erl" -exec basename {} .erl \; | tr '\n' ',' | sed "s/,$$//"`; \
        sed -e "s|%package_name%|@package_name@|g" \
            -e "s|%version%|@version@|g" \
            -e "s|@modules@|$$modules|g" \
            -e "s|%localconfdir%|../etc/pillow|g" \
            -e "s|@defaultini@|default.ini|g" \
            -e "s|@localini@|local.ini|g" > \
        $@ < $<
else
pillow.app: pillow.app.tpl
        modules=`find . -name "*.erl" -exec basename {} .erl \; | tr '\n' ',' | sed "s/,$$//"`; \
        sed -e "s|%package_name%|@package_name@|g" \
            -e "s|%version%|@version@|g" \
            -e "s|@modules@|$$modules|g" \
            -e "s|%localconfdir%|@localconfdir@|g" \
            -e "s|@defaultini@|default.ini|g" \
            -e "s|@localini@|local.ini|g" > \
        $@ < $<
        chmod +x $@
endif

%.beam: %.erl couch_db.hrl
        $(ERLC) $(ERLC_FLAGS) ${TEST} $<;
