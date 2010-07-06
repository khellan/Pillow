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

if WINDOWS
bin_SCRIPTS = pillow.bat
else
bin_SCRIPTS = pillow
endif

if HELP2MAN
dist_man1_MANS = pillow.1
endif

CLEANFILES = $(bin_SCRIPTS) $(dist_man1_MANS)

transform = @program_transform_name@
pillow_command_name = `echo pillow | sed '$(transform)'`

pillow: pillow.tpl
        sed -e "s|%ERL%|$(ERL)|g" \
            -e "s|%ICU_CONFIG%|$(ICU_CONFIG)|g" \
            -e "s|%bindir%|@bindir@|g" \
            -e "s|%localerlanglibdir%|@localerlanglibdir@|g" \
            -e "s|%defaultini%|default.ini|g" \
            -e "s|%localini%|local.ini|g" \
            -e "s|%localconfdir%|@localconfdir@|g" \
            -e "s|%localstatelogdir%|@localstatelogdir@|g" \
            -e "s|%localstatelibdir%|@localstatelibdir@|g" \
            -e "s|%localstatedir%|@localstatedir@|g" \
            -e "s|%bug_uri%|@bug_uri@|g" \
            -e "s|%package_author_address%|@package_author_address@|g" \
            -e "s|%package_author_name%|@package_author_name@|g" \
            -e "s|%package_name%|@package_name@|g" \
            -e "s|%version%|@version@|g" \
            -e "s|%pillow_command_name%|$(pillow_command_name)|g" > \
        $@ < $<
        chmod +x $@

pillow.bat: pillow.bat.tpl
        sed -e "s|%ICU_CONFIG%|$(ICU_CONFIG)|g" \
            -e "s|%version%|@version@|g" \
        $< > $@

HELP2MAN_OPTION=--no-info --help-option="-h" --version-option="-V"

# XXX: Because the scripts are made at build time for the user we need to
# XXX: depend on the original templates so as not to cause the rebuilding of
# XXX: the man pages.

pillow.1: pillow.tpl.in
        touch $@
        if test -x "$(HELP2MAN_EXECUTABLE)"; then \
            $(MAKE) -f Makefile pillow; \
            $(HELP2MAN_EXECUTABLE) $(HELP2MAN_OPTION) \
                --name="Pillow, the CouchDB router and rereducer" ./pillow --output $@; \
        fi
