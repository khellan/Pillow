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

localconf_DATA = default.ini

EXTRA_DIST = local.ini

CLEANFILES = $(localconf_DATA) $(noinst_DATA)

transform = @program_transform_name@

if WINDOWS
default.ini: default.ini.tpl
	sed -e "s|%bindir%|.|g" \
	    -e "s|%localconfdir%|$(localconfdir)|g" \
	    -e "s|%localdatadir%|../share/pillow|g" \
	    -e "s|%localbuilddatadir%|../share/pillow|g" \
	    -e "s|%localstatelibdir%|../var/lib/pillow|g" \
	    -e "s|%localstatelogdir%|../var/log/pillow|g" \
	< $< > $@
else
default.ini: default.ini.tpl
	sed -e "s|%bindir%|$(bindir)|g" \
	    -e "s|%localconfdir%|$(localconfdir)|g" \
	    -e "s|%localdatadir%|$(localdatadir)|g" \
	        -e "s|%localbuilddatadir%|$(localdatadir)|g" \
	    -e "s|%localstatelibdir%|$(localstatelibdir)|g" \
	    -e "s|%localstatelogdir%|$(localstatelogdir)|g" \
	< $< > $@
endif

install-data-hook:
	if test ! -f "$(DESTDIR)$(localconfdir)/local.ini"; then \
	    cp $(srcdir)/local.ini "$(DESTDIR)$(localconfdir)/local.ini"; \
	fi
	if test ! "$(mkdir_p)" = ""; then \
	    $(mkdir_p) "$(DESTDIR)$(localconfdir)/default.d"; \
	    $(mkdir_p) "$(DESTDIR)$(localconfdir)/local.d"; \
	else \
	    echo "WARNING: You may have to create these directories by hand."; \
	    mkdir -p "$(DESTDIR)$(localconfdir)/default.d"; \
	    mkdir -p "$(DESTDIR)$(localconfdir)/local.d"; \
	fi

uninstall-local:
	rm -f "$(DESTDIR)/$(localconfdir)/local.ini"
