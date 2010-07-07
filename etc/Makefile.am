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

SUBDIRS = default init

if INIT
init_sysconf_data = default/pillow
init_DATA = init/pillow
endif

EXTRA_DIST = default/pillow

transform = @program_transform_name@
pillow_command_name = `echo pillow | sed '$(transform)'`

init/pillow: init/pillow.tpl
	if test "$(mkdir_p)"; then \
	    $(mkdir_p) init; \
	else \
	    if test ! -d init; then \
	        mkdir init; \
	    fi \
	fi
	sed -e "s|%configure_input%|$@. Generated from $< by configure.|" \
	    -e "s|%bindir%|$(bindir)|" \
	    -e "s|%sysconfdir%|$(sysconfdir)|" \
	    -e "s|%localstaterundir%|$(localstaterundir)|" \
	    -e "s|%pillow_command_name%|$(pillow_command_name)|" \
	< $< > $@

if WINDOWS

# README.txt has \n line endings in the repo and must have \r\n
# when installed as notepad is used to view it.
# Also: the targets below don't seem to update after an svn-up (which
# changes the version string in the generated files) so we trick make
# into always building it with the FORCE pattern...
windows/README.txt: windows/README.txt.tpl FORCE
	sed -e "s|%package_name%|@package_name@|g" \
	    -e "s|%version%|@version@|g" \
	< $< | unix2dos > $@

windows/pillow.iss: windows/pillow.iss.tpl FORCE
	sed -e "s|%configure_input%|$@. Generated from $< by configure.|" \
	    -e "s|%package_name%|@package_name@|g" \
	    -e "s|%locallibbindir%|`cygpath -m @locallibbindir@`|g" \
	    -e "s|%version%|@version@|g" \
	    -e "s|%erts_version%|`$(ERL) -version 2>&1 | $(SED) 's/[^0-9\.]//g'`|g" \
	    -e "s|%openssl_bin_dir%|@openssl_bin_dir@|g" \
	    -e "s|%msvc_redist_dir%|@msvc_redist_dir@|g" \
	    -e "s|%msvc_redist_name%|@msvc_redist_name@|g" \
	< $< > $@

# The installer depends on all files, not just the source .iss file,
# so we trick make into always building it with the FORCE pattern...
windows/setup-pillow-@version@.exe: windows/pillow.iss windows/README.txt FORCE
	"$(INNO_COMPILER_EXECUTABLE)" /q $<
	@echo Windows Installer is at: `cygpath -a -w windows/setup-pillow-@version@.exe`

windows/setup-pillow-@version@.exe.md5: windows/setup-pillow-@version@.exe
	cd windows && md5sum ./$(<F) > $(@F)

windows/setup-pillow-@version@.exe.sha: windows/setup-pillow-@version@.exe
	cd windows && sha1sum ./$(<F) > $(@F)

FORCE:

endif

install-data-hook:
	if test -n "$(init_DATA)"; then \
	    chmod +x "$(DESTDIR)$(initdir)/pillow"; \
	fi
