## Licensed under the Apache License, Version 2.0 (the "License"); you may not
## use this file except in compliance with the License.  You may obtain a copy
## of the License at
##
##   http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
## WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
## License for the specific language governing permissions and limitations under
## the License.

webmachineebindir = $(localerlanglibdir)/webmachine/ebin

webmachine_file_collection = \
    webmachine.app.in \
    webmachine_app.erl \
    webmachine_decision_core.erl \
    webmachine_deps.erl \
    webmachine_dispatcher.erl \
    webmachine.erl \
    webmachine_error_handler.erl \
    webmachine_logger.erl \
    webmachine_mochiweb.erl \
    webmachine_multipart.erl \
    webmachine_perf_logger.erl \
    webmachine_request.erl \
    webmachine_resource.erl \
    webmachine_skel.erl \
    webmachine_sup.erl \
    webmachine_util.erl \
    wmtrace_resource.erl \
    wrq.erl

webmachineebin_make_generated_file_list = \
    webmachine.app \
    webmachine_app.beam \
    webmachine_decision_core.beam \
    webmachine_deps.beam \
    webmachine_dispatcher.beam \
    webmachine.beam \
    webmachine_error_handler.beam \
    webmachine_logger.beam \
    webmachine_mochiweb.beam \
    webmachine_multipart.beam \
    webmachine_perf_logger.beam \
    webmachine_request.beam \
    webmachine_resource.beam \
    webmachine_skel.beam \
    webmachine_sup.beam \
    webmachine_util.beam \
    wmtrace_resource.beam \
    wrq.beam

webmachineebin_DATA = $(webmachineebin_make_generated_file_list)

EXTRA_DIST =  $(webmachine_file_collection) \
    webmachine.hrl \
    webmachine_logger.hrl \
    wm_reqdata.hrl \
    wm_reqstate.hrl

CLEANFILES = $(webmachineebin_make_generated_file_list)

%.app: %.app.in
	cp $< $@


%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) $<
