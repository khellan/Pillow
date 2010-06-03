#! /bin/sh -e

# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.


MY_FULL_NAME=$(readlink -f $0)
BASE_PATH=$(echo "${MY_FULL_NAME}" | sed -e 's/\/bin\/deploy\.sh$//')

PREFIX=/usr/local
DEFAULT_CONFIG_DIR="${PREFIX}/etc/pillow/default.d"
DEFAULT_CONFIG_FILE="${PREFIX}/etc/pillow/default.ini"
LOCAL_CONFIG_DIR="${PREFIX}/etc/pillow/local.d"
LOCAL_CONFIG_FILE="${PREFIX}/etc/pillow/local.ini"
PID_DIR="${PREFIX}/var/run/pillow"
CONTROL_COMMAND="${PREFIX}/bin/pillow"
DEPLOYMENT_DIR="${PREFIX}/lib/pillow/erlang/lib"

make_directories() {
    for dir in "${DEFAULT_CONFIG_DIR}" "${LOCAL_CONFIG_DIR}" "${PID_DIR}" "${DEPLOYMENT_DIR}" "${DEPLOYMENT_DIR}/pillow/priv/styles/" "${DEPLOYMENT_DIR}/pillow/ebin"
    do
        mkdir -p "${dir}"
    done
}

deploy_files() {
    source_directory=$1
    target_directory=$2

    mkdir -p "${target_directory}"
    for file in $(ls "${source_directory}")
    do
        if [ ! -d "${source_directory}/${file}" ]
        then
            cp "${source_directory}/${file}" "${target_directory}/."
        fi
    done
}

deploy_dependencies() {
    for dependency in $(ls "${BASE_PATH}/deps")
    do
        deploy_files "${BASE_PATH}/deps/${dependency}/ebin" "${DEPLOYMENT_DIR}/${dependency}/ebin"
    done
    deploy_files "${BASE_PATH}/deps/webmachine/deps/mochiweb/ebin" "${DEPLOYMENT_DIR}/webmachine/deps/mochiweb/ebin"
}

deploy_pillow() {
    for file in $(ls "${BASE_PATH}/ebin")
    do
        if [ ! -d "${BASE_PATH}/deps/${dependency}/ebin/${file}" ]
        then
            cp "${BASE_PATH}/ebin/${file}" "${DEPLOYMENT_DIR}/pillow/ebin/."
        fi
    done
}

deploy_extras() {
    cp "${BASE_PATH}/priv/styles/pillow.css" "${DEPLOYMENT_DIR}/pillow/priv/styles/pillow.css"
    cp "${BASE_PATH}/etc/pillow.ini" "${DEFAULT_CONFIG_FILE}"
    echo "; Configuration for Pillow

[pillow]
document_root = ${DEPLOYMENT_DIR}/pillow/priv/" > "${LOCAL_CONFIG_FILE}"
    SED_DEFAULT_CONFIG_FILE=$(echo ${DEFAULT_CONFIG_FILE} | sed -e 's/\//\\\//g')
    cat "${BASE_PATH}/ebin/pillow.app" | sed -e "s/\.\/etc\/pillow.ini/${SED_DEFAULT_CONFIG_FILE}/" > "${DEPLOYMENT_DIR}/pillow/ebin/pillow.app"
}

modify_start_script() {
    SED_PREFIX=$(echo ${PREFIX} | sed -e 's/\//\\\//g')
    PILLOW_LIBRARIES="${DEPLOYMENT_DIR}/pillow/ebin"
    for dependency in $(ls "${BASE_PATH}/deps")
    do
            PILLOW_LIBRARIES="${PILLOW_LIBRARIES} ${DEPLOYMENT_DIR}/${dependency}/ebin"
    done
    PILLOW_LIBRARIES="${PILLOW_LIBRARIES} ${DEPLOYMENT_DIR}/webmachine/deps/mochiweb/ebin"
    SED_PILLOW_LIBRARIES=$(echo ${PILLOW_LIBRARIES} | sed -e 's/\//\\\//g')
    cat "${BASE_PATH}/bin/pillow.sh" \
        | sed -e "s/\/usr\/local/${SED_PREFIX}/g;s/PILLOW_LIBRARIES/${SED_PILLOW_LIBRARIES}/" \
        > "${CONTROL_COMMAND}"
    chmod a+x "${CONTROL_COMMAND}"
}

deploy_daemon() {
    cp "${BASE_PATH}/etc/init/pillow" /usr/local/etc/rc.d/pillow
    chmod a+x /usr/local/etc/rc.d/pillow
    cp "${BASE_PATH}/etc/default/pillow" /usr/local/etc/default/pillow
}

make_directories
deploy_dependencies
deploy_pillow
deploy_extras
modify_start_script
deploy_daemon
