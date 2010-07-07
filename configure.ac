dnl Licensed under the Apache License, Version 2.0 (the "License"); you may not
dnl use this file except in compliance with the License.  dnl You may obtain a
dnl copy of the License at
dnl
dnl   http://www.apache.org/licenses/LICENSE-2.0
dnl
dnl Unless required by applicable law or agreed to in writing, software
dnl distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
dnl WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
dnl License for the specific language governing permissions and limitations
dnl under the License.

m4_include([m4/ac_check_icu.m4])

AC_INIT([LOCAL_PACKAGE_NAME], [LOCAL_VERSION], [], [LOCAL_PACKAGE_TARNAME])

AC_PREREQ([2.59])

AC_CONFIG_SRCDIR([README])
AC_CONFIG_AUX_DIR([build-aux])

AM_CONFIG_HEADER([config.h])

AM_INIT_AUTOMAKE([1.6.3 foreign])

AC_GNU_SOURCE
AC_ENABLE_SHARED
AC_DISABLE_STATIC

AC_PROG_CC
AC_PROG_LIBTOOL
AC_PROG_LN_S
AC_PROG_MAKE_SET

AC_MSG_CHECKING([for pthread_create in -lpthread])

original_LIBS="$LIBS"
LIBS="-lpthread $original_LIBS"

AC_TRY_LINK([#include<pthread.h>],
    [pthread_create((void *)0, (void *)0, (void *)0, (void *)0)],
    [pthread=yes], [pthread=no])

if test x${pthread} = xyes; then
    AC_MSG_RESULT([yes])
else
    LIBS="$original_LIBS"
    AC_MSG_RESULT([no])
fi

AC_ARG_WITH([erlang], [AC_HELP_STRING([--with-erlang=PATH],
    [set PATH to the Erlang include directory])], [
    ERLANG_FLAGS="-I$withval"
], [
    ERLANG_FLAGS="-I${libdir}/erlang/usr/include"
    ERLANG_FLAGS="$ERLANG_FLAGS -I/usr/lib/erlang/usr/include"
    ERLANG_FLAGS="$ERLANG_FLAGS -I/usr/local/lib/erlang/usr/include"
    ERLANG_FLAGS="$ERLANG_FLAGS -I/opt/local/lib/erlang/usr/include"
])

AC_ARG_VAR([ERLC_FLAGS], [general flags to prepend to ERLC_FLAGS])
AC_ARG_VAR([FLAGS], [general flags to prepend to LDFLAGS and CPPFLAGS])

LIB_FLAGS="-L/usr/local/lib -L/opt/local/lib"
LIBS="$LIB_FLAGS $LIBS"

case "$(uname -s)" in
  CYGWIN*)
    FLAGS="$LIB_FLAGS $ERLANG_FLAGS $JS_FLAGS -DXP_WIN $FLAGS"
    CPPFLAGS="$FLAGS $CPPFLAGS"
    LDFLAGS="$FLAGS $LDFLAGS"
    IS_WINDOWS="TRUE"
    # The erlang cc.sh/ld.sh scripts will convert a -O option
    # into the same optimization flags erlang itself uses.
    CFLAGS="-O2"
    LTCFLAGS="$CFLAGS"
    ;;
  *)
    FLAGS="$LIB_FLAGS $ERLANG_FLAGS $FLAGS"
    CPPFLAGS="$FLAGS $CPPFLAGS"
    # manually linking libm is requred for FreeBSD 7.0
    LDFLAGS="$FLAGS -lm $LDFLAGS"
    ;;
esac

AM_CONDITIONAL([WINDOWS], [test x$IS_WINDOWS = xTRUE])

if test x${IS_WINDOWS} = xTRUE; then
    # On windows we need to know the path to the openssl binaries.
    AC_ARG_WITH([openssl-bin-dir], [AC_HELP_STRING([--with-openssl-bin-dir=PATH],
        [path to the open ssl binaries for distribution on Windows])], [
        openssl_bin_dir=`cygpath -m "$withval"`
        AC_SUBST(openssl_bin_dir)
    ], [])

    # Windows uses Inno setup - look for its compiler.
    AC_PATH_PROG([INNO_COMPILER_EXECUTABLE], [iscc])
    if test x${INNO_COMPILER_EXECUTABLE} = x; then
        AC_MSG_WARN([You will be unable to build the Windows installer.])
    fi

    # We need the msvc redistributables for this platform too
    # (in theory we could just install the assembly locally - but
    # there are at least 4 directories with binaries, meaning 4 copies;
    # so using the redist .exe means it ends up installed globally...)
    AC_ARG_WITH([msvc-redist-dir], [AC_HELP_STRING([--with-msvc-redist-dir=PATH],
        [path to the msvc redistributables for the Windows platform])], [
        msvc_redist_dir=`cygpath -m "$withval"`
        msvc_redist_name="vcredist_x86.exe"
        AC_SUBST(msvc_redist_dir)
        AC_SUBST(msvc_redist_name)
    ], [])
    if test ! -f ${msvc_redist_dir}/${msvc_redist_name}; then
        AC_MSG_WARN([The MSVC redistributable seems to be missing; expect the installer to fail.])
    fi
fi

AC_ARG_WITH([win32-icu-binaries], [AC_HELP_STRING([--with-win32-icu-binaries=PATH],
    [set PATH to the Win32 native ICU binaries directory])], [
    ICU_CONFIG="" # supposed to be a command to query options...
    ICU_LOCAL_CFLAGS="-I$withval/include"
    ICU_LOCAL_LDFLAGS="-L$withval/lib"
    ICU_LOCAL_BIN=$withval/bin
], [
    AC_CHECK_ICU([3])
    ICU_LOCAL_CFLAGS=`$ICU_CONFIG --cppflags-searchpath`
    ICU_LOCAL_LDFLAGS=`$ICU_CONFIG --ldflags-searchpath`
    ICU_LOCAL_BIN=
])

AC_SUBST(ICU_CONFIG)
AC_SUBST(ICU_LOCAL_CFLAGS)
AC_SUBST(ICU_LOCAL_LDFLAGS)
AC_SUBST(ICU_LOCAL_BIN)

AC_ARG_WITH([win32-curl], [AC_HELP_STRING([--with-win32-curl=PATH],
    [set PATH to the Win32 native curl directory])], [
    # default build on windows is a static lib, and that's what we want too
    CURL_CFLAGS="-I$withval/include -DCURL_STATICLIB"
    CURL_LIBS="$withval/lib/libcurl"
    CURL_LDFLAGS="-l$CURL_LIBS -lWs2_32 -lkernel32 -luser32 -ladvapi32 -lWldap32"
], [
    AC_CHECK_CURL([7.18.0])
    CURL_LDFLAGS=-lcurl
])

AC_SUBST(CURL_CFLAGS)
AC_SUBST(CURL_LIBS)
AC_SUBST(CURL_LDFLAGS)

case "$(uname -s)" in
  Linux)
    LIBS="$LIBS -lcrypt"
    CPPFLAGS="-D_XOPEN_SOURCE $CPPFLAGS"
    ;;
  FreeBSD)
    LIBS="$LIBS -lcrypt"
    ;;
  OpenBSD)
    LIBS="$LIBS -lcrypto"
  ;;
esac

AC_PATH_PROG([ERL], [erl])

if test x${ERL} = x; then
    AC_MSG_ERROR([Could not find the `erl' executable. Is Erlang installed?])
fi

erlang_version_error="The installed Erlang version is less than 5.6.5 (R12B05)."

version="`${ERL} -version 2>&1 | ${SED} "s/[[^0-9]]/ /g"`"

if test `echo $version | ${AWK} "{print \\$1}"` -lt 5; then
    AC_MSG_ERROR([$erlang_version_error])
fi

if test `echo $version | ${AWK} "{print \\$2}"` -lt 6; then
    AC_MSG_ERROR([$erlang_version_error])
fi

if test `echo $version | ${AWK} "{print \\$2}"` -eq 6; then
    if test `echo $version | ${AWK} "{print \\$3}"` -lt 5; then
        AC_MSG_ERROR([$erlang_version_error])
    fi
fi

AC_PATH_PROG([ERLC], [erlc])

if test x${ERLC} = x; then
    AC_MSG_ERROR([Could not find the `erlc' executable. Is Erlang installed?])
fi

AC_CHECK_HEADER([erl_driver.h], [], [
    AC_MSG_ERROR([Could not find the `erl_driver.h' header.

Are the Erlang headers installed? Use the `--with-erlang' option to specify the
path to the Erlang include directory.])])

AC_PATH_PROG([HELP2MAN_EXECUTABLE], [help2man])
if test x${HELP2MAN_EXECUTABLE} = x; then
    AC_MSG_WARN([You will be unable to regenerate any man pages.])
fi

use_init=yes
use_launchd=yes

AC_ARG_ENABLE([init], [AC_HELP_STRING([--disable-init],
    [don't install init script where applicable])], [
    use_init=$enableval
], [])

AC_ARG_ENABLE([launchd], [AC_HELP_STRING([--disable-launchd],
    [don't install launchd configuration where applicable])], [
    use_launchd=$enableval
], [])

init_enabled=false
launchd_enabled=false

if test "$use_init" = "yes"; then
   AC_MSG_CHECKING(location of init directory)
   if test -d /etc/rc.d; then
       init_enabled=true
       AC_SUBST([initdir], ['${sysconfdir}/rc.d'])
       AC_MSG_RESULT(${initdir})
   else
       if test -d /etc/init.d; then
           init_enabled=true
           AC_SUBST([initdir], ['${sysconfdir}/init.d'])
           AC_MSG_RESULT(${initdir})
       else
           AC_MSG_RESULT(not found)
       fi
    fi
fi

if test "$use_launchd" = "yes"; then
    AC_MSG_CHECKING(location of launchd directory)
    if test -d /Library/LaunchDaemons; then
        init_enabled=false
        launchd_enabled=true
        AC_SUBST([launchddir], ['${prefix}/Library/LaunchDaemons'])
        AC_MSG_RESULT(${launchddir})
    else
        AC_MSG_RESULT(not found)
    fi
fi

AC_ARG_VAR([ERL], [path to the `erl' executable])
AC_ARG_VAR([ERLC], [path to the `erlc' executable])
AC_ARG_VAR([HELP2MAN_EXECUTABLE], [path to the `help2man' program])

if test -n "$HELP2MAN_EXECUTABLE"; then
    help2man_enabled=true
fi

AM_CONDITIONAL([INIT], [test x${init_enabled} = xtrue])
AM_CONDITIONAL([LAUNCHD], [test x${launchd_enabled} = xtrue])
AM_CONDITIONAL([HELP2MAN], [test x${help2man_enabled} = xtrue])

AC_SUBST([package_author_name], ["LOCAL_PACKAGE_AUTHOR_NAME"])
AC_SUBST([package_author_address], ["LOCAL_PACKAGE_AUTHOR_ADDRESS"])
AC_SUBST([package_identifier], ["LOCAL_PACKAGE_IDENTIFIER"])
AC_SUBST([package_tarname], ["LOCAL_PACKAGE_TARNAME"])
AC_SUBST([package_name], ["LOCAL_PACKAGE_NAME"])

AC_SUBST([version], ["LOCAL_VERSION"])
AC_SUBST([version_major], ["LOCAL_VERSION_MAJOR"])
AC_SUBST([version_minor], ["LOCAL_VERSION_MINOR"])
AC_SUBST([version_revision], ["LOCAL_VERSION_REVISION"])
AC_SUBST([version_stage], ["LOCAL_VERSION_STAGE"])
AC_SUBST([version_release], ["LOCAL_VERSION_RELEASE"])

AC_SUBST([bug_uri], ["LOCAL_BUG_URI"])

AC_SUBST([localconfdir], [${sysconfdir}/${package_identifier}])
AC_SUBST([localdatadir], [${datadir}/${package_identifier}])
AC_SUBST([localdocdir], [${datadir}/doc/${package_identifier}])
AC_SUBST([locallibdir], [${libdir}/${package_identifier}])
AC_SUBST([localstatelibdir], [${localstatedir}/lib/${package_identifier}])
AC_SUBST([localstatelogdir], [${localstatedir}/log/${package_identifier}])
AC_SUBST([localstaterundir], [${localstatedir}/run/${package_identifier}])

# On Windows we install directly into our erlang distribution.
if test x${IS_WINDOWS} = xTRUE; then
    AC_SUBST([locallibbindir], [${prefix}/bin])
    AC_SUBST([localerlanglibdir], [${libdir}])
else
    AC_SUBST([locallibbindir], [${locallibdir}/bin])
    AC_SUBST([localerlanglibdir], [${locallibdir}/erlang/lib])
fi

# fix for older autotools that don't define "abs_top_YYY" by default
AC_SUBST(abs_top_srcdir)
AC_SUBST(abs_top_builddir)

AC_REVISION([LOCAL_VERSION])

AC_CONFIG_FILES([Makefile
                 bin/Makefile
                 deps/etap/Makefile
                 deps/ibrowse/Makefile
                 deps/webmachine/Makefile
                 deps/mochiweb/Makefile
                 etc/Makefile
                 etc/default/Makefile
                 etc/init/Makefile
                 priv/Makefile
                 src/Makefile
                 test/Makefile
                 bin/pillow.tpl
                 bin/pillow.bat.tpl
                 etc/init/pillow.tpl
                 src/pillow.app.tpl
                 ])

#AC_CONFIG_FILES([bin/pillow.tpl])
#AC_CONFIG_FILES([bin/pillow.bat.tpl])
#AC_CONFIG_FILES([etc/init/pillow.tpl])
#AC_CONFIG_FILES([src/pillow.app.tpl])

AC_OUTPUT

# *sob* - on Windows libtool fails as 'libname_spec' isn't correct (it
# expects GNU style lib names).  I can't work out how to configure this
# option sanely, so we pass the script through sed to modify it.
# Also, the erlang cc.sh script doesn't cope well with the '-link' command
# line option libtool provides.
# PLEASE, someone help put this out of its misery!!
# This hackery is being tracked via PILLOW-440.
if test x${IS_WINDOWS} = xTRUE; then
    sed -e 's,libname_spec="lib\\$name",libname_spec="\\\$name",' \
        -e 's,-link,,' \
        < libtool > libtool.tmp
    mv libtool.tmp libtool
    # probably would chmod +x if we weren't on windows...
fi

echo
echo "You have configured Pillow, time to find the couch."
echo
echo "Run \`make && sudo make install' to install."
