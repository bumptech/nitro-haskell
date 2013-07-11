AC_INIT([Haskell nitro package], [0.2.2.5], [libraries@haskell.org], [nitro])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([nitro.cabal])

# Header file to place defines in
AC_CONFIG_HEADERS([include/nitrocfg.h])

AC_CHECK_LIB([nitro],[sodium],[ev],[pthread])
AC_CHECK_HEADERS([nitro/nitro.h])

CPPFLAGS="$CPPFLAGS -std=gnu99 -Wall -Werror -g3 -I/usr/local/include/nitro -I/usr/local/include/sodium"

EXTRA_LIBRARIES="nitro sodium ev pthread"
#"nitro sodium ev pthread"

# Build the package if we found nitro stuff
if test "$no_nitro" = yes
then BUILD_PACKAGE_BOOL=False
else BUILD_PACKAGE_BOOL=True
fi

AC_SUBST([BUILD_PACKAGE_BOOL])
AC_SUBST([EXTRA_LIBRARIES])

AC_CONFIG_FILES([nitro.buildinfo])
AC_OUTPUT
