AC_INIT(le-chiffre-tests, 1.0)

cross_compiling=yes
AC_PROG_CC

AC_ARG_WITH(xlen,
        [AS_HELP_STRING([--with-xlen=XLEN],
                [Set XLEN, the X-register bit width (default is 64)])],
        AC_SUBST(XLEN, $withval),
        AC_SUBST(XLEN, 64)
        )

AC_ARG_WITH(rocketchip,
        [AS_HELP_STRING([--with-rocketchip=ROCKETCHIP_DIR],
                [Set the path to an clone of the rocket-chip repository])],
        AC_SUBST(ROCKETCHIP_DIR, $withval),
        AC_MSG_ERROR("You must specify a path to a clone of the rocket-chip repo (--with-rocketchip)", 1)
        )

AC_OUTPUT(
    Makefile
)
