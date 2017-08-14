# See LICENSE.IBM for license details.

#*****************************************************************************
# echo.S
#-----------------------------------------------------------------------------
#
# Test that the echo command works for Le Chiffre
#

#include "riscv_test.h"
#include "../riscv-tools/riscv-tests/isa/macros/scalar/test_macros.h"
#include "tests/rocc-software/src/xcustom.h"
#include "tests/rocc-software/src/riscv_test_rocc.h"

#include "tests/include/le-chiffre.h"

RVTEST_WITH_ROCC

RVTEST_CODE_BEGIN

  TEST_CASE( 1, x3, 0xdead, LE_CHIFFRE_ECHO(0xdead, 3) );

  TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

tdat:

RVTEST_DATA_END
