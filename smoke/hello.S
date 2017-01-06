# See LICENSE.ibm for license details.

#*****************************************************************************
# load-store.S
#-----------------------------------------------------------------------------
#
# Test that we can do loads and stores to/from Fox.
#

#include "riscv_test.h"
#include "include/test_macros.h"
#include "rocc-software/src/xcustom.h"
#include "rocc-software/src/riscv_test_rocc.h"
#include "include/le-chiffre.h"

RVTEST_WITH_ROCC

RVTEST_CODE_BEGIN

  TEST_CASE( 1, x0, 0x0, LE_CHIFFRE_ECHO(0xdead, 3) );

  TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

tdat:

RVTEST_DATA_END
