# See LICENSE.ibm for license details.

#*****************************************************************************
# cycle.S
#-----------------------------------------------------------------------------
#
# Test that we can cycle through the scan chain
#

#include "riscv_test.h"
#include "include/test_macros.h"
#include "rocc-software/src/xcustom.h"
#include "include/le-chiffre.h"

RVTEST_WITH_ROCC

RVTEST_CODE_BEGIN

  TEST_CASE( 1, x3, 5844, LE_CHIFFRE_CYCLE(5844, 8, 3) );
  ;; TEST_CASE( 2, x3, 5844, LE_CHIFFRE_CHECK(5844, 3) );

  TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

tdat:

RVTEST_DATA_END
