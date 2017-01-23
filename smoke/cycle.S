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
#include "rocc-software/src/riscv_test_rocc.h"
#include "include/le-chiffre.h"

RVTEST_WITH_ROCC

RVTEST_CODE_BEGIN

  TEST_CASE( 1, x3, 0, LE_CHIFFRE_CYCLE(tdat, 3) );
  TEST_CASE( 2, x3, 0, LE_CHIFFRE_ENABLE(3) );

  TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

tdat:
.word 0x000001e8
.word 0x30de092d
.word 0x00000008
.word 0x00000000
.word 0x00000008
.word 0x00000000
.word 0x00000008
.word 0x00000000
.word 0x00000008
.word 0x00000000
.word 0x00000008
.word 0x00000000
.word 0x00000008
.word 0x00000000
.word 0xffffffff
.word 0x000008ff
.word 0x00000000
.word 0x00000000
RVTEST_DATA_END
