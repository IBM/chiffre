// See LICENSE.IBM for license details.

#include "riscv_test.h"
#include "../riscv-tools/riscv-tests/isa/macros/scalar/test_macros.h"
#include "tests/rocc-software/src/xcustom.h"
#include "tests/rocc-software/src/riscv_test_rocc.h"

#include "tests/include/le-chiffre.h"
#include "tests/libs/src/xfiles-supervisor.S"
#include "tests/libs/src/xfiles-user.S"

#define STACK_TOP (_end + 4096)

#undef EXTRA_INIT
#define EXTRA_INIT                              \
  SET_ASID(1);                                  \
  SET_ANTP(antp_dana, antp_os);                 \
  la sp, _end + 1024;                           \
  csrw mscounteren, 7;                          \
  csrw mucounteren, 7;

RVTEST_WITH_ROCC

RVTEST_CODE_BEGIN

TEST_CASE( 1, x3, 0, LE_CHIFFRE_CYCLE(tdat3, 3) );
TEST_CASE( 2, x0, 0, LE_CHIFFRE_WRITE_DIFFICULTY(3, chiffre_difficulty));
TEST_CASE( 3, x0, 0, LE_CHIFFRE_WRITE_DURATION(3, chiffre_duration));
TEST_CASE( 4, x0, 0, LE_CHIFFRE_WRITE_SEED(3, chiffre_seed));
TEST_CASE( 5, x3, 0, LE_CHIFFRE_ENABLE(3) );
FANN_TEST_NO_COMPARE_CC(0, NUM_INPUTS, NUM_OUTPUTS, NUM_DATAPOINTS, data_in, data_out);

TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN
CHIFFRE_CONFIG
tdat3:
CHIFFRE_BITSTREAM

DANA_TEST_DATA
DANA_ANT_DATA

RVTEST_DATA_END
