# Copyright 2017 IBM
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

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
TEST_CASE( 2, x3, 0, LE_CHIFFRE_ENABLE(3) );
FANN_TEST_INFER_CC(0, NUM_INPUTS, NUM_OUTPUTS, data_in, data_out)

TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN
tdat3:
CHIFFRE_BITSTREAM

DANA_TEST_DATA
DANA_ANT_DATA

RVTEST_DATA_END
