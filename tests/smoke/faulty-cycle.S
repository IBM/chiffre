// Copyright 2017 IBM
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include "riscv_test.h"
#include "../riscv-tools/riscv-tests/isa/macros/scalar/test_macros.h"
#include "tests/rocc-software/src/xcustom.h"
#include "tests/rocc-software/src/riscv_test_rocc.h"
#include "tests/include/le-chiffre.h"
#include "tests/smoke/faulty-cycle.h"

#undef EXTRA_INIT
#define EXTRA_INIT \
  csrw mscounteren, 7; \
  csrw mucounteren, 7;

.macro init;
RVTEST_XS_ENABLE
li a0, MSTATUS_FS & (MSTATUS_FS >> 1)
csrs mstatus, a0
.endm

RVTEST_CODE_BEGIN

// Inject 0xdeadbeef into the cycle counter, ensure that we read that
TEST_CASE( 1, x3, 0, LE_CHIFFRE_CYCLE(config_cycle, 3) );
TEST_CASE( 2, x10, 0, LE_CHIFFRE_ENABLE(10) );
TEST_CASE( 3, x10, 0xdeadbeef, csrr x10, cycle;)
TEST_CASE( 4, x10, 0, LE_CHIFFRE_ENABLE(10) );

// Flip all the bits in the frm CSR
TEST_CASE( 5, x3, 0, LE_CHIFFRE_CYCLE(config_frm, 3) );
csrr x11, frm
TEST_CASE( 6, x10, 0, LE_CHIFFRE_ENABLE(10) );
csrr x10, frm
TEST_CASE( 7, x10, 0x07, xor x10, x10, x11;)
TEST_CASE( 8, x10, 0, LE_CHIFFRE_ENABLE(10) );

TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

config_instret:
reg_instret_LfsrInjector32

config_cycle:
reg_cycle_stuckAt

config_frm:
reg_frm_CycleInjector32

RVTEST_DATA_END
