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

RVTEST_WITH_ROCC

RVTEST_CODE_BEGIN

// Test 1: Cycle in the data from `tdat3` below. This will internally
// compute a Fletcher32 checksum and compare it to `tdat3_checksum`,
// returning zero if everything matches.
  TEST_CASE( 1, x3, 0, LE_CHIFFRE_CYCLE(tdat3, 3) );
// Test 2: Enable the cycle counter to go off. This returns a zero.
  TEST_CASE( 2, x3, 0, LE_CHIFFRE_ENABLE(3) );

// Test 3: Setup a watchdog timer accumulating loop iterations (x1)
// that will cause a jump to a failure state in 32 (x2) loop
// iterations. We can break out of this loop to success by using
// Chiffre to change the accumulator to a value greater than 1024
// (x3).
test_3_setup:
  mv x1, x0;                // x1 = 0
  addi x2, x0, 32;          // x2 = 32
  addi x3, x0, 1024;        // x3 = 1024
test_3_loop:
  addi x1, x1, 1;           // x1++ <-- Inject fault into x1 write back
  nop;                      // nop to avoid x1 forwarding
  bltu x3, x1, test_3_pass; // if (x1 >= 1024) { goto pass; }
  bltu x2, x1, test_3_fail; // if (x1 >= 32)   { goto fail; }
  j test_3_loop;            // goto loop
test_3_pass:
  li x4, 0;
  j test_3;
test_3_fail:
  li x4, 1;
  j test_3;
  TEST_CASE( 3, x4, 0 );

// Repeat the tests except use a bit flip pattern of all zeros that
// will allow the watchdog timer to go off.
  TEST_CASE( 4, x3, 0, LE_CHIFFRE_CYCLE(tdat4, 3) );
  TEST_CASE( 5, x3, 0, LE_CHIFFRE_ENABLE(3) );
test_6_setup:
  mv x1, x0;                // x1 = 0
  addi x2, x0, 32;          // x2 = 32
  addi x3, x0, 1024;        // x3 = 1024
test_6_loop:
  addi x1, x1, 1;           // x1++ <-- Inject fault into x1 write back
  nop;                      // nop to avoid x1 forwarding
  bltu x3, x1, test_6_fail; // if (x1 >= 1024) { goto pass; }
  bltu x2, x1, test_6_pass; // if (x1 >= 32)   { goto fail; }
  j test_6_loop;            // goto loop
test_6_pass:
  li x4, 0;
  j test_6;
test_6_fail:
  li x4, 1;
  j test_6;
  TEST_CASE( 6, x4, 0 );

  TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

// Scan chain that will flip all the bits of the register file writeback
// at cycle 800.
tdat3:
.word 0x00000200
.word 0xa85e15d5
.word 0x0000031f
.word 0x00000000
.word 0x0000031f
.word 0x00000000
.word 0x0000031f
.word 0x00000000
.word 0x0000031f
.word 0x00000000
.word 0x0000031f
.word 0x00000000
.word 0x0000031f
.word 0x00000000
.word 0xffffffff
.word 0xffffffff
.word 0x0000031f
.word 0x00000000

// Scan chain that will flip no bits at cycle 66:
//   util/fault-injection/scripts/gen-scan-bits -c 66 -p 0 ../emulator/generated-src/rocketchip.LeChiffreConfig.scan -o - | util/fault-injection/scripts/bits-to-asm
tdat4:
.word 0x00000200
.word 0xa87815d9
.word 0x0000031f
.word 0x00000000
.word 0x0000031f
.word 0x00000000
.word 0x0000031f
.word 0x00000000
.word 0x0000031f
.word 0x00000000
.word 0x0000031f
.word 0x00000000
.word 0x0000031f
.word 0x00000000
.word 0x00000000
.word 0x00000000
.word 0x0000031f
.word 0x00000000

RVTEST_DATA_END
