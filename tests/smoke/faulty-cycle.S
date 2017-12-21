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

        /* Enable access to RoCCs (XS bits), the FPU (FS bits), and
	the cycle counter by user processes. RoCC access is required
	to talk to the controller (LeChiffre) and FPU CSRs and the
	cycle counter are used for fault injection tests.
        */
.macro init;
        RVTEST_XS_ENABLE
        li a0, MSTATUS_FS & (MSTATUS_FS >> 1)
        csrs mstatus, a0
        csrw mucounteren, 7
.endm

        RVTEST_CODE_BEGIN

        /* Stuck at injection into cycle counter

        Enable stuck at fault injection and test that this forces the
	observed cycle counter value to 0xdeadbeef.

        Register map:
          - x10: the observed value of the cycle counter
        */
	TEST_CASE( 1, x10, 0, LE_CHIFFRE_CYCLE(config_cycle, 10) )
	TEST_CASE( 2, x10, 0, LE_CHIFFRE_ENABLE(10) )
	TEST_CASE( 3, x10, 0xdeadbeef, csrr x10, cycle)
	TEST_CASE( 4, x10, 0, LE_CHIFFRE_ENABLE(10) )

        /* Cycle injection into frm (floating point rounding mode)

        Read original frm value. Enable cycle-based injection to flip
	all frm bits. Read the new frm value and test that the
	difference between the original and new value is the mask
	we're injecting.

        Register map:
          - x10: current frm value
          - x11: original frm value
        */
	TEST_CASE( 5, x10, 0, LE_CHIFFRE_CYCLE(config_frm, 10) )
	csrr x11, frm
	TEST_CASE( 6, x10, 0, LE_CHIFFRE_ENABLE(10) )
	nop
	nop
	csrr x10, frm
	TEST_CASE( 7, x10, 0x07, xor x10, x10, x11)
	TEST_CASE( 8, x10, 0, LE_CHIFFRE_ENABLE(10) )

        /* LFSR injection into fflags (floating point flags)

        Read original fflags value. Enable LFSR injection and
	repeatedly read fflags looking for any difference. If we read
	the same thing 16 times, then the test fails. If we ever read
	a difference, then the test passes.

        Register map:
          - x10: current fflags value
          - x11: original fflags value
          - x12: loop counter
          - x13: success flag (zero if passing)
        */
	TEST_CASE( 9, x10, 0, LE_CHIFFRE_CYCLE(config_fflags, 10) )
	csrr x11, fflags
	li x12, 16
	li x13, 1
	TEST_CASE( 10, x10, 0, LE_CHIFFRE_ENABLE(10) )
lfsr_retry:
	csrr x10, fflags
	bne x10, x11, lfsr_pass
	addi x12, x12, -1
	bnez x12, lfsr_retry
        j lfsr_fail
lfsr_pass:
        xor x13, x13, x13
lfsr_fail:
        TEST_CASE( 11, x13, 0, )
        TEST_CASE( 12, x10, 0, LE_CHIFFRE_ENABLE(10) )

        /* Test cleanup */
        TEST_PASSFAIL
        RVTEST_CODE_END

        /* Scan chain bitstreams for these tests (in faulty-cycle.h) */
        .data
        RVTEST_DATA_BEGIN
config_fflags:
        reg_fflags_LfsrInjector32
config_cycle:
        reg_cycle_stuckAt
config_frm:
        reg_frm_CycleInjector32

        RVTEST_DATA_END
