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

#undef EXTRA_INIT
#define EXTRA_INIT \
  csrw mscounteren, 7; \
  csrw mucounteren, 7;

RVTEST_WITH_ROCC

RVTEST_CODE_BEGIN

TEST_CASE( 1, x3, 0, LE_CHIFFRE_CYCLE(tdat3, 3) );

// Enable fault injection on the cycle counter and drop into a loop.
// If the measured cycle counter is substantially different from what
// we expect, then the test passes.
test_3setup:
        li a0, 256
        rdcycle a1
        TEST_CASE( 2, x3, 0, LE_CHIFFRE_ENABLE(3) );
1:
        addi a0, a0, -1
        bnez a0, 1b
test_3_done:
        rdcycle a2
        blt a2, a1, test_3_pass
        addi a1, a1, 1024
        bgt a2, a1, test_3_pass
test_3_fail:
        li a3, 1
        j test_3
test_3_pass:
        li a3, 0
        j test_3
        TEST_CASE( 3, a3, 0)

TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN
// ../le-chiffre/util/instrumentation/scripts/inject-scan-chain -c 64 --debug -t prng -m CSRFile -r LeChiffre --signal-regex "_T_5389" -o generated-src-debug/velvet.VelvetEmulatorConfig.v -s generated-src-debug/velvet.VelvetEmulatorConfig.scan generated-src-debug/velvet.VelvetEmulatorConfig.v.bak
// ../le-chiffre/util/instrumentation/scripts/gen-scan-bits -t prng --seed=0 -p 0.000122 --debug generated-src-debug/velvet.VelvetEmulatorConfig.scan -o - | ../xfiles-dana/util/hdl-tools/scripts/bits-to-asm | xclip
tdat3:
.word 0x00001d00
.word 0x1a8873ca
.word 0x0296b335
.word 0x0007fed2
.word 0x4e4bc23e
.word 0xb8d7f173
.word 0x0296b335
.word 0x0007fed2
.word 0xa44812cf
.word 0x3176c262
.word 0x0296b335
.word 0x0007fed2
.word 0x5b113578
.word 0x798e210b
.word 0x0296b335
.word 0x0007fed2
.word 0xda269a12
.word 0xf781b068
.word 0x0296b335
.word 0x0007fed2
.word 0x84a92929
.word 0xbfe05a4f
.word 0x0296b335
.word 0x0007fed2
.word 0x33d46252
.word 0xaf81ab1f
.word 0x0296b335
.word 0x0007fed2
.word 0x2c1b1b60
.word 0x4d5c2abf
.word 0x0296b335
.word 0x0007fed2
.word 0x00ab1fa8
.word 0xb1177359
.word 0x0296b335
.word 0x0007fed2
.word 0x0b64d331
.word 0xa0cfddeb
.word 0x0296b335
.word 0x0007fed2
.word 0xbae7175d
.word 0x09bc2369
.word 0x0296b335
.word 0x0007fed2
.word 0x74b81b45
.word 0x3121af8b
.word 0x0296b335
.word 0x0007fed2
.word 0x3458e5ab
.word 0xdd12cab1
.word 0x0296b335
.word 0x0007fed2
.word 0x875f0880
.word 0x57669c14
.word 0x0296b335
.word 0x0007fed2
.word 0x68c3c3ea
.word 0x9553dff1
.word 0x0296b335
.word 0x0007fed2
.word 0xd13b6337
.word 0xa125e8f7
.word 0x0296b335
.word 0x0007fed2
.word 0xec3f14c7
.word 0x29e88fe6
.word 0x0296b335
.word 0x0007fed2
.word 0xb679e2fe
.word 0xa9cd51f2
.word 0x0296b335
.word 0x0007fed2
.word 0x6130ddcf
.word 0xfcace0ca
.word 0x0296b335
.word 0x0007fed2
.word 0xec696bf8
.word 0xfae4b32e
.word 0x0296b335
.word 0x0007fed2
.word 0xf4683d54
.word 0x834c415d
.word 0x0296b335
.word 0x0007fed2
.word 0x81da8a56
.word 0x6b94bfc7
.word 0x0296b335
.word 0x0007fed2
.word 0xf6e83dd9
.word 0xe0818ae5
.word 0x0296b335
.word 0x0007fed2
.word 0xf59d9b7b
.word 0x9ca5371e
.word 0x0296b335
.word 0x0007fed2
.word 0x0b5c153e
.word 0x665fbfe3
.word 0x0296b335
.word 0x0007fed2
.word 0xf876a377
.word 0x5a5dd7ee
.word 0x0296b335
.word 0x0007fed2
.word 0xd849b8bb
.word 0x8a468094
.word 0x0296b335
.word 0x0007fed2
.word 0x02240ad2
.word 0x387c320a
.word 0x0296b335
.word 0x0007fed2
.word 0xe5e31157
.word 0x67557008
.word 0x0296b335
.word 0x0007fed2
.word 0x1beb6516
.word 0x7ab7159d
.word 0x0296b335
.word 0x0007fed2
.word 0xc66ce08e
.word 0xaa241e65
.word 0x0296b335
.word 0x0007fed2
.word 0xe3a7889d
.word 0x0665f701
.word 0x0296b335
.word 0x0007fed2
.word 0x5b2e0434
.word 0x160f64c7
.word 0x0296b335
.word 0x0007fed2
.word 0xb03fdfd4
.word 0x16415e21
.word 0x0296b335
.word 0x0007fed2
.word 0xa4229d48
.word 0x447d3e2b
.word 0x0296b335
.word 0x0007fed2
.word 0xc71a82c8
.word 0x90dd1725
.word 0x0296b335
.word 0x0007fed2
.word 0xb0eff6ad
.word 0x5deea61a
.word 0x0296b335
.word 0x0007fed2
.word 0xb8f25f3c
.word 0xf438b2f9
.word 0x0296b335
.word 0x0007fed2
.word 0x19bb0222
.word 0x3e12dca8
.word 0x0296b335
.word 0x0007fed2
.word 0xc5c4097f
.word 0x935c65c4
.word 0x0296b335
.word 0x0007fed2
.word 0x803fb500
.word 0x6fed289e
.word 0x0296b335
.word 0x0007fed2
.word 0x8cebc674
.word 0xb3444af3
.word 0x0296b335
.word 0x0007fed2
.word 0x8fd733c4
.word 0xd7e1fd3d
.word 0x0296b335
.word 0x0007fed2
.word 0xa3726ae9
.word 0xf30a38b8
.word 0x0296b335
.word 0x0007fed2
.word 0x6afa87d2
.word 0xad62ca50
.word 0x0296b335
.word 0x0007fed2
.word 0x171e2e7a
.word 0x1c7dc45e
.word 0x0296b335
.word 0x0007fed2
.word 0xce58d8d3
.word 0xde225acd
.word 0x0296b335
.word 0x0007fed2
.word 0x44be5b46
.word 0x81580b67
.word 0x0296b335
.word 0x0007fed2
.word 0x34738ad7
.word 0xd162be92
.word 0x0296b335
.word 0x0007fed2
.word 0x0539ea74
.word 0x5a707e4f
.word 0x0296b335
.word 0x0007fed2
.word 0xba97f536
.word 0x6379b506
.word 0x0296b335
.word 0x0007fed2
.word 0xb5a5a9bc
.word 0xcc9b03ae
.word 0x0296b335
.word 0x0007fed2
.word 0xaffc2105
.word 0x3876bdbf
.word 0x0296b335
.word 0x0007fed2
.word 0x86bb532d
.word 0x4e91aa2f
.word 0x0296b335
.word 0x0007fed2
.word 0x583023ec
.word 0x38963bfc
.word 0x0296b335
.word 0x0007fed2
.word 0x3e4550d4
.word 0x0fbadab2
.word 0x0296b335
.word 0x0007fed2
.word 0x98d7a45b
.word 0x1018a014
.word 0x0296b335
.word 0x0007fed2
.word 0x675cb539
.word 0xee1209a2
.word 0x0296b335
.word 0x0007fed2
.word 0x6f3a508a
.word 0xa1361d1d

RVTEST_DATA_END
