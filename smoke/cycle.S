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

  TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

tdat:
  .word 0x0000189c
  .word 0x9375403a
  .word 0x00000001
  .word 0x00000000
  .word 0x33ed0592
  .word 0x976a5b01
  .word 0x358cffc7
  .word 0x18a4fe2f
  .word 0x3f3035c4
  .word 0xfa66009f
  .word 0x0000000a
  .word 0x00000000
  .word 0x799f8240
  .word 0xeba85699
  .word 0x00000295
  .word 0x00000000
  .word 0xbcbbc000
  .word 0xa79d5bd6
  .word 0xb66ee4db
  .word 0xbe5c7fcd
  .word 0xd1b75518
  .word 0x5141d8ee
  .word 0x20a3221c
  .word 0x00000000
  .word 0x80000000
  .word 0xbb35e990
  .word 0x80d29b0d
  .word 0x5403c364
  .word 0xb3ef4948
  .word 0x448e22c2
  .word 0xc1ebb5d3
  .word 0x97b6030c
  .word 0x63e5de82
  .word 0xff3fd14e
  .word 0x80a52f5b
  .word 0xc60df625
  .word 0xefcbd8e0
  .word 0x0c7e08e4
  .word 0x8ddc5fc1
  .word 0x7dddc1bc
  .word 0x5358e92e
  .word 0xc4a27260
  .word 0x3bd19291
  .word 0x10abcfc8
  .word 0xbd6401d0
  .word 0x01fc96fd
  .word 0x93270d11
  .word 0xc6bcc270
  .word 0xcd98425f
  .word 0x8e05a3bf
  .word 0xe0e4bcb9
  .word 0xe21470e7
  .word 0x39c937d6
  .word 0x451ced82
  .word 0x57ae6917
  .word 0x8f382e8f
  .word 0xc5f64c11
  .word 0x17e032f9
  .word 0x00076ce0
  .word 0x25c48637
  .word 0xdd1b46c3
  .word 0x6e83f70c
  .word 0x41d538e3
  .word 0xddcd03f8
  .word 0xd0bc330c
  .word 0xd46ebaab
  .word 0x7f4c15d6
  .word 0xdaaa6738
  .word 0xa9a12e12
  .word 0x2b0338cd
  .word 0x633e124f
  .word 0x1c18efd4
  .word 0x0829c016
  .word 0x6a5d9f2d
  .word 0x7aaa0a8e
  .word 0x6f4f0f61
  .word 0xa3e4a155
  .word 0x94eb6ceb
  .word 0x6d30e6e3
  .word 0x4e7ef73b
  .word 0xfa23fd25
  .word 0x4d1378b9
  .word 0xe302e4dc
  .word 0xcc425902
  .word 0x6da966fc
  .word 0xbbca7106
  .word 0xd0cb20a1
  .word 0x9d2271d7
  .word 0xba61bf32
  .word 0x1a420c94
  .word 0xd781a48a
  .word 0x6b231303
  .word 0x6b8df053
  .word 0x933cb3c5
  .word 0x24f60209
  .word 0xdd4b2c36
  .word 0x51184889
  .word 0x00465686
  .word 0x00000000
  .word 0x00400000
  .word 0x00000000
  .word 0x00400000
  .word 0x00000000
  .word 0x5bc00000
  .word 0x61f6282f
  .word 0xc1ad43be
  .word 0x4aee91cb
  .word 0xde68c170
  .word 0x8f3a1558
  .word 0x9f27dafe
  .word 0xd236aa03
  .word 0x890ef501
  .word 0x4ab5984c
  .word 0x14887ac4
  .word 0x1ca7c025
  .word 0xc0502cce
  .word 0xc4826611
  .word 0x75ec1de4
  .word 0x360bf51d
  .word 0xada3de5c
  .word 0x60b0df2f
  .word 0xe2ce158d
  .word 0xf461a1aa
  .word 0x85f41985
  .word 0xd4900e90
  .word 0x8014afe4
  .word 0x96052289
  .word 0x6210fd9e
  .word 0x5495a0b2
  .word 0xacb39ca9
  .word 0xe99fa116
  .word 0xa892724b
  .word 0x37d42663
  .word 0x94ab60f2
  .word 0x91c4c214
  .word 0xd1ab2b66
  .word 0xe9ad3a0a
  .word 0xdf30e605
  .word 0xe4d511ca
  .word 0xb58a35cf
  .word 0xd2ca9fbb
  .word 0x0a71e6b6
  .word 0xbcf7ce2d
  .word 0x6d22465a
  .word 0x51f346b8
  .word 0x1eab8fd4
  .word 0x1cc5e37e
  .word 0xff533833
  .word 0x9f87e97d
  .word 0x1ac0b7d5
  .word 0xc5f80150
  .word 0x316b55a2
  .word 0xa050440f
  .word 0xc005493e
  .word 0x287bfade
  .word 0xdeae313e
  .word 0xd2cfaf5b
  .word 0xa038df49
  .word 0x08a23f94
  .word 0xa40ce2c4
  .word 0x5b08a2be
  .word 0xa7e319e1
  .word 0xfe01fc6f
  .word 0x32a715c5
  .word 0x07ae92ea
  .word 0xd01f4aef
  .word 0x833f1534
  .word 0x33e93b85
  .word 0x199f3aac
  .word 0x1c632ecd
  .word 0x44afb554
  .word 0xe6197393
  .word 0x9f54c331
  .word 0x7409ffcc
  .word 0x8f180936
  .word 0xf2fb2fc0
  .word 0xd887ebb0
  .word 0x208080bd
  .word 0xd2c491f7
  .word 0x9b031a1e
  .word 0x057c96c2
  .word 0xba86a090
  .word 0xbbbe54d3
  .word 0x23207083
  .word 0x62177576
  .word 0x6e9477f7
  .word 0xf9d45fc9
  .word 0x3545a256
  .word 0x428d83d0
  .word 0x5e73c618
  .word 0x092918a5
  .word 0x3a2ef29e
  .word 0xbf3d341c
  .word 0x415117b2
  .word 0xebe8c1e8
  .word 0x894b7bca
  .word 0x12131db8
  .word 0x00000000
  .word 0x00000000
RVTEST_DATA_END
