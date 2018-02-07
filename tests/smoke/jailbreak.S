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
#include "riscv-tests/isa/macros/scalar/test_macros.h"
#include "rocc-software/src/xcustom.h"
#include "rocc-software/src/riscv_test_rocc.h"
#include "include/le-chiffre.h"

#undef EXTRA_INIT
#define EXTRA_INIT \
  csrw mscounteren, 7; \
  csrw mucounteren, 7;

RVTEST_WITH_ROCC

RVTEST_CODE_BEGIN

jail:

TEST_CASE( 1, x0, 0, nop)

TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

RVTEST_DATA_END
