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

#ifndef LE_CHIFFRE_TESTS_INCLUDE_LE_CHIFFRE_H_
#define LE_CHIFFRE_TESTS_INCLUDE_LE_CHIFFRE_H_

#define f_ECHO 0
#define f_CYCLE 1
#define f_ENABLE 2

#ifndef CUSTOM_X
#define CUSTOM_X 2
#endif

#define LE_CHIFFRE_ECHO(data, rd)                           \
  li x10, data;                                             \
  ROCC_INSTRUCTION_RAW_R_R_R(CUSTOM_X, rd, 10, 0, f_ECHO);

// Send COUNT ones and SIZE-COUNT zeros through the scan chain.
#define LE_CHIFFRE_CYCLE(LABEL, rd)                         \
  la x10, LABEL;                                            \
  ROCC_INSTRUCTION_RAW_R_R_R(CUSTOM_X, rd, 10, 0, f_CYCLE);

#define LE_CHIFFRE_ENABLE(rd)                               \
  ROCC_INSTRUCTION_RAW_R_R_R(CUSTOM_X, rd, 0, 0, f_ENABLE);

#endif  // LE_CHIFFRE_TESTS_INCLUDE_LE_CHIFFRE_H_
