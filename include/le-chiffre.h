// See LICENSE.ibm for license details.

#ifndef LE_CHIFFRE_TESTS_INCLUDE_LE_CHIFFRE_H_
#define LE_CHIFFRE_TESTS_INCLUDE_LE_CHIFFRE_H_

#define f_ECHO 0
#define f_CYCLE 1
#define f_CHECK 2

#ifndef CUSTOM_X
#define CUSTOM_X 0
#endif

#define LE_CHIFFRE_ECHO(data, rd)                               \
  li x1, data;                                                  \
  ROCC_INSTRUCTION_RAW_R_R_R(CUSTOM_X, rd, 1, 0, f_ECHO);

// Send COUNT ones and SIZE-COUNT zeros through the scan chain.
#define LE_CHIFFRE_CYCLE(LABEL, rd)                             \
  la x1, LABEL;                                                 \
  ROCC_INSTRUCTION_RAW_R_R_R(CUSTOM_X, rd, 1, 0, f_CYCLE);

#define LE_CHIFFRE_CHECK(checksum, rd)                          \
  li x1, checksum;                                              \
  ROCC_INSTRUCTION_RAW_R_R_R(CUSTOM_X, rd, 1, 0, f_CHECK);

#endif  // LE_CHIFFRE_TESTS_INCLUDE_LE_CHIFFRE_H_
