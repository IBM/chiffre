// See LICENSE.ibm for license details.

#ifndef LE_CHIFFRE_TESTS_INCLUDE_LE_CHIFFRE_H_
#define LE_CHIFFRE_TESTS_INCLUDE_LE_CHIFFRE_H_

#define f_ECHO 0
#define f_CYCLE 1
#define f_ENABLE 2
#define f_FAULT_DIFFICULTY 3
#define f_FAULT_DURATION 4
#define f_WRITE_SEED 5

#ifndef CUSTOM_X
#define CUSTOM_X 2
#endif

#define LE_CHIFFRE_ECHO(data, rd)                               \
  li x1, data;                                                  \
  ROCC_INSTRUCTION_RAW_R_R_R(CUSTOM_X, rd, 1, 0, f_ECHO);

// Send COUNT ones and SIZE-COUNT zeros through the scan chain.
#define LE_CHIFFRE_CYCLE(LABEL, rd)                             \
  la x1, LABEL;                                                 \
  ROCC_INSTRUCTION_RAW_R_R_R(CUSTOM_X, rd, 1, 0, f_CYCLE);

#define LE_CHIFFRE_ENABLE(rd)                                   \
  ROCC_INSTRUCTION_RAW_R_R_R(CUSTOM_X, rd, 0, 0, f_ENABLE);

#define LE_CHIFFRE_WRITE_DIFFICULTY(rd, LABEL)                          \
  ld a0, LABEL;                                                         \
  ROCC_INSTRUCTION_RAW_R_R_R(CUSTOM_X, rd, 0, 10, f_FAULT_DIFFICULTY);

#define LE_CHIFFRE_WRITE_DURATION(rd, LABEL)                            \
  ld a0, LABEL;                                                         \
  ROCC_INSTRUCTION_RAW_R_R_R(CUSTOM_X, rd, 0, 10, f_FAULT_DURATION);

#define LE_CHIFFRE_WRITE_SEED(rd, LABEL)                                \
  ld a0, LABEL;                                                         \
  ROCC_INSTRUCTION_RAW_R_R_R(CUSTOM_X, rd, 0, 10, f_WRITE_SEED);

#endif  // LE_CHIFFRE_TESTS_INCLUDE_LE_CHIFFRE_H_
