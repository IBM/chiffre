// See LICENSE.ibm for license details.

#ifndef LE_CHIFFRE_TESTS_INCLUDE_LE_CHIFFRE_H_
#define LE_CHIFFRE_TESTS_INCLUDE_LE_CHIFFRE_H_

#define ADDR_SM 0x00000000
#define ADDR_IM 0x60000000
#define ADDR_DM 0x80000000

#ifndef CUSTOM_X
#define CUSTOM_X 0
#endif

#define LE_CHIFFRE_ECHO(data, rd)                       \
  li x1, data;                                          \
  ROCC_INSTRUCTION_RAW_R_R_R(CUSTOM_X, rd, 1, 0, 0);

#endif  // LE_CHIFFRE_TESTS_INCLUDE_LE_CHIFFRE_H_
