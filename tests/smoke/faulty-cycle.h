#ifndef __TESTS_SMOKE_FAULTY_CYCLE_H__
#define __TESTS_SMOKE_FAULTY_CYCLE_H__

// Conversion to .word format: ~/repos/github.com/ibm/hdl-tools/scripts/bits-to-asm scan-chains/main.bin | sed 's/$/;\\/' | xclip

// ./utils/bin/chiffre ../../freechipsproject/rocket-chip/emulator/generated-src/freechips.rocketchip.system.LeChiffreConfig/scan-chain.json --output-dir=../../freechipsproject/rocket-chip/emulator/scan-chains --verbose --mask=0000000000000000 --stuck-at=0000000000000000 --probability=0.5 --seed=0 --cycle=0 --cycle-inject=0 && hexdump -v -e '".word 0x" 1 4 "%08x" "\n"' ../../freechipsproject/rocket-chip/emulator/scan-chains/main.bin | xclip
#define reg_fflags_LfsrInjector32               \
  .word 0x000001e3;                             \
  .word 0x89eaf09a;                             \
  .word 0x00000000;                             \
  .word 0x00000000;                             \
  .word 0x00000000;                             \
  .word 0x00000000;                             \
  .word 0x00000000;                             \
  .word 0xfffffff8;                             \
  .word 0xb6496d1b;                             \
  .word 0xffffffff;                             \
  .word 0xf381ccdb;                             \
  .word 0xfffffffd;                             \
  .word 0xd65c99eb;                             \
  .word 0xfffffffb;                             \
  .word 0xc28ecea3;                             \
  .word 0xfffffff9;                             \
  .word 0x05a105db;                             \
  .word 0x00000003;

// ./utils/bin/chiffre ../../freechipsproject/rocket-chip/emulator/generated-src/freechips.rocketchip.system.LeChiffreConfig/scan-chain.json --output-dir=../../freechipsproject/rocket-chip/emulator/scan-chains --verbose --mask=00000000ffffffff --stuck-at=00000000deadbeef --probability=0 --seed=0 --cycle=0 --cycle-inject=0 && hexdump -v -e '".word 0x" 1 4 "%08x" "\n"' ../../freechipsproject/rocket-chip/emulator/scan-chains/main.bin | xclip
#define reg_cycle_stuckAt                       \
  .word 0x000001e3;                             \
  .word 0x6c70dda1;                             \
  .word 0x00000000;                             \
  .word 0xf56df778;                             \
  .word 0x00000006;                             \
  .word 0xfffffff8;                             \
  .word 0x00000007;                             \
  .word 0x00000000;                             \
  .word 0xb6496d18;                             \
  .word 0x00000007;                             \
  .word 0xf381ccd8;                             \
  .word 0x00000005;                             \
  .word 0xd65c99e8;                             \
  .word 0x00000003;                             \
  .word 0xc28ecea0;                             \
  .word 0x00000001;                             \
  .word 0x05a105d8;                             \
  .word 0x00000003;

// ./utils/bin/chiffre ../../freechipsproject/rocket-chip/emulator/generated-src/freechips.rocketchip.system.LeChiffreConfig/scan-chain.json --output-dir=../../freechipsproject/rocket-chip/emulator/scan-chains --verbose --mask=0000000000000000 --stuck-at=0000000000000000 --probability=0 --seed=0 --cycle=1 --cycle-inject=7 && hexdump -v -e '".word 0x" 1 4 "%08x" "\n"' ../../freechipsproject/rocket-chip/emulator/scan-chains/main.bin | xclip
#define reg_frm_CycleInjector32                 \
  .word 0x000001e3;                             \
  .word 0x8d5af0c7;                             \
  .word 0x0000000f;                             \
  .word 0x00000000;                             \
  .word 0x00000000;                             \
  .word 0x00000000;                             \
  .word 0x00000000;                             \
  .word 0x00000000;                             \
  .word 0xb6496d18;                             \
  .word 0x00000007;                             \
  .word 0xf381ccd8;                             \
  .word 0x00000005;                             \
  .word 0xd65c99e8;                             \
  .word 0x00000003;                             \
  .word 0xc28ecea0;                             \
  .word 0x00000001;                             \
  .word 0x05a105d8;                             \
  .word 0x00000003;

#endif  // __TESTS_SMOKE_FAULTY_CYCLE_H__
