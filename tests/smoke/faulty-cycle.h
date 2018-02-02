#ifndef __TESTS_SMOKE_FAULTY_CYCLE_H__
#define __TESTS_SMOKE_FAULTY_CYCLE_H__

// sbt:ScanChainConfig> run ../../../emulator/scan-chain.yaml --output-dir=../../../emulator/scan-chains --verbose --mask=0000000000000000 --stuck-at=0000000000000000 --probability=0.5 --seed=0 --cycle=0 --cycle-inject=0
#define reg_fflags_LfsrInjector32 \
.word 0x000001e3;\
.word 0x0ebaf09a;\
.word 0x00000000;\
.word 0xfffffff8;\
.word 0xb6496d1b;\
.word 0xffffffff;\
.word 0xf381ccdb;\
.word 0xfffffffd;\
.word 0xd65c99eb;\
.word 0xfffffffb;\
.word 0xc28ecea3;\
.word 0xfffffff9;\
.word 0x05a105db;\
.word 0x00000003;\
.word 0x00000000;\
.word 0x00000000;\
.word 0x00000000;\
.word 0x00000000;

// sbt:ScanChainConfig> run ../../../emulator/scan-chain.yaml --output-dir=../../../emulator/scan-chains --verbose --mask=00000000ffffffff --stuck-at=00000000deadbeef --probability=0 --seed=0 --cycle=0 --cycle-inject=0
#define reg_cycle_stuckAt \
.word 0x000001e3;\
.word 0x6ffcdda1;\
.word 0x00000000;\
.word 0x00000000;\
.word 0xb6496d18;\
.word 0x00000007;\
.word 0xf381ccd8;\
.word 0x00000005;\
.word 0xd65c99e8;\
.word 0x00000003;\
.word 0xc28ecea0;\
.word 0x00000001;\
.word 0x05a105d8;\
.word 0xf56df77b;\
.word 0x00000006;\
.word 0xfffffff8;\
.word 0x00000007;\
.word 0x00000000;

// sbt:ScanChainConfig> run ../../../emulator/scan-chain.yaml --output-dir=../../../emulator/scan-chains --verbose --mask=0000000000000000 --stuck-at=0000000000000000 --probability=0 --seed=0 --cycle=1 --cycle-inject=7
#define reg_frm_CycleInjector32 \
.word 0x000001e3;\
.word 0x131af0c7;\
.word 0x0000000f;\
.word 0x00000000;\
.word 0xb6496d18;\
.word 0x00000007;\
.word 0xf381ccd8;\
.word 0x00000005;\
.word 0xd65c99e8;\
.word 0x00000003;\
.word 0xc28ecea0;\
.word 0x00000001;\
.word 0x05a105d8;\
.word 0x00000003;\
.word 0x00000000;\
.word 0x00000000;\
.word 0x00000000;\
.word 0x00000000;

#endif  // __TESTS_SMOKE_FAULTY_CYCLE_H__
