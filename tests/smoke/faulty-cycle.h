#ifndef __TESTS_SMOKE_FAULTY_CYCLE_H__
#define __TESTS_SMOKE_FAULTY_CYCLE_H__

// sbt:ScanChainConfig> runMain scanChainConfig.Main ../../../emulator/scan-chain.yaml --output-dir=../../../emulator/scan-chains --verbose --mask=0000000000000000 --stuck-at=0000000000000000 --probability=0.5 --seed=0 --cycle=0 --cycle-inject=0
#define reg_fflags_LfsrInjector32 \
.word 0x000001e3; \
.word 0x7d253e09; \
.word 0x7fffffff; \
.word 0xf6c92da3; \
.word 0x7fffffff; \
.word 0xbe70399b; \
.word 0x7fffffff; \
.word 0x7acb933d; \
.word 0x7fffffff; \
.word 0x3851d9d4; \
.word 0x7fffffff; \
.word 0x60b420bb; \
.word 0x00000000; \
.word 0x00000000; \
.word 0x00000000; \
.word 0x00000000; \
.word 0x00000000; \
.word 0x00000000; \

// sbt:ScanChainConfig> runMain scanChainConfig.Main ../../../emulator/scan-chain.yaml --output-dir=../../../emulator/scan-chains --verbose --mask=00000000ffffffff --stuck-at=00000000deadbeef --probability=0 --seed=0 --cycle=0 --cycle-inject=0
#define reg_cycle_stuckAt \
.word 0x000001e3; \
.word 0xdcb9aafc; \
.word 0x00000000; \
.word 0xf6c92da3; \
.word 0x00000000; \
.word 0xbe70399b; \
.word 0x00000000; \
.word 0x7acb933d; \
.word 0x00000000; \
.word 0x3851d9d4; \
.word 0x00000000; \
.word 0x60b420bb; \
.word 0x00000000; \
.word 0xf56df778; \
.word 0x00000006; \
.word 0xfffffff8; \
.word 0x00000007; \
.word 0x00000000;

// sbt:ScanChainConfig> runMain scanChainConfig.Main ../../../emulator/scan-chain.yaml --output-dir=../../../emulator/scan-chains --verbose --mask=0000000000000000 --stuck-at=0000000000000000 --probability=0 --seed=0 --cycle=1 --cycle-inject=7
#define reg_frm_CycleInjector32 \
.word 0x000001e3; \
.word 0x7eabbe22; \
.word 0x00000000; \
.word 0xf6c92da3; \
.word 0x00000000; \
.word 0xbe70399b; \
.word 0x00000000; \
.word 0x7acb933d; \
.word 0x00000000; \
.word 0x3851d9d4; \
.word 0x00000000; \
.word 0x60b420bb; \
.word 0x0000000f; \
.word 0x00000000; \
.word 0x00000000; \
.word 0x00000000; \
.word 0x00000000; \
.word 0x00000000;

#endif  // __TESTS_SMOKE_FAULTY_CYCLE_H__
