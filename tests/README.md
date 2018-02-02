## Software Testing Environment for Fox
This contains a Rocket-attached suite of tests for verifying the functionality of [Fox](../fox) based off of [`riscv-tests`](https://github.com/riscv/riscv-tests).

### Organization
Tests are organized into the following categories:

* [Smoke Tests](smoke) -- Low-level verification of all of Fox's underlying instructions
* Benchmarks -- TBD

Like with `riscv-tests`, the tests are intended to be built in varieties that use physical (`-p`) or virtual memory (`-v`). Currently, only the `-p` variants are built.

### Usage

```
mkdir build
cd build
../configure --with-riscvtools=$HOME/repos/github.com/freechipsproject/rocket-chip/riscv-tools
make
```

You can then run one of these tests if you have the emulator:

```
$ROCKET_CHIP/emulator/emulator-rocketchip-FoxConfig smoke/load-store-p
```

You can run these through `spike`, but these will naturally fail as `spike` is currently Foxless.
