## Software Testing Environment for LeChiffre

### Usage

```bash
autoconf
mkdir build
cd build
../configure --with-rocketchip=$HOME/repos/github.com/freechipsproject/rocket-chip
make
```

You can then run one of these tests if you have a built version of the `LeChiffreConfig` Rocket Chip emulator:

```bash
./emulator-freechips.rocketchip.system-LeChiffreConfig ../chiffre/tests/build/smoke/le-chiffre-p-faulty-cycle
```
