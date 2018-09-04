## Chained Hierarchical Injection for Fault Resiliency Evaluation (Chiffre)

[![Build Status](https://travis-ci.com/IBM/chiffre.svg?branch=master)](https://travis-ci.com/IBM/chiffre)

This provides a framework for automatically instrumenting a hardware design with run-time configurable fault injectors.
This relies on three major components:
  * A [Chisel](https://github.com/freechipsproject/chisel3) library that emits _annotations_ marking specific circuit components as fault injectable
  * New [FIRRTL](https://github.com/freechipsproject/firrtl) passes that instrument the circuit components with run-time configurable fault injectors
  * A utility for configuring fault injectors at run time

More information can be found in a CARRV 2018 workshop paper, [_Chiffre_: A Configurable Hardware Fault Injection Framework for RISC-V Systems](https://carrv.github.io/2018/papers/CARRV_2018_paper_2.pdf):

``` bibtex
@inproceedings{eldridge:2018:carrv,
  author = {Eldridge, Schuyler and Buyuktosunoglu, Alper and Bose, Pradip},
  title = {Chiffre: A Configurable Hardware Fault Injection Framework for RISC-V Systems},
  booktitle = {2nd Workshop on Computer Architecture Research with RISC-V (CARRV '18)},
  year = {2018},
}
```

### Examples

There are currently two major usage classes: standalone injection and Rocket Chip-assisted injection.

In __Standalone Injection__ you define and implement a fault injection controller, `MyController`, that manages a specific scan chain.
You then annotate specific circuit components with the `isFaulty` method indicating which scan chain they belong to and what type of fault injector to use.
An overall example of this is shown below.

``` scala
import chisel3._
import chiffre._

/* A controller for injectors on the "main" scan chain */
class MyController extends Module with ChiffreController {
  val io = IO(new Bundle{})
  lazy val scanId = "main"
  // MyController body with scan chain logic not shown
}

/* A module with faulty components */
class MyModule extends Module with ChiffreInjectee {
  val io = IO(new Bundle{})
  val x = Reg(UInt(1.W))
  val y = Reg(UInt(4.W))
  val z = Reg(UInt(8.W))
  isFaulty(x, "main", classOf[inject.LfsrInjector32])
  isFaulty(y, "main", classOf[inject.StuckAt])
  isFaulty(z, "main", classOf[inject.CycleInjector32])
}
```

In __Rocket-Chip Assisted Injection__, the fault controller is a provided Rocket Custom Coprocessor (RoCC) called [`LeChiffre`](le-chiffre/src/main/scala/chiffre/LeChiffre.scala).
This can then be used to orchestrate fault injection experiments in external components or inside Rocket itself.
We provide an example [patch](le-chiffre/patches/rocket-chip-fault-cycle.patch) that makes certain control and status registers (CSRs) in rocket fault injectable and a bare metal test program that makes sure this fault injection is working.
You can build an emulator with the correct configuration using the following (note: this will clone the `chiffre` repository inside of your Rocket Chip clone directory):

``` bash
git clone https://github.com/freechipsproject/rocket-chip $ROCKETCHIP_DIR
cd $ROCKETCHIP_DIR
git clone https://github.com/ibm/chiffre chiffre
git apply le-chiffre/patches/rocket-chip-fault-cycle.patch
cd emulator
make CONFIG=LeChiffreConfig ROCKETCHIP_ADDONS="chiffre/le-chiffre chiffre"
```

You can then run the test provided by [chiffre/tests](tests) (instructions provided in that directory).
