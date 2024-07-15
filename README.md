
# SMLTest - A testing framework for Standard ML

This library is designed to have a simple implementation for easy
integration, but allow for more additions by hiding much of the internals.
The API is mostly designed around the HSpec Haskell library, but with
much simpler types.

## Simple API

The following a stripped-down version the `SMLTest` signature:

```sml
    structure SMLTest : sig

      (* A bunch of tests that are parametrized over some input context ['ctx]
         The tests are structured in a tree, with labeled nodes corresponding
         to groups (corresponding with `group`) *)
      type 'ctx tree

      (* A [spec] is a closed set of tests *)
      type spec = unit tree

      (* Create a test that runs the given function.
         Returning successfully means the test succeeded,
         while raising an exception means the test failed.
         The ['ctx] parameter represents the "Subject Under Test".
         Most of the time, the ['ctx] parameter will be [unit],
         which is given the useful type alias [spec] *)
      val predicate : string -> ('ctx -> bool) -> 'ctx tree

      (* Combine a list of tests into a larger test with the given name *)
      val describe : string -> 'ctx tree list -> 'ctx tree
      val context : string -> 'ctx tree list -> 'ctx tree

      (* Run the tests specified by [spec],
         writing the result to the output stream in TAP format. *)
      val runTAP : TextIO.outstream * spec -> unit
      
    end
```

[TAP](https://testanything.org/), the Test Anything Protocol, is a simple
interface for external consumption of test results.
A POSIX shell-based test harness can be found [here](https://gitlab.com/esr/tapview),
and is also included in this repo for convenience.

## Example

Assume you have a math library that you want to test.
Its signature is defined below.

```sml
    structure MyMath : sig
      val add : int * int -> int
      val divide : int * int -> int option
    end
```

To test this module, define a `SMLTest.spec` in a separate file.

```sml
    structure MyMathTests : sig

      val spec : SMLTest.spec

    end = struct

      structure T = SMLTest

      val addTests = T.describe "MyMath.add" [
        T.it "has 0 as a left identity" (fn () => (
          T.assertEqualInt "add(0,10)" 10 (MyMath.add (0, 10));
          T.assertEqualInt "add(0,~10)" ~10 (MyMath.add (0, ~10)))),
        T.it "works on easy examples" (fn () =>
          T.assertEqualInt "add(2,2)" 4 (MyMath.add (2, 2)))
      ]

      (* Custom equality assertion *)
      val assertEqualIntOpt = T.assertEqualWith op =
            (fn NONE => "NONE" | SOME n => "SOME " ^ Int.toString n)

      val divideTests = T.describe "MyMath.divide" [
        T.specify "division by 0 is safe" (fn () =>
          T.assertBool "divide(10,0)" (Option.isNone (MyMath.divide (10, 0)))),
        T.describe "division rounds towards negative infinity" [
          T.it "rounds correctly with positive numbers" (fn () =>
            assertEqualIntOpt "divide(5,2)" (SOME 2) (MyMath.divide (5, 2))),
          T.it "rounds correctly with negative numbers" (fn () =>
            assertEqualIntOpt "divide(~5,2)" (SOME ~3) (MyMath.divide (~5, 2)))
        ]
      ]

      val spec = T.context "MyMath" [addTests, divideTests]

    end
```

Then to run these tests, you have a few options.

### Option 1. Write the output to a file

This is the simplest method, but doesn't support streaming.

```sml
    - val f = TextIO.openOut "out.tap";
    - SMLTest.runTAP (f, MyMathTests.spec);
    - TextIO.closeOut f;
```

```shell
    $ ./tapview < out.tap
```

### Option 2. Pipe the output into a running TAP consumer

This relies on the `Posix` or `Windows` structures from the SML basis.

```sml
    - SMLTest.runTAPHarness ("./tapview", [], MyMathTests.spec);
```

### Option 3. Write to many files, then view them all

This method is not recommended, since TAP files cannot be concatenated
while remaining valid.
However the `tapview` consumer supports this via the `-s` option.
This does remove some TAP format error handling, so
any tests that result in a process exit will not be detected.

```sml
    - val f1 = TextIO.openOut "out1.tap";
    - SMLTest.runTAP (f1, MyMathTests.spec);
    - TextIO.closeOut f1;
    - val f2 = TextIO.openOut "out2.tap";
    - SMLTest.runTAP (f2, AnotherModuleTests.spec);
    - TextIO.closeOut f2;
```

```shell
    $ cat out1.tap out2.tap | ./tapview -s
```

### Larger examples

If this is part of a larger project, you can import many `SMLTest.spec`s
from different modules into one large `spec`,
and run all the tests from one runner.

