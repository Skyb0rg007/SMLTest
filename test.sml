
structure Test =
struct

structure T = SMLTest

val math = T.describe "math tests" [
    T.it "handles simple tests" (fn () => (
      T.assertEqualInt "1 + 1 = 2" 2 (1 + 1);
      T.assertEqualInt "2 + 1 = 1" 1 (2 + 1))),
    T.describe "multiplication" [
      T.specify "(*) is monotonic for positive arguments" (fn () => (
        T.assertBool "2 * 3 >= 2" (2 * 3 >= 2);
        T.assertBool "2 * 1 >= 2" (2 * 1 >= 2);
        T.assertBool "1 * 1 >= 1" (1 * 1 >= 1))),
      T.specify "0 is a left identity" (fn () =>
        List.app
          (fn n => T.assertEqualInt "0 * _ = _" 0 (0 * n))
          [~10, 0, 1, 2, 100]),
      T.specify "0 is a right identity" (fn () =>
        List.app (fn n => T.assertEqualInt "_ * 0 = 0" 0 (n * 0))
          [~10, 0, 1, 2, 100]),
      T.specify "zero times itself behaves correctly" (fn () =>
        T.assertEqualInt "0 * 0 = 1" 1 (0 * 0))
    ],
    T.it "returns 0 on division by 0" (fn () =>
        T.assertEqualInt "1 / 0 = 0" 0 (1 div 0))
  ]

val spec = math

fun run () =
  let
    val f = TextIO.openOut "out.tap"
  in
    T.runTAP (f, math);
    TextIO.closeOut f
  end

end
