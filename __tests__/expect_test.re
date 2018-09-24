open Jest;
open Expect;
open Puzzles;

let description = (method, expected) => method ++ " expected=" ++ expected;

type input('a, 'b, 'c) =
  | Input('a, 'b)
  | PuzzleInput('a, 'b, 'c);

let runTest = (fn, fnName, case) =>
  switch (case) {
  | Input(input, expected) =>
    test(description(fnName, string_of_int(expected)), () =>
      expect(fn(input)) |> toBe(expected)
    )
  | PuzzleInput(input, expected, _) =>
    test(description(fnName, string_of_int(expected)), () =>
      expect(fn(input)) |> toBe(expected)
    )
  };

describe("Puzzles", () => {
  describe("Day 1 - find floor", () => {
    let testCases = [
      Input("()()", 0),
      Input("(())", 0),
      Input("(((", 3),
      Input("(()(()(", 3),
      Input("))(((((", 3),
      Input("())", -1),
      Input("))(", -1),
      Input(")))", -3),
      Input(")())())", -3),
      PuzzleInput(PuzzleInputs.day1, 232, "puzzle_input"),
    ];
    describe("findFloor (rec)", () => {
      List.iter(runTest(findFloor, "findFloor"), testCases);
      List.iter(runTest(findFloorInd, "findFloorIndTail"), testCases);
      List.iter(
        runTest(findFloorTailOptimised, "findFloorTailOptimised"),
        testCases,
      );
    });
  });
  describe("Day 1 - find position", () => {
    let testCases = [
      Input(")", 1),
      Input("()())", 5),
      PuzzleInput(PuzzleInputs.day1, 1783, "puzzle_input"),
    ];
    describe("findPosition", () =>
      List.iter(runTest(findPosition, "findPosition"), testCases)
    );
  });
  describe("Day 2 ", () => {
    let testCases = [Input((2, 3, 4), 58), Input((1, 1, 10), 43)];

    let puzzleInput =
      Array.fold_left(
        (a, (s1, s2, s3)) => Puzzles.findWrappingPaper(s1, s2, s3) + a,
        0,
        PuzzleInputs.day2,
      );

    describe("findWrappingPaper", () =>
      test("puzzleInput expected=" ++ string_of_int(1586300), () =>
        expect(puzzleInput) |> toBe(1586300)
      )
    );
    describe("findWrappingPaper", () =>
      List.iter(
        runTest(
          args => {
            let (a, b, c) = args;
            findWrappingPaper(a, b, c);
          },
          "findWrappingPaper",
        ),
        testCases,
      )
    );
  });
  describe("Day 3 ", () => {
    let testCases = [Input((2, 3, 4), 58), Input((1, 1, 10), 43)];

    describe("findRibbon", () =>
      List.iter(
        runTest(
          args => {
            let (a, b, c) = args;
            findWrappingPaper(a, b, c);
          },
          "findWrappingPaper",
        ),
        testCases,
      )
    );
  });
});