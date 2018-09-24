open Utils;

/* Day 1 */
let rec findFloor = code =>
  switch (head(code)) {
  | "" => 0
  | "(" => 1 + findFloor(tail(code))
  | ")" => (-1) + findFloor(tail(code))
  | _ => 0
  };

let findFloorTailOptimised = code => {
  let rec findFloorWithAccumulator = (code, acc) =>
    switch (head(code)) {
    | "" => acc
    | "(" => findFloorWithAccumulator(tail(code), acc + 1)
    | ")" => findFloorWithAccumulator(tail(code), acc - 1)
    | _ => 0
    };
  findFloorWithAccumulator(code, 0);
};

let findFloorInd = code => {
  let rec iterIndex = ind =>
    switch (ind) {
    | (-1) => 0
    | n =>
      (
        switch (Js.String.get(code, ind)) {
        | "(" => 1
        | ")" => (-1)
        | _ => 0
        }
      )
      + iterIndex(n - 1)
    };
  iterIndex(Js.String.length(code) - 1);
};

let findPosition = code => {
  let rec findPositionWithAccumulator = (code, level, position) =>
    switch (head(code)) {
    | "" when level != (-1) => failwith("Santa is lost :(")
    | _ when level == (-1) => position
    | "(" => findPositionWithAccumulator(tail(code), level + 1, position + 1)
    | ")" => findPositionWithAccumulator(tail(code), level - 1, position + 1)
    | _ => findPositionWithAccumulator(tail(code), level, position + 1)
    };
  findPositionWithAccumulator(code, 0, 0);
};

/* Day 2 */
/* 2*l*w + 2*w*h + 2*h*l */
let findWrappingPaper = (length, width, height) =>
  [2 * length * width, 2 * width * height, 2 * height * length]
  |> List.fold_left(
       ((sum, min), cur) => (sum + cur, cur < min ? cur : min),
       (0, Js.Int.max),
     )
  |> (r => fst(r) + snd(r) / 2);