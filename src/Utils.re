let head = str =>
  switch (str) {
  | "" => ""
  | _ => String.sub(str, 0, 1)
  };

let tail = str =>
  switch (str) {
  | "" => ""
  | _ => String.sub(str, 1, String.length(str) - 1)
  };