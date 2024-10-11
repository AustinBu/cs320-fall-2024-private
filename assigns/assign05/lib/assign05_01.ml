type 'a test = 
| TestCase of 'a
| TestList of 'a test list

let rec fold_left op acc =
  function
  | TestCase x -> op acc x
  | TestList[] -> acc
  | TestList (head :: tail) ->
    let a = fold_left op acc head in
    fold_left op a (TestList tail)