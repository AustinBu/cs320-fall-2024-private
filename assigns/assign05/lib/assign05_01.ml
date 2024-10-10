type 'a test = 
  | TestCase of 'a
  | TestList of 'a test list

let rec fold_left op acc = function
  | TestCase x -> op acc x
  | TestList [] -> acc
  | TestList (t :: ts) ->
      let a = fold_left op acc t in
      fold_left op a (TestList ts)
