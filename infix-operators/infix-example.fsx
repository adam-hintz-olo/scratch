// Valid characters for making your own operator: !%&*+-./<=>?@^|~
// Enclose in parens to show that it's an infix operator
let (<+>) x y = x + y

printfn "Adder: %d" (3 <+> 5)

// Note: the parameters are implicit here
let (<@>) = sprintf "%d%d"

printfn "Int concatenator: %s" (2 <@> 5)

// You can also take an infix operator and call it as prefix with parens
printfn "Prefix: %d" ((/) 10 2)
