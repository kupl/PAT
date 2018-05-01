let inputfile = ref ""
let enum = ref false
let base = ref false
let profile0 = ref false
let profile1 = ref false
let profile2 = ref false
let exp = ref false
let demo = ref false

let options = 
  [
    ("-input", (Arg.String (fun s -> inputfile := s)), "provide your examples");
    ("-profile0", (Arg.Set profile0), "show states which are not pruned without profiling");
    ("-profile1", (Arg.Set profile1), "show unpruned states");
    ("-profile2", (Arg.Set profile2), "show pruned states");
    ("-enum", (Arg.Set enum), "perform enumerative search only"); (* set the reference as true *)
    ("-base", (Arg.Set base), "perform enumerative search with state normalization and without pruning");
    ("-exp", (Arg.Set exp), "provide only time and iter information");
    ("-demo", (Arg.Set demo), "only shows a resultant program")
  ]
