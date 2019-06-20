let assocs xs x =
  let xs' = List.filter (fun (a,_) -> a=x) xs
  in List.map (fun (_,b) -> b) xs'
