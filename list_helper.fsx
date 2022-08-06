let foldWithSkip<'T, 'State> folder (state: 'State) (list: 'T list) =
    match list with
    | [] -> state
    | _ ->
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt (folder)
        let mutable acc = state
        let mutable toSkip = 0
        for x in list do
            if toSkip <> 0 then
                toSkip <- toSkip - 1
            else
                let newState, skip = f.Invoke(acc, x)
                toSkip <- skip
                acc <- newState

        acc