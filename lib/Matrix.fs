module Matrix

type Matrix<'a> = 'a list list

let tryFindIndex f (m: Matrix<'a>) =
    m
    |> List.indexed
    |> List.choose (fun (idxX, l) ->
        match List.tryFindIndex f l with
        | Some idxY -> Some (idxX, idxY)
        | None -> None
    )
    |> List.tryHead

let fold (f: 'state -> 'a -> 'state) (state: 'state) (m: Matrix<'a>) =
    let mutable s = state
    for row in m do
        for el in row do
            s <- f s el
    s
