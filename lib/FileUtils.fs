namespace lib

module FileUtils =
    let getFileName (argv: string array) =
        if argv.Length <> 0 then
            argv.[0]
        else
            "data/source.txt"
