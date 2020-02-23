module IdGenerators

type Id = Id of string

let makeIdGenerator prefix =
    let mutable intId = 0
    fun () ->
        intId <- (+) intId 1
        Id(prefix <| intId)

let makeNameGenerator prefix =
    let mutable intId = 0
    fun () ->
        intId <- (+) intId 1
        (prefix <| intId)

let createSheetId = makeIdGenerator (sprintf "ID%03i")
let createSheetName = makeNameGenerator (sprintf "-%03i")
let createActionId = makeIdGenerator (sprintf"ID%03i")
let createModelDataId = makeIdGenerator (sprintf"ID%03i")