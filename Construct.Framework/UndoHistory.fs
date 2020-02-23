namespace Construct.UndoHistoryTypes

type Model<'T> =
  { Past: 'T list
    Present: 'T
    Future: 'T list }

type ActionType =
  | Simple
  | Transactional

module UndoHistory =

  let addNew undoList model =
      { undoList with
            Past = undoList.Present :: undoList.Past
            Present = model
            Future = [] }

  let undo undoList =
      match undoList.Past with
      |[] -> undoList
      |_ -> { undoList with
                Future = undoList.Present :: undoList.Future
                Present = List.head undoList.Past
                Past = List.tail undoList.Past }


  let redo undoList =
      match undoList.Future with
      | [] -> undoList
      | _ -> { undoList with
                Past = undoList.Present :: undoList.Past
                Present = List.head undoList.Future
                Future = List.tail undoList.Future }

  let update undoList model = { undoList with Present = model }
  //let history f undoList = (f undoList.Present) :: (List.map f undoList.Past)
  let history undoList = undoList.Future @ undoList.Present :: undoList.Past
  let setPresent m undoList =  { undoList with Future = (history undoList) |> List.takeWhile (fun m' -> m' <> m) 
                                               Present = m
                                               Past = List.rev (history undoList) |> List.takeWhile (fun m' -> m' <> m) }
                                 
  let movePresentToPast undoList = {undoList with Past = undoList.Present::undoList.Past}
  
  let add undolist getUndoAction model =
      match getUndoAction (model) with
      | Simple -> addNew undolist model
      | Transactional -> update undolist model
