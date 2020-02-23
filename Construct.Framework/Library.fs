namespace Construct.Framework

module App =
    open Elmish
    open Elmish.WPF
    open IdGenerators
    open Construct.UndoHistoryTypes

    type UndoEntry = UndoEntry of string

    type ModelData =
        { Id : Id
          Sheets: Sheet list
          SelectedSheetId: Id option
          UndoEntry: UndoEntry
          CurrentUndoMode: ActionType
          }
    and Sheet =
        { Id: Id
          Name: string
          Content: string
          IsOpen : bool}

    type Msg =
        | CreatePoint
        | CreateSheet
        | SetSelectedSheet of Id: Id option
        | Reset
        | RemoveSheet of Id: Id
        | CloseSheet of Id: Id
        | OpenSheet of Id: Id
        | Undo
        | Redo
        | StartUndoTransaction of string
        | EndUndoTransaction
        | SetModelData of Id : Id


    let initModelData =
        { 
          Id = createModelDataId()
          Sheets = []
          SelectedSheetId = None
          UndoEntry = UndoEntry("Init")
          CurrentUndoMode = Simple
         }

    let init =
        { Past = []
          Present = initModelData
          Future = [] }, Cmd.none

    let addToHistory undolist model = UndoHistory.add undolist (fun m -> m.CurrentUndoMode) model

    let createSheet =
        fun () ->
            { Id = createSheetId()
              Name = "new" + (createSheetName())
              Content = "Hello World"
              IsOpen = true}

    let addSheet m s =
        { m with
              Sheets = (m.Sheets |> List.append [ s ])
              UndoEntry  = UndoEntry("Sheet added")
              }
    
    let updateElement key f l = 
       l |> List.map (fun e -> if e.Id = key then f e else e)

    module ModelData =
      let newId (m:ModelData) = {m with Id = createModelDataId()}

    let update (msg: Msg) (m: Model<ModelData>) =
      
        match msg with
        | CreatePoint -> m, Cmd.none
        | Reset -> init
        | CreateSheet ->
            let sheet = {createSheet() with IsOpen = true}
            sheet
            |> addSheet m.Present
            |> ModelData.newId
            |> UndoHistory.addNew m, Cmd.ofMsg (SetSelectedSheet(Some(sheet.Id)))

        | SetSelectedSheet sheetId ->
            let s = List.tryFind (fun s -> Some(s.Id) = sheetId) m.Present.Sheets
            match s with
            |Some(s) -> match s.IsOpen with
                        |true -> { m.Present with SelectedSheetId = sheetId } |> UndoHistory.update m, Cmd.none
                        | false -> m, Cmd.none
            |None -> m, Cmd.none

        | RemoveSheet sheetIdToRemove ->
            let newSheets = m.Present.Sheets |> List.filter (fun s -> s.Id <> sheetIdToRemove)
            // Logic for selected tab: Keep existing selected if not removed. If
            // selected tab is removed, select next tab in list, or the new last tab
            // if we removed the last tab, or no tab if empty
            let newSelectedSheetId =
                if m.Present.SelectedSheetId <> Some sheetIdToRemove then
                    m.Present.SelectedSheetId
                else
                    let selectedIdx = m.Present.Sheets |> List.findIndex (fun t -> t.Id = sheetIdToRemove)
                    newSheets
                    |> List.tryItem selectedIdx
                    |> Option.orElse (newSheets |> List.tryLast)
                    |> Option.map (fun t -> t.Id)
            { m.Present with
                  Sheets = newSheets
                  SelectedSheetId = newSelectedSheetId
                  UndoEntry  = UndoEntry("Sheet removed") }
            |> ModelData.newId
            |> addToHistory m, Cmd.ofMsg (SetSelectedSheet(newSelectedSheetId))
        | CloseSheet sheetId -> {m with Present = {m.Present with Sheets = m.Present.Sheets
                                                                          |> updateElement sheetId (fun s -> {s with IsOpen = false}) }}
                                                                          , Cmd.none                             
        | OpenSheet sheetId -> {m with Present = {m.Present with Sheets = m.Present.Sheets
                                                                           |> updateElement sheetId (fun s -> {s with IsOpen = true}) }}
                                                                           , Cmd.none
        | Undo -> UndoHistory.undo m, Cmd.none
        | Redo -> UndoHistory.redo m, Cmd.none
        | StartUndoTransaction s ->
            let m' = UndoHistory.movePresentToPast m
            addToHistory m'
                { m.Present with
                      UndoEntry = UndoEntry(s)
                      CurrentUndoMode = Transactional
                      Id = createModelDataId()}, Cmd.none
        | EndUndoTransaction -> {m with Present = {m.Present with CurrentUndoMode = Simple}} , Cmd.none
        | SetModelData modelId -> UndoHistory.setPresent (UndoHistory.history m |> List.find (fun (md:ModelData) -> md.Id = modelId))  m, Cmd.none

    let bindings(): Binding<Model<ModelData>, Msg> list =
        [ "CreateSheet" |> Binding.cmd (CreateSheet)
          "Undo" |> Binding.cmd Undo
          "Redo" |> Binding.cmd Redo
          "History" 
          |> Binding.subModelSeq 
              ((fun m -> UndoHistory.history m), id,
                (fun () ->
                    [ "UndoEntry" |> Binding.oneWay (fun (_,e) -> e.UndoEntry)
                      "Id" |> Binding.oneWay (fun (_, t) -> t.Id)
                    ]))
          "SelectedUndoEntry" |> Binding.twoWay ((fun (m:Model<ModelData>) -> m.Present.Id), SetModelData)
          "Sheets"
          |> Binding.subModelSeq
              ((fun m -> m.Present.Sheets), id,
               (fun () ->
                   [ "Name" |> Binding.oneWay (fun (_, e) -> e.Name)
                     "CloseSheet" |> Binding.cmd (fun (_, s: Sheet) -> CloseSheet s.Id)
                     "OpenSheet" |> Binding.cmd (fun (_, s: Sheet) -> OpenSheet s.Id)
                     "RemoveSheet" |> Binding.cmd (fun (_, s: Sheet) -> RemoveSheet s.Id)
                     "Id" |> Binding.oneWay (fun (_, t) -> t.Id)
                     "IsOpen" |> Binding.oneWay (fun (_, e) -> e.IsOpen)
                     
                     ]))

          "OpenSheets"
          |> Binding.subModelSeq
              ((fun m -> List.filter (fun s -> s.IsOpen)  m.Present.Sheets), id,
               (fun () ->
                   [ "Name" |> Binding.oneWay (fun (_, e) -> e.Name)
                     "CloseSheet" |> Binding.cmd (fun (_, s: Sheet) -> CloseSheet s.Id)
                     "Id" |> Binding.oneWay (fun (_, t) -> t.Id) ]))
          "SelectedSheet" |> Binding.twoWayOpt ((fun m -> m.Present.SelectedSheetId), SetSelectedSheet) ]
