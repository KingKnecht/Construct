namespace Construct.Framework

module App =
  open Elmish
  open Elmish.WPF

  type Id = Id of string

  let makeIdGenerator prefix =
    let mutable intId = 0
    fun () -> 
        intId <- (+) intId 1
        Id  (prefix <| intId)

  let makeNameGenerator prefix =
    let mutable intId = 0
    fun () -> 
          intId <- (+) intId 1
          (prefix <| intId)
  
  let createSheetId = makeIdGenerator (sprintf "ID%03i")
  let createSheetName = makeNameGenerator (sprintf "-%03i")
   
  type Sheet = {
    Id : Id
    Name : string
    }
    
  type Model =
    {
      Sheets : Sheet list
      SelectedSheetId : Id option
    }


  type Msg =
    | CreatePoint
    | CreateSheet
    | SetSelectedSheet of Id : Id option
    | Reset
    | CloseSheet of Id : Id


  let init =  
    {
         Sheets = []
         SelectedSheetId = None
    }, Cmd.none

  let createSheet m =
    {m with Sheets = (m.Sheets |> List.append [{Id = createSheetId (); Name = "new" + (createSheetName ())}])}
        
 
  let update msg m =
    match msg with
    | CreatePoint -> m, Cmd.none
    | Reset -> init
    | CreateSheet -> let newId = createSheetId()
                     let newSheet = {Id = newId; Name = "new" + (createSheetName ())}
                     {m with Sheets = (m.Sheets |> List.append [newSheet]); SelectedSheetId = Some newId}, Cmd.none
    | SetSelectedSheet sheetId -> {m with SelectedSheetId = sheetId}, Cmd.none

    | CloseSheet sheetIdToClose -> 
            let newSheets = m.Sheets |> List.filter (fun s -> s.Id <> sheetIdToClose)
            // Logic for selected tab: Keep existing selected if not removed. If
            // selected tab is removed, select next tab in list, or the new last tab
            // if we removed the last tab, or no tab if empty
            let newSelectedSheetId =
                if m.SelectedSheetId <> Some sheetIdToClose then m.SelectedSheetId   
                else
                let selectedIdx = m.Sheets |> List.findIndex (fun t -> t.Id = sheetIdToClose)
                newSheets
                |> List.tryItem selectedIdx
                |> Option.orElse (newSheets |> List.tryLast)
                |> Option.map (fun t -> t.Id)
            { m with Sheets = newSheets; SelectedSheetId = newSelectedSheetId }, Cmd.none

  let bindings () : Binding<Model, Msg> list = [
    "CreateSheet" |> Binding.cmd CreateSheet
    "Sheets" |> Binding.subModelSeq((fun m -> m.Sheets),(fun s -> s), fun () -> 
        [
            "Name" |> Binding.oneWay (fun (_, e) -> e)
            "CloseSheet" |> Binding.cmd (fun (_, s) -> CloseSheet s.Id)
            "Id" |> Binding.oneWay(fun (_,t) -> t.Id)
        ])
    "SelectedSheet" |> Binding.twoWayOpt((fun m -> m.SelectedSheetId), SetSelectedSheet)
  
]



    