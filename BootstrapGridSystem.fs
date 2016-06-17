module Bootstrap.GridSystem

open Suave.Html

type ColumnWidth =
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Eleven
  | Twelve

  member this.ToClassString =
    match this with
      | One -> "1"
      | Two -> "2"
      | Three -> "3"
      | Four -> "4"
      | Five -> "5"
      | Six -> "6"
      | Seven -> "7"
      | Eight -> "8"
      | Nine -> "9"
      | Ten -> "10"
      | Eleven -> "11"
      | Twelve -> "12"

type ColumnSize =
  | ExtraSmall of ColumnWidth
  | Small of ColumnWidth
  | Medium of ColumnWidth
  | Large of ColumnWidth

  member this.ToClassString =
    match this with
      | ExtraSmall width ->
        sprintf "col-xs-%s" width.ToClassString
      | Small width ->
        sprintf "col-sm-%s" width.ToClassString
      | Medium width ->
        sprintf "col-md-%s" width.ToClassString
      | Large width ->
        sprintf "col-lg-%s" width.ToClassString

let containerFluid attributes innerHtml =
  let attributes = attributes @ [ "class", "container-fluid" ]
  divAttr attributes innerHtml

let row attributes innerHtml =
  let attributes = attributes @ [ "class", "row" ]
  divAttr attributes innerHtml

let column (columnSizes : ColumnSize List) attributes innerHtml =
  let attributes =
    let columnClasses =
      columnSizes
      |> List.map (fun column -> sprintf "%s " column.ToClassString)
      |> List.fold (+) " "
    attributes @ [ "class", columnClasses ]
  divAttr attributes innerHtml

//example Column
(*
   column [ ExtraSmall Ten; Small Ten; Medium Ten; Large Ten; ] [ "id", "testId" ] []
*)
