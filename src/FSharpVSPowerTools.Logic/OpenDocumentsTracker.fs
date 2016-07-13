﻿namespace FSharpVSPowerTools.ProjectSystem

open FSharp.EditingServices.BufferModel
open System
open System.Text
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text.Editor
open System.Collections.Generic
open FSharpVSPowerTools

[<NoComparison>]
type OpenDocument =
    { Document: ITextDocument
      Snapshot: ITextSnapshot 
      Encoding: Encoding
      LastChangeTime: DateTime
      ViewCount: int }
    member x.Text = lazy (x.Snapshot.GetText())
    static member Create document snapshot encoding lastChangeTime = 
        { Document = document
          Snapshot = snapshot
          Encoding = encoding
          LastChangeTime = lastChangeTime
          ViewCount = 1 }

type IOpenDocumentsTracker =
    abstract RegisterView: IWpfTextView -> unit
    abstract MapOpenDocuments: (KeyValuePair<string, OpenDocument> -> 'a) -> seq<'a>
    abstract TryFindOpenDocument: string -> OpenDocument option
    abstract TryGetDocumentText: string -> string option
    abstract DocumentChanged: IEvent<string>
    abstract DocumentClosed: IEvent<string>

[<Export(typeof<IOpenDocumentsTracker>)>]
type OpenDocumentsTracker [<ImportingConstructor>](textDocumentFactoryService: ITextDocumentFactoryService) =
    [<VolatileField>]
    let mutable openDocs = Map.empty
    let documentChanged = Event<_>()
    let documentClosed = Event<_>()
    let tryFindDoc path = openDocs |> Map.tryFind path
    let addDoc path doc = openDocs <- openDocs |> Map.add path doc
    
    let updateDoc path (f: OpenDocument -> OpenDocument) =
        match tryFindDoc path with
        | Some doc -> addDoc path (f doc)
        | None -> ()

    let tryGetDocument buffer = textDocumentFactoryService.TryGetTextDocument buffer

    interface IOpenDocumentsTracker with
        member __.RegisterView (view: IWpfTextView) = 
            ForegroundThreadGuard.CheckThread()
            maybe {
                let! doc = tryGetDocument (ITextBuffer view.TextBuffer)
                let path = doc.FilePath
                
                let textBufferChanged (args: TextContentChangedEventArgs) =
                    if openDocs |> Map.containsKey path then
                        ForegroundThreadGuard.CheckThread()
                        updateDoc path (fun doc -> { doc with Snapshot = args.After
                                                              LastChangeTime = DateTime.UtcNow })
                        documentChanged.Trigger path
                
                let textBufferChangedSubscription = view.TextBuffer.ChangedHighPriority.Subscribe (textBufferChanged << TextContentChangedEventArgs)
                
                let rec viewClosed _ =
                    match tryFindDoc path with
                    | Some doc when doc.ViewCount = 1 ->
                        Logging.logInfo (fun _ -> sprintf "[OpenDocumentTracker] Last view for %s, removing from map." path)
                        ForegroundThreadGuard.CheckThread()
                        textBufferChangedSubscription.Dispose()
                        viewClosedSubscription.Dispose()
                        openDocs <- Map.remove path openDocs
                        documentClosed.Trigger path
                    | Some doc ->
                        Logging.logInfo (fun _ -> 
                            sprintf "[OpenDocumentTracker] Still %d view are open for %s, do not remove." 
                                    (doc.ViewCount - 1) path) 
                        updateDoc path (fun doc -> { doc with ViewCount = doc.ViewCount - 1 })
                    | None -> ()
                
                and viewClosedSubscription: IDisposable = view.Closed.Subscribe viewClosed
                let! lastWriteTime = File.tryGetLastWriteTime path
                
                tryFindDoc path
                |> Option.map (fun doc -> { doc with ViewCount = doc.ViewCount + 1 })
                |> Option.getOrTry (fun _ ->
                    OpenDocument.Create doc (ITextSnapshot view.TextBuffer.CurrentSnapshot) doc.Encoding lastWriteTime)
                |> addDoc path
            } |> ignore
        
        member __.MapOpenDocuments f = Seq.map f openDocs
        member __.TryFindOpenDocument path = Map.tryFind path openDocs
        member __.TryGetDocumentText path = 
            let doc = openDocs |> Map.tryFind path 
            doc |> Option.map (fun x -> x.Text.Value)
        member __.DocumentChanged = documentChanged.Publish
        member __.DocumentClosed = documentClosed.Publish