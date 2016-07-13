namespace FSharp.EditingServices.BufferModel

open FSharpVSPowerTools
open System.Diagnostics
type VSClassificationSpan = Microsoft.VisualStudio.Text.Classification.ClassificationSpan
type VSSnapshotSpan = Microsoft.VisualStudio.Text.SnapshotSpan
type VSSnapshotPoint = Microsoft.VisualStudio.Text.SnapshotPoint
type VSTextSnapshot = Microsoft.VisualStudio.Text.ITextSnapshot
type VSTextSnapshotLine = Microsoft.VisualStudio.Text.ITextSnapshotLine
type VSCaretPosition = Microsoft.VisualStudio.Text.Editor.CaretPosition
type VSPositionAffinity = Microsoft.VisualStudio.Text.PositionAffinity
type VSTextBuffer = Microsoft.VisualStudio.Text.ITextBuffer
type VSSpan = Microsoft.VisualStudio.Text.Span
type VSIClassificationType = Microsoft.VisualStudio.Text.Classification.IClassificationType
type VSIClassificationTypeRegistryService = Microsoft.VisualStudio.Text.Classification.IClassificationTypeRegistryService
type VSITextDocument = Microsoft.VisualStudio.Text.ITextDocument
type VSTextContentChangedEventArgs = Microsoft.VisualStudio.Text.TextContentChangedEventArgs
type VSITextDocumentFactoryService = Microsoft.VisualStudio.Text.ITextDocumentFactoryService
type VSIMappingPoint = Microsoft.VisualStudio.Text.IMappingPoint
type VSVirtualSnapshotPoint = Microsoft.VisualStudio.Text.VirtualSnapshotPoint
type VSNormalizedSnapshotSpanCollection = Microsoft.VisualStudio.Text.NormalizedSnapshotSpanCollection
type VSITrackingSpan = Microsoft.VisualStudio.Text.ITrackingSpan
module Utils =
  open System
  let mapNullable f (nullable: System.Nullable<_>) = if nullable.HasValue then Nullable<_>(f nullable.Value) else Nullable<_>()

type ITextSnapshotLine(textSnapshotLine : VSTextSnapshotLine) =
  member x.GetText = textSnapshotLine.GetText
  member x.Start = textSnapshotLine.Start
  member x.Length = textSnapshotLine.Length
  member x.LineNumber = textSnapshotLine.LineNumber
  
  //member x.Start = textSnapshotLine.Start
and ITextSnapshot(snapshot: VSTextSnapshot) = 
    member x.VSObject = snapshot
    /// SnapshotSpan of the entirety of this TextSnapshot
    member x.FullSpan = SnapshotSpan(snapshot, 0, x.Length)
    member x.CreateEdgeExclusiveTrackingSpan (span: Span) = snapshot.CreateTrackingSpan(VSSpan(span.Start, span.End), Microsoft.VisualStudio.Text.SpanTrackingMode.EdgeExclusive)
    member x.GetLineFromLineNumber = snapshot.GetLineFromLineNumber >> ITextSnapshotLine
    member x.GetLineNumberFromPosition = snapshot.GetLineNumberFromPosition
    member x.GetLineFromPosition = snapshot.GetLineFromPosition >> ITextSnapshotLine
    member x.getPosition (startLine: int) startColumn = snapshot.GetLineFromLineNumber(startLine).Start.Position + startColumn
    member x.LineCount = snapshot.LineCount
    member x.Length = snapshot.Length
    member x.Version = snapshot.Version
    member x.GetText () = snapshot.GetText()
    member x.TextBuffer = ITextBuffer snapshot.TextBuffer
    member x.snapshotSpanFromRange (startLine: int, startCol, endLine: int, endCol) =
        let startPos = x.getPosition (startLine) startCol
        let endPos = x.getPosition (endLine) endCol
        Debug.Assert(startPos <= endPos, sprintf "startPos = %d, endPos = %d" startPos endPos)
        SnapshotSpan(snapshot, startPos, endPos - startPos)
    static member op_Implicit (textSnapshot:ITextSnapshot) = textSnapshot.VSObject
and IClassificationTypeRegistryService(service:VSIClassificationTypeRegistryService) =
  class
    member x.GetClassificationType = service.GetClassificationType >> IClassificationType
  end
and IClassificationType(classificationType: VSIClassificationType) =
    member x.VSObject = classificationType
and ClassificationSpan(snapshotSpan: SnapshotSpan, classificationType: IClassificationType) =
    member x.VSObject = VSClassificationSpan(snapshotSpan.VSObject, classificationType.VSObject)
and SnapshotSpan(snapshotSpan: VSSnapshotSpan) =
    new(startPoint: VSSnapshotPoint, endPoint: VSSnapshotPoint) = SnapshotSpan(VSSnapshotSpan(startPoint, endPoint))
    new(snapshot: ITextSnapshot, startPos, length) = SnapshotSpan(VSSnapshotSpan(snapshot.VSObject, startPos, length))
    new(snapshot: VSTextSnapshot, startPos, length) = SnapshotSpan(VSSnapshotSpan(snapshot, startPos, length))
    member x.VSObject = snapshotSpan
    member x.Contains (span: SnapshotSpan) = snapshotSpan.Contains span.VSObject
    member x.TranslateTo = snapshotSpan.TranslateTo 
    member x.TranslateToEdgeExclusive (targetSnapshot: ITextSnapshot) = SnapshotSpan(snapshotSpan.TranslateTo (targetSnapshot.VSObject, Microsoft.VisualStudio.Text.SpanTrackingMode.EdgeExclusive))
    member x.Snapshot = ITextSnapshot(snapshotSpan.Snapshot)
    member x.GetText = snapshotSpan.GetText
    member x.Start = snapshotSpan.Start
    member x.End = snapshotSpan.End
    member x.Span = Span(snapshotSpan.Span)
    member x.Length = snapshotSpan.Length

    member inline x.StartLine = snapshotSpan.Snapshot.GetLineFromPosition (snapshotSpan.Start.Position)
    member inline x.StartLineNum = snapshotSpan.Snapshot.GetLineNumberFromPosition snapshotSpan.Start.Position
    member inline x.StartColumn = snapshotSpan.Start.Position - x.StartLine.Start.Position 
    member inline x.EndLine = snapshotSpan.Snapshot.GetLineFromPosition (snapshotSpan.End.Position)
    member inline x.EndLineNum  = snapshotSpan.Snapshot.GetLineNumberFromPosition x.End.Position
    member inline x.EndColumn = snapshotSpan.End.Position - x.EndLine.Start.Position

    /// Return corresponding zero-based FCS range
    /// (lineStart, colStart, lineEnd, colEnd)
    member inline x.ToRange () =
        (x.StartLineNum, x.StartColumn, x.EndLineNum, x.EndColumn-1)
and SnapshotPoint (point: VSSnapshotPoint) =
  class
    let line () = point.Snapshot.GetLineNumberFromPosition point.Position
    let col () = point.Position - point.GetContainingLine().Start.Position
    let line_text () = point.GetContainingLine().GetText()
    
    member x.VSObject = point
    member x.ColumnIndex = col ()
    member x.LineIndex = line ()
    member x.ContainingLineText = line_text ()
    member x.Snapshot =  ITextSnapshot point.Snapshot
    member x.Position = point.Position
    member x.GetContainingLine = point.GetContainingLine
    member x.InSpan (span: SnapshotSpan) = 
        // The old snapshot might not be available anymore, we compare on updated snapshot
        let point = x.VSObject.TranslateTo(span.VSObject.Snapshot, Microsoft.VisualStudio.Text.PointTrackingMode.Positive)
        point.CompareTo span.Start >= 0 && point.CompareTo span.End <= 0
    static member op_Implicit (snapshotPoint:SnapshotPoint) = int snapshotPoint.VSObject
  end
and IMappingPoint(point: VSIMappingPoint) =
    member x.GetPoint (textBuffer: ITextBuffer) (affinity: PositionAffinity) = 
      let point = point.GetPoint((textBuffer.VSObject : VSTextBuffer), (affinity.VSObject : VSPositionAffinity))
      if point.HasValue then Some (point.Value |> SnapshotPoint) else None

and CaretPosition(caret: VSCaretPosition) =
    member x.Affinity = PositionAffinity caret.Affinity
    member x.Point = IMappingPoint caret.Point

and PositionAffinity(affinity: VSPositionAffinity) =
    member internal x.VSObject = affinity

and ITextBuffer(buffer: VSTextBuffer) =
    member internal x.VSObject = buffer
    member x.CurrentSnapshot = ITextSnapshot buffer.CurrentSnapshot
    member x.GetSnapshotPoint (position: CaretPosition) = position.Point.GetPoint x position.Affinity
    member x.Replace (span: Span) newText = buffer.Replace(VSSpan(span.Start, span.End), newText)
    member x.ChangedLowPriority = buffer.ChangedLowPriority
    member x.Insert = buffer.Insert
    static member op_Implicit (textBuffer:ITextBuffer) = textBuffer.VSObject
and ITextDocument(document: VSITextDocument) =
    member x.TextBuffer = ITextBuffer(document.TextBuffer)
    member x.FilePath = document.FilePath
    member x.Encoding = document.Encoding
    member x.IsDirty = document.IsDirty

and Span(span: VSSpan) =
    member x.Start = span.Start
    member x.End = span.End

and ITextDocumentFactoryService(service: VSITextDocumentFactoryService) = 
  member x.TryGetTextDocument (textBuffer:ITextBuffer) = 
    match service.TryGetTextDocument(textBuffer.VSObject) with
    | true, doc -> Some (ITextDocument(doc))
    | _ -> None

and TextContentChangedEventArgs(args: VSTextContentChangedEventArgs) =
  member x.After = ITextSnapshot args.After
and VirtualSnapshotPoint (point: VSVirtualSnapshotPoint) =
  new (sn:ITextSnapshot, position: int) = VirtualSnapshotPoint(VSVirtualSnapshotPoint(sn.VSObject, position))
  member x.VSObject = point
  member x.Position = point.Position |> SnapshotPoint
and NormalizedSnapshotSpanCollection (col: VSNormalizedSnapshotSpanCollection) =
  member x.VSObject = col
and ITrackingSpan (span: VSITrackingSpan) =
  member x.VSObject = span