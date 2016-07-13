namespace Microsoft.VisualStudio.Text
open FSharpVSPowerTools
open Microsoft.VisualStudio.Text

module RefactorExtensions = 
  type Microsoft.VisualStudio.Text.ITextSnapshot
  with
    member x.getPosition = FSharp.EditingServices.BufferModel.ITextSnapshot(x).getPosition
    member x.snapshotSpanFromRange (startLine, startColumn, endLine, endColumn) = 
      FSharp.EditingServices.BufferModel.ITextSnapshot(x).snapshotSpanFromRange(startLine, startColumn, endLine, endColumn)

    /// Get the start and end line numbers of a snapshotSpan based on this textSnapshot
    /// returns a tuple of (startLineNumber, endLineNumber)
    member inline x.LineBounds (snapshotSpan: SnapshotSpan) =
        let startLineNumber = x.GetLineNumberFromPosition (snapshotSpan.Span.Start)
        let endLineNumber = x.GetLineNumberFromPosition (snapshotSpan.Span.End)
        (startLineNumber, endLineNumber)
 
    /// Get the text at line `num`
    member inline x.LineText num =  x.GetLineFromLineNumber(num).GetText()

    member x.FullSpan = FSharp.EditingServices.BufferModel.ITextSnapshot(x).FullSpan

  type Microsoft.VisualStudio.Text.SnapshotPoint
  with
    member x.ColumnIndex        = FSharp.EditingServices.BufferModel.SnapshotPoint(x).ColumnIndex
    member x.LineIndex          = FSharp.EditingServices.BufferModel.SnapshotPoint(x).LineIndex
    member x.ContainingLineText = FSharp.EditingServices.BufferModel.SnapshotPoint(x).ContainingLineText

  type Microsoft.VisualStudio.Text.SnapshotSpan with
      member inline x.StartLine = x.Snapshot.GetLineFromPosition (x.Start.Position)
      member inline x.StartLineNum = x.Snapshot.GetLineNumberFromPosition x.Start.Position
      member inline x.StartColumn = x.Start.Position - x.StartLine.Start.Position 
      member inline x.EndLine = x.Snapshot.GetLineFromPosition (x.End.Position)
      member inline x.EndLineNum  = x.Snapshot.GetLineNumberFromPosition x.End.Position
      member inline x.EndColumn = x.End.Position - x.EndLine.Start.Position

      member x.ModStart (num) = SnapshotSpan(SnapshotPoint (x.Snapshot, x.Start.Position + num), x.End)
      member x.ModEnd (num) = SnapshotSpan(x.Start, (SnapshotPoint (x.Snapshot,x.End.Position + num)))

      member x.ModBoth m1 m2 =
          SnapshotSpan(SnapshotPoint (x.Snapshot, x.Start.Position + m1),
                       SnapshotPoint (x.Snapshot, x.End.Position + m2))

      /// get the position of the token found at (line,.col) if token was not found then -1,-1
      member x.PositionOf (token:string) =
          let firstLine = x.StartLineNum
          let lastLine = x.EndLineNum
          let lines = [| for idx in firstLine .. lastLine -> x.Snapshot.LineText idx |]

          let withinBounds (line, col) =
              match line, col with
              | -1,-1 -> -1,-1 // fast terminate if token wasn't found
              |  l, c when c < x.StartColumn &&  l = firstLine -> -1,-1
              |  l, c when c > x.EndColumn &&  l = lastLine -> -1,-1
              | _ -> line,col

          let rec loop idx =
              if idx > lines.Length then -1,-1 else
              match lines.[idx].IndexOf(token) with
              | -1 -> loop (idx+1)
              | toki -> (firstLine+idx,toki)
        
          loop 0 |> withinBounds

      /// Return corresponding zero-based FCS range
      /// (lineStart, colStart, lineEnd, colEnd)
      member inline x.ToRange () =
          (x.StartLineNum, x.StartColumn, x.EndLineNum, x.EndColumn-1)

  type Microsoft.VisualStudio.Text.ITextBuffer with
      member x.GetSnapshotPoint (position: Microsoft.VisualStudio.Text.Editor.CaretPosition) = 
          Option.ofNullable <| position.Point.GetPoint(x, position.Affinity)
    
      member x.TriggerTagsChanged (sender: obj) (event: Event<_,_>) =
          let span = x.CurrentSnapshot.FullSpan.VSObject
          event.Trigger(sender, SnapshotSpanEventArgs(span))

  type Microsoft.VisualStudio.Text.Editor.ITextView with
      /// Return a simple zero-based (line, column) tuple from 
      /// actual caret position in this ITextView
      member x.GetCaretPosition () =
          maybe {
            let! point = x.TextBuffer.GetSnapshotPoint x.Caret.Position
            let line = point.Snapshot.GetLineNumberFromPosition point.Position
            let col = point.Position - point.GetContainingLine().Start.Position
            return (line, col)
          }

      /// Return Microsoft.FSharp.Compiler.Range.pos from actual 
      /// caret position in this ITextView taking care of off by 
      /// 1 indexing between VS and FCS
      member x.PosAtCaretPosition () =
          maybe {
            let! line, col = x.GetCaretPosition()
            return Microsoft.FSharp.Compiler.Range.mkPos (line + 1) (col + 1)
          }
