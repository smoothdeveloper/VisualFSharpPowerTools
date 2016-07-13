﻿namespace FSharpVSPowerTools

open System
open System.Windows
open System.Windows.Shapes
open System.Windows.Media
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Utilities
open System.Windows.Controls
open FSharp.EditingServices.BufferModel
open System.Windows.Input
open Microsoft.VisualStudio.Text.Tagging
open FSharpVSPowerTools.ProjectSystem
open FSharpVSPowerTools.SyntaxColoring.UnusedSymbols

[<Name(Constants.fsharpUnusedDeclarationMargin)>]
type UnusedDeclarationMargin(textView: IWpfTextView, 
                             marginContainer: IWpfTextViewMargin,
                             tagAggregator: ITagAggregator<UnusedDeclarationTag>) =
    inherit Canvas()

    let children = base.Children
    let verticalScrollMargin = marginContainer.GetTextViewMargin(PredefinedMarginNames.VerticalScrollBar) :?> IWpfTextViewMargin
    let mutable markerData = Unchecked.defaultof<_>
    let markerBrush = SolidColorBrush(Color.FromRgb(255uy, 165uy, 0uy))

    let updateDisplay (CallInUIContext callInUIContext) =
        callInUIContext <| fun _ ->
            if not textView.IsClosed then
                let buffer = ITextBuffer textView.TextBuffer
                let span = buffer.CurrentSnapshot.FullSpan
                
                let data =
                    tagAggregator.GetTags(span.VSObject)
                    |> Seq.collect (fun mappedSpan -> mappedSpan.Span.GetSpans(buffer.VSObject))
                    |> Seq.map (fun span -> 
                        let pos = span.Start.Position
                        buffer.CurrentSnapshot.GetLineNumberFromPosition(pos), pos)
                    |> Seq.distinctBy fst
                    |> Seq.toArray

                markerData <- data
                children.Clear()
                let verticalScrollBar = verticalScrollMargin :?> IVerticalScrollBar
                let lineStart = verticalScrollBar.TrackSpanTop
                let totalLines = textView.TextSnapshot.LineCount
                let lineHeight = verticalScrollBar.TrackSpanHeight / float totalLines
                let scrollBarWidth = 
                    let tentativeWidth = verticalScrollMargin.VisualElement.Width
                    // VS 2012's 'MarginSize' returns height of vertical scrollbar, we use 'VisualElement.Width' instead
                    // Note that 'VisualElement.Width' is NaN in VS 2013
                    if Double.IsNaN tentativeWidth then
                        verticalScrollMargin.MarginSize
                    else tentativeWidth
                for (lineNo, pos) in markerData do               
                    let markerHeight = 3.0
                    let markerMargin = 2.0
                    // Ensure that marker width is non-negative, otherwise Rectangle.Width throws ArgumentException.
                    let markerWidth = max 3.0 (scrollBarWidth - markerMargin * 2.0)
                    let marker = Rectangle(Fill = Brushes.Orange, StrokeThickness = 0.5, Stroke = markerBrush,
                                            Height = markerHeight, Width = markerWidth,
                                            Cursor = Cursors.Hand, ToolTip = sprintf "Unused declaration(s) at line %i" (lineNo + 1))
                    // Try to place the marker on top of vertical scroll bar
                    Canvas.SetLeft(marker, - (markerWidth + markerMargin))
                    Canvas.SetTop(marker, lineStart + float lineNo * lineHeight)
                    marker.MouseDown.Add(fun _ -> 
                        let line = textView.TextSnapshot.GetLineFromLineNumber(lineNo)
                        textView.Caret.MoveTo(VirtualSnapshotPoint(ITextSnapshot textView.TextSnapshot, pos)) |> ignore
                        textView.ViewScroller.EnsureSpanVisible(SnapshotSpan(line.Start, 0), EnsureSpanVisibleOptions.ShowStart))
                    children.Add(marker) |> ignore

    let docEventListener = new DocumentEventListener ([ViewChange.viewportHeightEvent textView; ViewChange.tagsEvent tagAggregator], 
                                                      200us, updateDisplay)

    interface IWpfTextViewMargin with
        member __.Enabled = true
        
        member x.GetTextViewMargin(marginName: string): ITextViewMargin = 
            if marginName = Constants.fsharpUnusedDeclarationMargin then x :> _ else null
        
        member x.MarginSize: float = x.ActualHeight
        member x.VisualElement: FrameworkElement = x :> _

        member __.Dispose() = 
            (docEventListener :> IDisposable).Dispose()
            tagAggregator.Dispose()
