package flux.react.app.desktop

private[desktop] case class TaskSeqSelection(start: TaskSeqCursor, end: TaskSeqCursor) {
  require(start <= end)
}
private[desktop] object TaskSeqSelection {
  def collapsed(cursor: TaskSeqCursor): TaskSeqSelection = TaskSeqSelection(start = cursor, end = cursor)
}
