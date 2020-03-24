#' Fold code regions in an r notebook file
#'
#' @param currentChunkOnly Fold the current chunk, or all of them?
#' @examples
#' \dontrun{
#' foldCode(T) # fold only the code region in the selection
#' foldCode() # fold all code regions
#' }
#' @export
foldCode = function (
  currentChunkOnly = F
) {

  library(rstudioapi)

  ctx = getSourceEditorContext()
  rangeTypes = c('```','$$')

  ranges = c()
  rangeStart = NULL
  rangeType = NULL

  lineRangeType = function(line) {
    for(rangeType in rangeTypes)
      if (startsWith(line,rangeType))
        return(rangeType)
  }

  for(l in 1:length(ctx$contents)) {

    line = ctx$contents[l]
    lastLine = ifelse(l == 0, NULL, ctx$contents[l-1])
    lrt = lineRangeType(line)

    if(is.null(rangeType) & !is.null(lrt)) {
      rangeType = lrt
      rangeStart = c(l,nchar(line)+1)
    }
    else if (!is.null(rangeType) & !is.null(lrt)) {

      if (rangeStart[1] < l-1)
        ranges[[length(ranges)+1]] = document_range(
          rangeStart,
          c(l-1,nchar(lastLine)+1)
        )

      rangeType = NULL
      rangeStart = NULL

    }

  }

  selection = ctx$selection[[1]]$range

  for(range in ranges) {
    if(currentChunkOnly & !isInRange(selection, range))
      next
    setSelectionRanges(range, id = ctx$id)
    executeCommand('fold')
  }

}

isInRange = function(selection, comparison) {

  lessThan = function (a,b) {
    if(a[1] < b[1]) return(T)
    if(a[1] == b[1] & a[2] < b[2]) return (T);
    return (F);
  }

  if(lessThan(selection$start, comparison$start)) return(F)
  if(lessThan(comparison$end, selection$end)) return(F)
  return(T)

}


