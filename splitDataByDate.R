#' @rdname DataSplit
#' @export
#'
splitDataByDate <- function(DxDataFile, idColName, icdColName, dateColName, indexDateFile, Gap = 30){

  DxDataFile <- as.data.table(DxDataFile)
  indexDateFile <- as.data.table(indexDateFile)

  ## 將使用者傳入三個欄位名稱依序更改為ID, ICD, Date
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")

  ## Date 統一轉成date的格式
  DxDataFile[,"Date"] <- as.Date(DxDataFile$Date)

  ## 在 indexDate之前標示B, 之後標示A
  ## 並計算window (以Gap為單位)
  splitedData <- merge(DxDataFile, indexDateFile,
                       all.x = TRUE)[,diff := Date - as.Date(indexDate)][diff >= 0, timeTag := "A"][diff < 0, timeTag := "B"][,window := abs((as.integer(diff) %/% Gap)),][timeTag == "A", window := window +1,][order(ID,Date), -"diff"]

  splitedData
}
