#' @rdname dataWide
#' @export
#'
groupedDataLongToWide <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, groupDataType = ccs, CustomGroupingTable, isDescription = TRUE, numericOrBinary = B, selectedCaseFile = NULL){

  DxDataFile <- as.data.table(DxDataFile)

  ## 將使用者傳入三個欄位名稱依序更改為ID, ICD, Date
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")

  ## 根據*groupDataType*將資料進行標準化分組 >
  groupDataType <- toupper(deparse(substitute(groupDataType)))
  groupedData <- groupMethodSelect(DxDataFile, ID, ICD, Date, icd10usingDate, groupDataType, CustomGroupingTable, isDescription)
  ## 根據*groupDataType*將資料進行標準化分組 <

  if(groupDataType != "ICD"){
    ## 當*groupDataType*不是ICD, 取標準化分組的長表"summarised_groupedDT"
    groupedData <- groupedData$summarised_groupedDT

    if(is.null(groupedData)){return(groupedData)}

  }else{
    ## summarize firstDate, EndDate, count, period
    groupedData <- groupedData[,list(firstCaseDate = min(Date),endCaseDate = max(Date),count = .N),by = c("ID","Short")][,period := (endCaseDate - firstCaseDate),]

  }

  ## 將長表依ID及標準化分組的欄位 轉成寬表
  wideData <- dcast(groupedData, ID~eval(parse(text = paste(names(groupedData)[2]))), value.var = c("count"))

  ## 取不在寬表內的病患 (表示標準化分組結果都是NA的病患) 和寬表的病患表格合併
  if(length(wideData$ID) != length(DxDataFile$ID)){
    OtherPatientDt <- data.table(ID =  DxDataFile[!wideData, on = "ID"][!duplicated(ID), ID])
    wideData <- rbindlist(list(OtherPatientDt, wideData), fill = TRUE, use.names = TRUE)
  }

  ## 寬表中NA轉為 0
  wideData[is.na(wideData)] <- 0L
  numericOrBinary <- toupper(deparse(substitute(numericOrBinary)))

  ## 當使用者選擇以binary方式顯示
  if(numericOrBinary == "B"){
    wideData_B <- as.data.frame(wideData >= 1L)
    wideData_B$ID <- wideData$ID
    wideData <- wideData_B
  }else if(numericOrBinary != "B" && numericOrBinary != "N"){
    stop("'please enter N or B for 'numericOrBinary'", call. = FALSE)
  }

  ## 為了後續tableone 將部分標準化分組的組別名稱前面加上前綴字
  if(isDescription == FALSE && groupDataType == "ICD|CCS|CCSLVL|PHEWAS"){
    names(wideData)[2:ncol(wideData)] <- paste0(groupDataType,"_",names(wideData)[2:ncol(wideData)])
  }

  ##當使用者有分case/control
  if(!is.null(selectedCaseFile)){
    wideData_selected <- merge(wideData, selectedCaseFile[,list(ID, selectedCase)],by = "ID")

    return(wideData_selected)

  }else{

    return(wideData)
  }
}
