#' @rdname era
#' @export
#'
getConditionEra <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, groupDataType = ccs, CustomGroupingTable, isDescription = TRUE, gapDate = 30, selectedCaseFile = NULL){

  DxDataFile <- as.data.table(DxDataFile)

  ## 將使用者傳入三個欄位名稱依序更改為ID, ICD, Date
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")

  ## Date 統一轉成date的格式
  DxDataFile[,"Date"] <- as.Date(DxDataFile[,Date])

  ## 根據*groupDataType*將資料進行標準化分組 >
  groupDataType <- toupper(deparse(substitute(groupDataType)))
  groupedData <- groupMethodSelect(DxDataFile, ID, ICD, Date,
                                   icd10usingDate, groupDataType, CustomGroupingTable, isDescription)
  ## 根據*groupDataType*將資料進行標準化分組 <

  if(groupDataType != "ICD"){
    ## 當*groupDataType*不是ICD, 取標準化分組的長表"groupedDT"
    groupedData <- groupedData$groupedDT[,-"ICD"]
  }else{
    ## 當*groupDataType*是ICD, 取統一轉換的欄位名稱(short或decimal)改成ICD
    groupedData <- groupedData[,-"ICD"]
    names(groupedData) <- gsub("Short|Decimal", "ICD", names(groupedData))
  }
  ## 將 *groupDataType* 改為"groupedData"的標準分組的欄位(如CCS: CCS_CATEGORY or CCS_DESCRIPTION)
  groupDataType <- names(groupedData)[ncol(groupedData)]
  groupByCol <- c("ID",groupDataType)

  ## 當groupedData是空值或groupedData中標準化分組的欄位都是NA, 則回傳groupedData (無法計算condition era)
  if(is.null(groupedData) | nrow(groupedData[is.na(eval(parse(text = paste(groupDataType))))]) == nrow(groupedData)){
    return(groupedData)
  }else{
    #移除groupedData中groupDataType是NA的資料
    groupedData <- na.omit(groupedData, groupDataType)
  }

  ## 將groupedData依照groupByCol跟date排序
  ## 新增欄位lastDate: 同一個使用者且同一個分組類別的前一個紀錄日期
  ## 新增欄位Gap: date跟lastDate相差天數
  ## 新增欄位episode: 當gap > 使用者設定的gapDate,則為TRUE
  ## 新增欄位era: 累計同一個使用者且同一個分組類別的episode, 預設從era = 1開始累計
  eraCount <- groupedData[order(eval(parse(text = paste(groupByCol))),Date)][,LastDate := shift(Date, type = "lag"),by = groupByCol][is.na(LastDate), LastDate := Date][,Gap := Date- LastDate][,episode := Gap > gapDate][,era := cumsum(episode)+1, by = groupByCol][,-"episode"]

  ## summarize firstDate, EndDate, count, period
  ## 刪除同一個era中至少兩筆的第一筆 (period = 0)
  conditionEra <- eraCount[,list(firstCaseDate = min(Date),
                                 endCaseDate = max(Date),
                                 count = .N,
                                 period = Gap),by = c(groupByCol,"era")][!(period == 0 & count > 1),]

  ## 若有select case合併
  if(!is.null(selectedCaseFile)){
    conditionEra <- merge(conditionEra, selectedCaseFile[,list(ID, selectedCase)], all.x = TRUE)
  }
  ## 排序
  conditionEra <- conditionEra[order(ID),]
  conditionEra
}
