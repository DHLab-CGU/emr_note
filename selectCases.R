#' @rdname selectCase
#' @export
#'
selectCases <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, groupDataType = ccs, CustomGroupingTable, isDescription = TRUE, caseCondition, caseCount, PeriodRange = c(30, 365), CaseName = "Selected"){

  DxDataFile <- as.data.table(DxDataFile)

  ## 將使用者傳入三個欄位名稱依序更改為ID, ICD, Date
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol, with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")

  ## Date 統一轉成date的格式
  DxDataFile[,"Date"] <- as.Date(DxDataFile[,Date])

  nonCaseName <- paste0("non-",CaseName)
  semiCaseName <- paste0(CaseName,"*")

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

  ## Case篩選的第一個條件: 分組欄位中符合caseCondition. 並依ID & Date 做排序
  Case <- unique(groupedData[grepl(caseCondition, groupedData[,eval(parse(text = paste(groupDataType)))]),][order(ID, Date)])

  ## Case篩選的第2&3個條件: 計算case 在時間範圍內 (PeriodRange) 的次數 (CaseCount)
  if(nrow(Case) > 0){
    ## 使用者設定的case最少出現次數 > 1
    if(caseCount > 1){
      ## 假設caseCount設為3, 將同一個病患的診斷紀錄分成三筆日期一組(endCaseDate為第三筆), 當時間間隔(period)在PeriodRange內, 表示該病患符合篩選條件
      CaseCount <- Case[, endCaseDate := shift(Date, caseCount -1 , type = "lead"), by = "ID"][is.na(endCaseDate), endCaseDate := Date][, period := endCaseDate - Date, by = "ID"][between(period, PeriodRange[1], PeriodRange[2], incbounds = TRUE),][,count := caseCount,]
      ## 修改欄位名稱: Date -> firstCaseDate
      setnames(CaseCount,"Date", "firstCaseDate")

    }else{
      ## 使用者設定的case最少出現次數 = 1, 就不考慮PeriodRange.
      ## summarize firstDate, EndDate, count, period
      CaseCount <- Case[, c("firstCaseDate","endCaseDate","count") := list(min(Date), max(Date),.N), by = "ID"][,period := endCaseDate - firstCaseDate,][,-"Date"]
      CaseCount <- unique(CaseCount)
    }
  }else{
    ## 當case沒有資料時,則列為control (non-SelectedCase)
    nonSelectedCase <- DxDataFile[,list(ID)][,selectedCase := nonCaseName][!duplicated(ID),][order(ID),]
    message("No matching Case")
    return(nonSelectedCase)
  }

  ##找出每個Case中最常出現的ICD (MostCommonICD) 及次數 (MostCommonICDCount)
  CaseMostICDCount <- CaseCount[,list(MostCommonICDCount = .N),by = list(ID,ICD)][order(MostCommonICDCount, decreasing = TRUE),][!duplicated(ID),]
  selectedCase <- merge(CaseCount[,-"ICD"], CaseMostICDCount,"ID")[,selectedCase := CaseName]
  setnames(selectedCase,"ICD","MostCommonICD")
  ## 將原本資料中不是selectedCase的都列為 non-selected case
  nonSelectedCase <- DxDataFile[!Case, on = "ID", list(ID)][,selectedCase := nonCaseName][!duplicated(ID),]

  ## 當selectedCase跟Case 人數不一時, 表示有些case可能不符合篩選的特定條件,將此dataset列為semi-Case (selected*)
  if(length(unique(Case$ID)) > length(unique(selectedCase$ID))){
    semiCase <- Case[!selectedCase, on = "ID", list(ID)][,selectedCase := semiCaseName][!duplicated(ID),]
    nonSelectedCase <- rbind(nonSelectedCase,semiCase)
  }
  ## 將SelectedCase & nonSelectedCase合併, 並依照MostCommonICDCount由大到小排序
  allData <- rbindlist(list(nonSelectedCase, selectedCase),fill = TRUE, use.names = TRUE)[order(MostCommonICDCount,decreasing = TRUE),][!duplicated(ID),]
  allData <- allData[,c("ID","selectedCase","count","firstCaseDate","endCaseDate","period","MostCommonICD","MostCommonICDCount")]
  allData
}
