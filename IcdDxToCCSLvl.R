#' @rdname DxCCS
#' @export
#'
IcdDxToCCSLvl <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, CCSLevel = 1, isDescription = TRUE){

  DxDataFile <- as.data.table(DxDataFile)

  ## 將使用者傳入三個欄位名稱依序更改為ID, ICD, Date
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")

  ## Date 統一轉成date的格式
  ## 新增Number欄位, 為了最後標準化分組完的資料能依照原始資料(DxDataFile)的排列順序
  DxDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(DxDataFile))]

  ## 將DxDataFile的ICD進行統一格式的轉換
  ## 新增欄位Short: 統一格式轉換的ICD short format
  Conversion <- IcdDxDecimalToShort(DxDataFile,ICD,Date,icd10usingDate)
  DxDataFile[,Short := Conversion$ICD]

  ## 依照使用者選擇的分組方式選擇相對應的 CCS的欄位
  if(isDescription){
    CCSLvlCol <- paste0("CCS_LVL_", CCSLevel, "_LABEL")
  }else{
    CCSLvlCol <- paste0("CCS_LVL_", CCSLevel)
  }

  ##由於ICD10的CCS lvl是1~2, 所以設條件 CCS小於等於2時會進行ICD9 + ICD10的標準化分組, 大於2則只進行ICD9的標準化分組
  if(CCSLevel <= 2){
    ## 將ICD9跟ICD10的資料分別進行標準化分組最後再進行合併
    allCCSLvl <- rbind(merge(DxDataFile[Date < icd10usingDate],ccsDxICD9[,c("ICD", CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                         merge(DxDataFile[Date >= icd10usingDate],ccsDxICD10[,c("ICD", CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))
  }else{
    ## 將ICD9資料進行標準化分組最後再和icd10的資料進行合併
    allCCSLvl <- merge(merge(DxDataFile[Date < icd10usingDate],ccsDxICD9[,c("ICD", CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                         DxDataFile[Date >= icd10usingDate], by = names(DxDataFile), all = TRUE)
  }

  ## 將分組完的資料依照number的順序排成原本使用者資料的順序
  allCCSLvl <- allCCSLvl[order(Number),-"Number"]

  ## 當標準化分組的欄位為NA的數量小於總量 (表示有分到組)
  if(nrow(allCCSLvl[is.na(eval(parse(text = paste(CCSLvlCol))))]) < nrow(allCCSLvl)){
    ## 取有分到組的資料
    ## summarize firstDate, EndDate, count by ID and com_col, 並計算period
    summarisedCCSLvl <- allCCSLvl[!is.na(eval(parse(text = paste(CCSLvlCol)))),
                                  list(firstCaseDate = min(Date),
                                       endCaseDate = max(Date),
                                       count = .N),
                                  by = c("ID",CCSLvlCol)][,period := (endCaseDate - firstCaseDate),][order(ID),]

    return(list(groupedDT = allCCSLvl,
                summarised_groupedDT = summarisedCCSLvl,
                Error = Conversion$Error))

  }else{
    return(list(groupedDT = allCCSLvl,
                Error = Conversion$Error))
  }

}
