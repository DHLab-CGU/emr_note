#' @importFrom stats complete.cases
#' @export
#' @rdname DxCCS
#'
IcdDxToCCS <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, isDescription = TRUE){

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
  if (isDescription){
    ccs_col <- "CCS_CATEGORY_DESCRIPTION"
  }else {
    ccs_col <- "CCS_CATEGORY"
  }

  ## 將ICD9跟ICD10的資料分別進行標準化分組最後再進行合併
  allCCS <- rbind(merge(DxDataFile[Date <icd10usingDate],ccsDxICD9[,c("ICD",ccs_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                  merge(DxDataFile[Date >=icd10usingDate],ccsDxICD10[,c("ICD",ccs_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))

  ## 將分組完的資料依照number的順序排成原本使用者資料的順序
  allCCS <- allCCS[order(Number),-"Number"]


  ## 當標準化分組的欄位為NA的數量小於總量 (表示有分到組)
  if(nrow(allCCS[is.na(eval(parse(text = paste(ccs_col))))]) < nrow(allCCS)){
    ## 取有分到組的資料
    ## summarize firstDate, EndDate, count by ID and com_col, 並計算period
    summarisedIcdToCCS <- allCCS[!is.na(eval(parse(text = paste(ccs_col)))),
                                 list(firstCaseDate = min(Date),
                                      endCaseDate = max(Date),
                                      count = .N),
                                 by = c("ID",ccs_col)][,period := (endCaseDate - firstCaseDate),][order(ID)]

    return(list(groupedDT = allCCS,
                summarised_groupedDT = summarisedIcdToCCS,
                Error = Conversion$Error))

  }else{
    return(list(groupedDT = allCCS,
                Error = Conversion$Error))
  }
}
