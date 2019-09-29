#' @rdname DxPheWAS
#' @export
IcdDxToPheWAS <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, isDescription = TRUE){

  DxDataFile <- as.data.table(DxDataFile)

  ## 將使用者傳入三個欄位名稱依序更改為ID, ICD, Date
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  DxDataFile <- DxDataFile[,DataCol,with = FALSE]
  names(DxDataFile) <- c("ID", "ICD", "Date")

  ## Date 統一轉成date的格式
  ## 新增Number欄位, 為了最後標準化分組完的資料能依照原始資料(DxDataFile)的排列順序
  DxDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(DxDataFile))]

  ## 將DxDataFile的ICD進行統一格式的轉換
  ## 新增欄位Decimal: 統一格式轉換的ICD Decimal format
  Conversion <- IcdDxShortToDecimal(DxDataFile,ICD,Date,icd10usingDate)
  DxDataFile[,Decimal := Conversion$ICD]

  ## 依照使用者選擇的分組方式選擇相對應的 PheWAS的欄位
  if(isDescription){
    PheWASCol <- "PheCodeDescription"
  }else{
    PheWASCol <- "PheCode"
  }

  ## 將ICD9跟ICD10的資料分別進行標準化分組最後再進行合併
  allPheWAS <- rbind(merge(DxDataFile[Date < icd10usingDate,], phecode_icd9_2[,c(PheWASCol,"ICD"), with = FALSE], by.x ="Decimal", by.y = "ICD", all.x = TRUE),
                       merge(DxDataFile[Date >= icd10usingDate,], phecode_icd9_2[,c(PheWASCol,"ICD"), with = FALSE], by.x ="Decimal", by.y = "ICD", all.x = TRUE))

  ## 將分組完的資料依照number的順序排成原本使用者資料的順序
  allPheWAS <- allPheWAS[order(Number),-"Number"]

  ## 當標準化分組的欄位為NA的數量小於總量 (表示有分到組)
  if(nrow(allPheWAS[is.na(eval(parse(text = paste(PheWASCol))))]) < nrow(allPheWAS)){
    ## 取有分到組的資料
    ## summarize firstDate, EndDate, count by ID and com_col, 並計算period
    summarisedPheWAS <- allPheWAS[!is.na(eval(parse(text = paste(PheWASCol)))),
                                  list(firstCaseDate = min(Date),
                                       endCaseDate = max(Date),
                                       count = .N),
                                  by = c("ID",PheWASCol)][,period := (endCaseDate - firstCaseDate),]

    return(list(groupedDT = allPheWAS,
                summarised_groupedDT = summarisedPheWAS,
                Error = Conversion$Error))
  }else{
    return(list(groupedDT = allPheWAS,
                Error = Conversion$Error))
  }
}
