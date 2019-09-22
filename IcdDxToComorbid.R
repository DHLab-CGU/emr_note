#' @rdname DxComorbid
#' @export
#'
IcdDxToComorbid <- function(DxDataFile, idColName, icdColName, dateColName, icd10usingDate, comorbidMethod, isDescription = FALSE){

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

  ## 依照使用者選擇的comorbidMethod選擇相對應的 comorbid dataset
  comorbidMethod <- tolower(deparse(substitute(comorbidMethod)))
  if(comorbidMethod == "ahrq"){
    comorbidMap9 <- `icd9_ahrq`
    comorbidMap10 <- `icd10_ahrq`
  }else if(comorbidMethod == "charlson"){
    comorbidMap9 <- `icd9_charlson`
    comorbidMap10 <- `icd10_charlson`
  }else if(comorbidMethod == "elix"){
    comorbidMap9 <- `icd9_elix`
    comorbidMap10 <- `icd10_elix`
  }else{
    stop("'please enter AHRQ, Charlson or Elix for 'comorbidMethod'", call. = FALSE)
  }

  ## 依照使用者選擇的分組方式選擇相對應的 comorbid dataset的欄位
  if (isDescription){
    com_col <- "Description"
  }else{
    com_col <- "Comorbidity"
  }

  ## 將ICD9跟ICD10的資料分別進行標準化分組最後再進行合併
  allComorbid <- rbind(merge(DxDataFile[Date < icd10usingDate], comorbidMap9[,c("ICD",com_col),with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                       merge(DxDataFile[Date >= icd10usingDate], comorbidMap10[,c("ICD",com_col),with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))

  ## 將分組完的資料依照number的順序排成原本使用者資料的順序
  allComorbid <- allComorbid[order(Number),-"Number"]

  ## 當標準化分組的欄位為NA的數量小於總量 (表示有分到組)
  if(nrow(allComorbid[is.na(eval(parse(text = paste(com_col))))]) < nrow(allComorbid)){
    ## 取有分到組的資料
    ## summarize firstDate, EndDate, count by ID and com_col, 並計算period
    summarisedComorbid <- allComorbid[!is.na(eval(parse(text = paste(com_col)))),
                                      list(firstCaseDate = min(Date),
                                           endCaseDate = max(Date),
                                           count = .N),
                                      by = c("ID",com_col)][,period := (endCaseDate - firstCaseDate),][order(ID),]

    return(list(groupedDT = allComorbid,
                summarised_groupedDT = summarisedComorbid,
                Error = Conversion$Error))

  }else{
    return(list(groupedDT = allComorbid,
                Error = Conversion$Error))
  }
}
