#' @export
#' @rdname PrCCS
#'
IcdPrToCCSLvl <- function(PrDataFile, idColName, icdColName, dateColName, icd10usingDate, CCSLevel = 1, isDescription = TRUE){

  PrDataFile <- as.data.table(PrDataFile)

  ## 將使用者傳入三個欄位名稱依序更改為ID, ICD, Date
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  PrDataFile <- PrDataFile[,DataCol,with = FALSE]
  names(PrDataFile) <- c("ID","ICD", "Date")

  ## Date 統一轉成date的格式
  ## 新增Number欄位, 為了最後標準化分組完的資料能依照原始資料(DxDataFile)的排列順序
  PrDataFile[,c("Date", "Number") := list(as.Date(Date), 1:nrow(PrDataFile))]

  ## 將PrDataFile的ICD進行統一格式的轉換
  ## 新增欄位Short: 統一格式轉換的ICD short format
  Conversion <- IcdPrDecimalToShort(PrDataFile,ICD,Date,icd10usingDate)
  PrDataFile[,Short := Conversion$ICD]

  ## 依照使用者選擇的分組方式選擇相對應的 CCS的欄位
  if(isDescription){
    CCSLvlCol <- paste0("CCS_LVL_", CCSLevel, "_LABEL")
  }else{
    CCSLvlCol <- paste0("CCS_LVL_", CCSLevel)
  }

  ##由於ICD10的CCS lvl是1~2, 所以設條件 CCS小於等於2時會進行ICD9 + ICD10的標準化分組, 大於2則只進行ICD9的標準化分組
  if(CCSLevel <= 2){
    ## 將ICD9跟ICD10的資料分別進行標準化分組最後再進行合併
    IcdToCCSLvl <- rbind(merge(PrDataFile[Date < icd10usingDate],ccsPrICD9[,c("ICD", CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                         merge(PrDataFile[Date >= icd10usingDate],ccsPrICD10[,c("ICD", CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))
  }else{
    ## 將ICD9資料進行標準化分組最後再和icd10的資料進行合併
    IcdToCCSLvl <- merge(merge(PrDataFile[Date < icd10usingDate],ccsPrICD9[,c("ICD", CCSLvlCol), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                         PrDataFile[Date >= icd10usingDate], by = names(PrDataFile), all = TRUE)
  }

  ## 將分組完的資料依照number的順序排成原本使用者資料的順序
  IcdToCCSLvl <- IcdToCCSLvl[order(Number),-"Number"]

  return(list(groupedDT = IcdToCCSLvl,
              Error = Conversion$Error))
}
