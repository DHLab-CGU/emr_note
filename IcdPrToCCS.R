#' @export
#' @rdname PrCCS
#'
IcdPrToCCS <- function(PrDataFile, idColName, icdColName, dateColName, icd10usingDate, isDescription = TRUE){

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
  if (isDescription) {
    ccs_col <- "CCS_CATEGORY_DESCRIPTION"
  } else {
    ccs_col <- "CCS_CATEGORY"
  }

  ## 將ICD9跟ICD10的資料分別進行標準化分組最後再進行合併
  IcdToCCS <- rbind(merge(PrDataFile[Date <icd10usingDate],ccsPrICD9[,c("ICD",ccs_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE),
                    merge(PrDataFile[Date >=icd10usingDate],ccsPrICD10[,c("ICD",ccs_col), with = FALSE],by.x ="Short",by.y = "ICD",all.x = TRUE))

  ## 將分組完的資料依照number的順序排成原本使用者資料的順序
  IcdToCCS <- IcdToCCS[order(Number), -"Number"]

  return(list(groupedDT = IcdToCCS,
              Error = Conversion$Error))
}


