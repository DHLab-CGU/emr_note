#' @rdname DxCustom
#' @export
#'
IcdDxToCustom <- function(DxDataFile, idColName, icdColName, dateColName, CustomGroupingTable){

  customICD <- as.data.table(DxDataFile)
  CustomGroupingTable <- as.data.table(CustomGroupingTable)

  ## 將使用者傳入三個欄位名稱依序更改為ID, ICD, Date
  DataCol <- c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  customICD <- customICD[,DataCol,with = FALSE]
  names(customICD) <- c("ID", "ICD", "Date")

  ## Date 統一轉成date的格式
  ## 新增Number欄位, 為了最後標準化分組完的資料能依照原始資料(DxDataFile)的排列順序
  customICD[,c("Date", "Number") := list(as.Date(Date), 1:nrow(customICD))]

  ## 將customICD的資料進行自定義分組
  ## 將分組完的資料依照number的順序排成原本使用者資料的順序
  groupedICD <- merge(customICD, CustomGroupingTable, by = "ICD", all.x = TRUE)[order(Number), -"Number"]

  ## 當分組的欄位不為NA的數量大於零 (表示有分到組)
  if(sum(!is.na(groupedICD$Group)) > 0){
    ## 取有分到組的資料
    ## summarize firstDate, EndDate, count by ID and com_col, 並計算period
    summarisedgroupedICD <- groupedICD[!is.na(Group),
                                       list(firstCaseDate = min(Date),
                                            endCaseDate = max(Date),
                                            count = .N),
                                       by = list(ID, Group)][,period := (endCaseDate - firstCaseDate),][order(ID),]
    return(list(groupedDT = groupedICD,
                summarised_groupedDT = summarisedgroupedICD))
  }else{
    warning("There is no match diagnostic code with the groupingTable")
    return(groupedDT = groupedICD)
  }
}
