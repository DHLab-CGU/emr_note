#' @rdname DxCustom
#' @export
#'
IcdDxToCustomGrep <- function(DxDataFile, idColName, icdColName, dateColName, CustomGroupingTable){

  GrepedIcd <- as.data.table(DxDataFile)
  CustomGroupingTable <- as.data.table(CustomGroupingTable)
  ## 將使用者傳入三個欄位名稱依序更改為ID, ICD, Date
  DataCol  <-c(deparse(substitute(idColName)), deparse(substitute(icdColName)), deparse(substitute(dateColName)))
  GrepedIcd <- GrepedIcd[,DataCol,with = FALSE]
  names(GrepedIcd) <- c("ID", "ICD", "Date")

  ## Date 統一轉成date的格式
  ## 新增Number欄位, 為了最後標準化分組完的資料能依照原始資料(DxDataFile)的排列順序
  ## 新增Group欄位, 後續進行自定義分組時, 有符合條件的組別, 預設值為NA
  GrepedIcd[,c("Date", "Number", "Group") := list(as.Date(Date), 1:nrow(GrepedIcd), NA)]

  ## 將customICD的資料進行自定義分組
  ## 當CustomGroupingTable的ICD = GrepedIcd 的 ICD, GrepedIcd地group欄位為CustomGroupingTable的group,否則為NA(預設)
  for (rule in 1:nrow(CustomGroupingTable)){
    GrepedIcd$Group<-ifelse(grepl(CustomGroupingTable[rule,"grepIcd"],GrepedIcd$ICD), CustomGroupingTable[rule,Group], GrepedIcd$Group)
  }

  ## 將分組完的資料依照number的順序排成原本使用者資料的順序
  GrepedIcd <- GrepedIcd[order(Number),-"Number"]

  ## 當分組的欄位不為NA的數量大於零 (表示有分到組)
  if(sum(!is.na(GrepedIcd$Group)) > 0){
    ## 取有分到組的資料
    ## summarize firstDate, EndDate, count by ID and com_col, 並計算period
    summarisedGrepedIcd <- GrepedIcd[nchar(Group)>0,
                                     list(firstCaseDate = min(Date),
                                          endCaseDate = max(Date),
                                          count = .N),by = list(ID,Group)][,period := (endCaseDate - firstCaseDate),][order(ID),]

    return(list(groupedDT = GrepedIcd,
                summarised_groupedDT = summarisedGrepedIcd))
  }else{
    warning("There is no match diagnostic code with the grepTable")
    return(groupedDT = GrepedIcd)
  }
}
