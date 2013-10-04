#' Load the merged scleroderma data.
#'
#' @export
#' @author Peter Schulam <pschulam@gmail.com>
#' 
sclero_merged <- function() {
  data(list=c("patient", "clinic", "pft", "sero"))
  
  df <- merge(clinic, pft, by=c("patient.id", "date"), all=TRUE)
  df <- merge(df, sero, by=c("patient.id", "date"), all=TRUE)
  df <- merge(patient, df, by="patient.id")

  return(df)
}
