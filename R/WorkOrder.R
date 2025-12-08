
#' WorkOrder_select
#'
#' @param erp_token

#' @return 无返回值
#' @export
#'
#' @examples
#' WorkOrder_select()
WorkOrder_select<- function(erp_token,FWorkOrderID) {

  sql=paste0("
  select distinct * from rds_erp_vw_rpt_PNSerialNoTrace where FWorkOrderID ='",FWorkOrderID,"'
             ")

  res=tsda::sql_select2(token = erp_token,sql = sql)
  return(res)
}
