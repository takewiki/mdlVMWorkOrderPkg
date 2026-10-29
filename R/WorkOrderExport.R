# =====================================================================
# WorkOrder - Export tab: data access
#   Source view: rds_erp_vw_rpt_PNSerialNoTrace
#   Filter conditions built on the Export tab are appended to the query,
#   so the filtering happens in the database.
# =====================================================================

#' Field list used by the Export tab
#' @export
WorkOrderExport_fields <- function() {
  data.frame(
    label = c("Work OrderID", "Work OrderId2", "Production Date", "Sales OrderID", "Delivery Location", "Sales OrderID2", "PN", "Product Name", "Serial Number", "Sales OrderQty", "Delivery Date", "Total DeliveryQty"),
    expr = c("[FWorkOrderID]", "[FWorkOrderID2]", "cast([FProductionDate] as date)", "[FSalesOrderID]", "[FDeliveryLocation]", "[FSalesOrderID2]", "[FPN]", "[FProductName]", "[FSerialNumber]", "[FSalesOrderQty]", "cast([FDeliveryDate] as date)", "[FTotalDeliveryQty]"),
    where_expr = c("[FWorkOrderID]", "[FWorkOrderID2]", "cast([FProductionDate] as date)", "[FSalesOrderID]", "[FDeliveryLocation]", "[FSalesOrderID2]", "[FPN]", "[FProductName]", "[FSerialNumber]", "[FSalesOrderQty]", "cast([FDeliveryDate] as date)", "[FTotalDeliveryQty]"),
    type = c("text", "text", "date", "text", "text", "text", "text", "text", "text", "number", "date", "number"),
    stringsAsFactors = FALSE
  )
}

#' Quote a SQL Server string literal
#' @export
WorkOrderExport_sql_string <- function(x) {
  paste0("'", gsub("'", "''", as.character(x), fixed = TRUE), "'")
}

#' WHERE built from the filter rows
#' @export
WorkOrderExport_where <- function(FWhere = "") {
  if (is.null(FWhere) || !nzchar(FWhere)) return("")
  paste0("(", FWhere, ")")
}

#' Query used by the Export tab
#' @export
WorkOrderExport_select_sql <- function(FWhere = "", cols = NULL) {
  meta <- WorkOrderExport_fields()
  if (!is.null(cols) && length(cols)) {
    meta <- meta[meta$label %in% cols, , drop = FALSE]
    if (!nrow(meta)) meta <- WorkOrderExport_fields()
  }
  sel <- paste(paste0(meta$expr, " as [", meta$label, "]"), collapse = ",\n       ")
  sql <- paste0("select ", sel, "\n  from rds_erp_vw_rpt_PNSerialNoTrace")
  w <- WorkOrderExport_where(FWhere = FWhere)
  if (nzchar(w)) sql <- paste0(sql, "\n where ", w)
  sql
}

#' Run the export query
#' @export
WorkOrderExport_select <- function(erp_token, FWhere = "", cols = NULL) {
  sql <- WorkOrderExport_select_sql(FWhere = FWhere, cols = cols)
  res <- tsda::sql_select2(token = erp_token, sql = sql)
  res
}
