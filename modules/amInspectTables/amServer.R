#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \\ / ___// ___// /|_/ // __ \\ / __  /  /___ \\
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/

#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care

#    Copyright (c) 2014-present WHO, Frederic Moser (GeoHealth group, University of Geneva)

#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.


tableClasses <- names(Filter(function(x) {
  type <- tryCatch(x[["type"]], error = function(e) NA_character_)
  if (length(type) > 0) {
    type <- type[[1]]
  }
  identical(type, "table")
}, config$dataClassList))

# Update selectable tables whenever data list changes
observeEvent(listen$dataListUpdated,
  {
    amUpdateSelectChoice(
      idData = tableClasses,
      idSelect = c("inspectTableSelect"),
      dataList = dataList
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "inspect_tables_update_choices")

# Display helper message
renderInspectTableMessage <- function(textId = NULL, details = NULL, class = "callout callout-info") {
  if (is.null(textId) && is.null(details)) {
    return(NULL)
  }
  msg <- tagList()
  if (!is.null(textId)) {
    msg <- tagList(msg, tags$p(class = class, amt(id = textId)))
  }
  if (!is.null(details)) {
    msg <- tagList(msg, tags$p(class = "help-block", details))
  }
  msg
}

output$inspectTableMessage <- renderUI({
  renderInspectTableMessage("inspect_tables_no_selection")
})

# Render table preview
observe(
  {
    amErrorAction(title = "Inspect table preview", {
      tableSel <- input$inspectTableSelect
      limit <- input$inspectTableLimit
      if (is.null(limit) || !is.numeric(limit) || is.na(limit) || limit < 1) {
        limit <- config$maxRowPreview
      }
      limit <- min(as.integer(round(limit)), 1000L)

      dbCon <- isolate(grassSession$dbCon)
      tableName <- amNameCheck(dataList, tableSel, "table", dbCon = dbCon)

      if (isEmpty(tableName)) {
        output$inspectTableMessage <- renderUI({
          renderInspectTableMessage("inspect_tables_no_selection")
        })
        output$inspectTablePreview <- render_tabulator({
          tabulator(
            data = data.frame(message = character(0)),
            readOnly = TRUE
          )
        })
        return()
      }

      query <- sprintf("SELECT * FROM %s LIMIT %d", tableName, limit)
      preview <- tryCatch(
        dbGetQuery(dbCon, query),
        error = function(e) {
          amDebugMsg(sprintf("Failed to read table %s: %s", tableName, e$message))
          NULL
        }
      )

      if (is.null(preview)) {
        output$inspectTableMessage <- renderUI({
          renderInspectTableMessage(details = sprintf("Unable to read table %s", tableName))
        })
        output$inspectTablePreview <- render_tabulator({
          tabulator(
            data = data.frame(message = character(0)),
            readOnly = TRUE
          )
        })
        return()
      }

      nShown <- nrow(preview)
      summaryText <- sprintf("Showing %d row%s (limit %d)", nShown, ifelse(nShown == 1, "", "s"), limit)

      output$inspectTablePreview <- render_tabulator({
        tabulator(
          data = preview,
          readOnly = TRUE,
          option = list(layout = "fitColumns")
        )
      })

      output$inspectTableMessage <- renderUI({
        renderInspectTableMessage(details = summaryText, class = "callout callout-info")
      })
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "inspect_tables_preview")
