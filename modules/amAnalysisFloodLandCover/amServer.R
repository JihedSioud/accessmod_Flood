#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \\ / ___// ___// /|_/ // __ \\ / __  /  /___ \\
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\\___/ \\___/ \\___//____//____//_/  /_/ \\____/ \\__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#
#    Copyright (c) 2014-present WHO, Frederic Moser (GeoHealth group, University of Geneva)
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.

idModule <- "module_toolbox"

trimNumeric <- function(value) {
  if (is.null(value) || is.na(value)) {
    return(NA_character_)
  }
  fmt <- trimws(format(value, trim = TRUE, scientific = FALSE))
  fmt <- sub("0+$", "", fmt)
  fmt <- sub("\\.$", "", fmt)
  fmt
}

suffixFriendly <- function(value) {
  if (is.na(value) || is.null(value)) {
    return(NA_character_)
  }
  gsub("[^0-9A-Za-z]+", "_", value)
}

safeNumeric <- function(x) {
  if (is.null(x)) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(x))
}

buildIntervalMetadata <- function(thresholds, speeds) {
  n <- length(speeds)
  offsets <- seq(0, by = 5000, length.out = n)
  fmt <- if (length(thresholds) > 0) {
    vapply(thresholds, trimNumeric, character(1))
  } else {
    character(0)
  }
  fmtSuffix <- if (length(fmt) > 0) {
    vapply(fmt, suffixFriendly, character(1))
  } else {
    character(0)
  }
  labelsSuffix <- character(n)
  labelsHuman <- character(n)
  if (n >= 1) {
    labelsSuffix[1] <- paste0("Under_", fmtSuffix[1])
    labelsHuman[1] <- if (!is.na(fmt[1])) sprintf("< %s", fmt[1]) else "< ?"
  }
  if (n > 2) {
    for (i in 2:(n - 1)) {
      labelsSuffix[i] <- paste0(fmtSuffix[i - 1], "_to_", fmtSuffix[i])
      labelsHuman[i] <- if (!any(is.na(fmt[c(i - 1, i)]))) {
        sprintf("%s - %s", fmt[i - 1], fmt[i])
      } else {
        "?"
      }
    }
  }
  if (n >= 2) {
    labelsSuffix[n] <- paste0("Over_", fmtSuffix[n - 1])
    labelsHuman[n] <- if (!is.na(fmt[n - 1])) sprintf(">= %s", fmt[n - 1]) else ">= ?"
  }
  list(
    thresholds = thresholds,
    speeds = speeds,
    offsets = offsets,
    labelSuffix = labelsSuffix,
    labelHuman = labelsHuman,
    labelFormatted = fmt
  )
}

buildFloodExpression <- function(outName, srcName, thresholds, values) {
  expr <- as.character(values[length(values)])
  if (length(thresholds) > 0) {
    thresholdFmt <- vapply(
      thresholds,
      function(x) trimws(format(x, trim = TRUE, scientific = FALSE)),
      character(1)
    )
    for (idx in seq_along(thresholds)) {
      revIdx <- length(thresholds) - idx + 1
      comparator <- if (revIdx == 1) "<" else "<="
      expr <- sprintf(
        "if(%s %s %s, %d, %s)",
        srcName,
        comparator,
        thresholdFmt[revIdx],
        values[revIdx],
        expr
      )
    }
  }
  sprintf(
    "%s = if(isnull(%s), %d, %s)",
    outName,
    srcName,
    values[1],
    expr
  )
}

intervalLabelPreview <- function(index, thresholds) {
  if (length(thresholds) == 0) {
    return("?")
  }
  fmt <- vapply(thresholds, trimNumeric, character(1))
  if (index == 1) {
    return(ifelse(is.na(fmt[1]), "< ?", sprintf("< %s", fmt[1])))
  }
  if (index > length(thresholds)) {
    last <- fmt[length(thresholds)]
    return(ifelse(is.na(last), ">= ?", sprintf(">= %s", last)))
  }
  prev <- fmt[index - 1]
  curr <- fmt[index]
  if (any(is.na(c(prev, curr)))) {
    return("?")
  }
  sprintf("%s - %s", prev, curr)
}

# Ensure interval count initialised
observe({
  if (is.null(listen$floodLcIntervalCount)) {
    listen$floodLcIntervalCount <- 2
  }
}) %>% amStoreObs(idModule, "flood_lc_init_count")

# Update selectable datasets when data list changes
observeEvent(listen$dataListUpdated,
  {
    amUpdateSelectChoice(
      idData = c("rFloodMap"),
      idSelect = c("floodLcSelectFlood"),
      dataList = dataList
    )
    amUpdateSelectChoice(
      idData = c("rLandCoverMerged"),
      idSelect = c("floodLcSelectLandCover"),
      dataList = dataList
    )
    amUpdateSelectChoice(
      idData = c("tScenario"),
      idSelect = c("floodLcSelectScenario"),
      dataList = dataList
    )
    amUpdateSelectChoice(
      idData = c("tLandCover"),
      idSelect = c("floodLcSelectLandCoverTable"),
      dataList = dataList
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "flood_lc_update_choices")

# Interval add/remove handlers
observeEvent(input$floodLcAddInterval,
  {
    count <- isolate(listen$floodLcIntervalCount)
    if (is.null(count) || length(count) == 0 || !is.numeric(count) || any(!is.finite(count))) {
      count <- 2
    }
    count <- max(2, min(10, as.integer(count)))
    if (count < 10) {
      listen$floodLcIntervalCount <- count + 1
    }
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "flood_lc_add_interval")

observeEvent(input$floodLcRemoveInterval,
  {
    count <- isolate(listen$floodLcIntervalCount)
    if (is.null(count) || length(count) == 0 || !is.numeric(count) || any(!is.finite(count))) {
      count <- 2
    }
    count <- max(2, min(10, as.integer(count)))
    if (count > 2) {
      listen$floodLcIntervalCount <- count - 1
    }
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "flood_lc_remove_interval")

# Toggle add/remove buttons based on interval count
observe(
  {
    count <- listen$floodLcIntervalCount
    if (is.null(count) || length(count) == 0 || !is.numeric(count) || any(!is.finite(count))) {
      count <- 2
    }
    count <- max(2, min(10, as.integer(count)))
    listen$floodLcIntervalCount <- count
    amActionButtonToggle("floodLcAddInterval", session, disable = count >= 10)
    amActionButtonToggle("floodLcRemoveInterval", session, disable = count <= 2)
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "flood_lc_toggle_interval_btns")

# Render interval configuration table
output$floodLcIntervals <- renderUI({
  count <- listen$floodLcIntervalCount
  if (is.null(count) || count < 2) {
    count <- 2
  }
  thresholds <- if (count > 1) {
    vapply(seq_len(count - 1), function(i) safeNumeric(input[[paste0("floodLc_threshold_", i)]]), numeric(1))
  } else {
    numeric(0)
  }
  header <- tags$thead(
    tags$tr(
      tags$th(ams("toolbox_flood_lc_interval_column")),
      tags$th(ams("toolbox_flood_lc_upper_column")),
      tags$th(ams("toolbox_flood_lc_speed_column"))
    )
  )
  rows <- lapply(seq_len(count), function(i) {
    label <- sprintf("%s %d", ams("toolbox_flood_lc_interval_label"), i)
    upperInput <- if (i < count) {
      value <- thresholds[i]
      if (is.na(value)) value <- 0
      numericInput(
        inputId = paste0("floodLc_threshold_", i),
        label = NULL,
        value = value,
        min = NULL,
        step = 0.01,
        width = "100%"
      )
    } else {
      tags$span(class = "text-muted", ams("toolbox_flood_lc_no_upper"))
    }
    speedInput <- if (i == 1) {
      tags$span(class = "text-muted", ams("toolbox_flood_lc_speed_fixed"))
    } else {
      value <- safeNumeric(input[[paste0("floodLc_speed_", i)]])
      if (is.na(value)) value <- 1
      numericInput(
        inputId = paste0("floodLc_speed_", i),
        label = NULL,
        value = value,
        min = 0,
        step = 0.1,
        width = "100%"
      )
    }
    hint <- intervalLabelPreview(i, thresholds)
    tags$tr(
      tags$td(
        tags$div(label),
        tags$small(class = "text-muted", hint)
      ),
      tags$td(upperInput),
      tags$td(speedInput)
    )
  })
  bodyRows <- if (length(rows) > 0) do.call(tagList, rows) else NULL
  tags$table(
    class = "table table-condensed table-striped",
    header,
    tags$tbody(bodyRows)
  )
})

# Validation observer
observe(
  {
    amErrorAction(title = "Flood land cover validation", {
      count <- listen$floodLcIntervalCount
      if (is.null(count) || count < 2) count <- 2
      dbCon <- isolate(grassSession$dbCon)
      thresholds <- if (count > 1) {
        vapply(seq_len(count - 1), function(i) safeNumeric(input[[paste0("floodLc_threshold_", i)]]), numeric(1))
      } else {
        numeric(0)
      }
      speeds <- if (count > 1) {
        c(1, vapply(seq(2, count), function(i) safeNumeric(input[[paste0("floodLc_speed_", i)]]), numeric(1)))
      } else {
        c(1)
      }
      err <- character(0)
      info <- character(0)
      disableGenerate <- TRUE

      hasFlood <- !is.null(amNameCheck(dataList, input$floodLcSelectFlood, "raster"))
      hasLandCover <- !is.null(amNameCheck(dataList, input$floodLcSelectLandCover, "raster"))
      hasScenario <- !is.null(amNameCheck(dataList, input$floodLcSelectScenario, "table", dbCon = dbCon))
      hasLandCoverTable <- !is.null(amNameCheck(dataList, input$floodLcSelectLandCoverTable, "table", dbCon = dbCon))
      tagsClean <- amGetUniqueTags(input$floodLcTag)
      hasTags <- isNotEmpty(tagsClean)

      if (!hasFlood) err <- c(err, ams("srv_flood_lc_missing_flood_raster"))
      if (!hasLandCover) err <- c(err, ams("srv_flood_lc_missing_landcover_raster"))
      if (!hasScenario) err <- c(err, ams("srv_flood_lc_missing_scenario_table"))
      if (!hasLandCoverTable) err <- c(err, ams("srv_flood_lc_missing_landcover_table"))
      if (!hasTags) err <- c(err, ams("srv_flood_lc_missing_tag"))

      if (length(thresholds) != (count - 1) || any(is.na(thresholds))) {
        err <- c(err, ams("srv_flood_lc_invalid_threshold"))
      } else if (any(diff(thresholds) <= 0)) {
        err <- c(err, ams("srv_flood_lc_threshold_order"))
      }

      if (length(speeds) != count || any(is.na(speeds[-1]))) {
        err <- c(err, ams("srv_flood_lc_missing_speed"))
      } else if (any(speeds[-1] <= 0)) {
        err <- c(err, ams("srv_flood_lc_speed_positive"))
      }

      msgList <- tagList()

      if (length(err) > 0) {
        errHtml <- HTML(
          paste(
            "<div>",
            icon("exclamation-triangle"),
            err,
            "</div>",
            collapse = ""
          )
        )
        msgList <- tagList(
          tags$b(ams("srv_flood_lc_validation_issues")),
          errHtml
        )
        listen$floodLcOutputNames <- NULL
        listen$floodLcIntervalsData <- NULL
      } else {
        disableGenerate <- FALSE
        intervalMeta <- buildIntervalMetadata(thresholds, speeds)
        listen$floodLcIntervalsData <- intervalMeta
        listen$floodLcTagClean <- tagsClean
        classes <- c("rFloodIntervals", "rLandCoverFloodMerged", "tLandCover", "tScenario")
        vNames <- amCreateNames(classes, tagsClean, dataList)
        listen$floodLcOutputNames <- vNames
        outputs <- HTML(
          paste(
            "<div>",
            icon("sign-out-alt"),
            vNames$html,
            "</div>",
            collapse = ""
          )
        )
        msgList <- tagList(
          tags$b(ams("srv_flood_lc_outputs_notice")),
          outputs
        )
      }

      amActionButtonToggle("btnFloodLcGenerate", session, disable = disableGenerate)
      listen$floodLcGenerateDisabled <- disableGenerate

      output$floodLcValidation <- renderUI({
        msgList
      })
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "flood_lc_validation")

# Handle generation button
observeEvent(input$btnFloodLcGenerate,
  {
    amErrorAction(title = "Generate flood land cover", {
      if (isTRUE(listen$floodLcGenerateDisabled)) {
        stop(ams("srv_flood_lc_invalid_inputs"))
      }
      outputs <- listen$floodLcOutputNames
      intervalMeta <- listen$floodLcIntervalsData
      tagsClean <- listen$floodLcTagClean
      if (is.null(outputs) || is.null(intervalMeta) || is.null(tagsClean)) {
        stop(ams("srv_flood_lc_invalid_inputs"))
      }
      dbCon <- isolate(grassSession$dbCon)
      floodMap <- amNameCheck(dataList, input$floodLcSelectFlood, "raster")
      landCoverMap <- amNameCheck(dataList, input$floodLcSelectLandCover, "raster")
      scenarioTable <- amNameCheck(dataList, input$floodLcSelectScenario, "table", dbCon = dbCon)
      landCoverTable <- amNameCheck(dataList, input$floodLcSelectLandCoverTable, "table", dbCon = dbCon)
      if (any(sapply(list(floodMap, landCoverMap, scenarioTable, landCoverTable), is.null))) {
        stop(ams("srv_flood_lc_invalid_inputs"))
      }
      floodMap <- floodMap[1]
      landCoverMap <- landCoverMap[1]
      scenarioTable <- scenarioTable[1]
      landCoverTable <- landCoverTable[1]

      pBarTitle <- ams("srv_flood_lc_progress_title")
      pbc(
        id = "flood_lc",
        visible = TRUE,
        percent = 0,
        title = pBarTitle,
        text = ams("srv_flood_lc_progress_prepare")
      )
      on.exit({
        pbc(id = "flood_lc", visible = FALSE)
        amActionButtonToggle("btnFloodLcGenerate", session, disable = FALSE)
      }, add = TRUE)
      amActionButtonToggle("btnFloodLcGenerate", session, disable = TRUE)

      thresholds <- intervalMeta$thresholds
      offsets <- intervalMeta$offsets
      labelHuman <- intervalMeta$labelHuman
      speeds <- intervalMeta$speeds

      outFlood <- outputs$file["rFloodIntervals"]
      outMerged <- outputs$file["rLandCoverFloodMerged"]
      tableLandCover <- outputs$file["tLandCover"]
      tableScenario <- outputs$file["tScenario"]

      exprFlood <- buildFloodExpression(outFlood, floodMap, thresholds, offsets)
      pbc(
        id = "flood_lc",
        visible = TRUE,
        percent = 20,
        title = pBarTitle,
        text = ams("srv_flood_lc_progress_intervals")
      )
      execGRASS(
        "r.mapcalc",
        expression = exprFlood,
        flags = c("overwrite")
      )

      rulesFile <- tempfile()
      writeLines(
        sprintf("%d:%s", offsets, labelHuman),
        con = rulesFile
      )
      execGRASS("r.category", map = outFlood, rules = rulesFile)
      colorFlood <- amClassListInfo("rFloodIntervals", "colors")
      if (isNotEmpty(colorFlood)) {
        execGRASS("r.colors", map = outFlood, color = colorFlood[1])
      }

      pbc(
        id = "flood_lc",
        visible = TRUE,
        percent = 45,
        title = pBarTitle,
        text = ams("srv_flood_lc_progress_merge")
      )
      execGRASS(
        "r.mapcalc",
        expression = sprintf("%s = %s + %s", outMerged, outFlood, landCoverMap),
        flags = c("overwrite")
      )

      landCoverDf <- dbGetQuery(dbCon, sprintf("select * from %s", landCoverTable))
      scenarioDf <- dbGetQuery(dbCon, sprintf("select * from %s", scenarioTable))

      landCoverDf$class <- as.integer(landCoverDf$class)
      landCoverDf$label <- as.character(landCoverDf$label)
      scenarioDf$class <- as.integer(scenarioDf$class)
      scenarioDf$label <- as.character(scenarioDf$label)
      scenarioDf$speed <- as.numeric(scenarioDf$speed)
      scenarioDf$mode <- as.character(scenarioDf$mode)

      labelSuffix <- intervalMeta$labelSuffix

      expandedLandCover <- do.call(rbind, lapply(seq_along(offsets), function(i) {
        offset <- offsets[i]
        suffix <- labelSuffix[i]
        data.frame(
          class = as.integer(landCoverDf$class + offset),
          label = paste(
            amSubPunct(landCoverDf$label, "_"),
            suffix,
            "FLOOD",
            sep = "_"
          ),
          stringsAsFactors = FALSE
        )
      }))

      expandedScenario <- do.call(rbind, lapply(seq_along(offsets), function(i) {
        offset <- offsets[i]
        suffix <- labelSuffix[i]
        data.frame(
          class = as.integer(scenarioDf$class + offset),
          label = paste(
            amSubPunct(scenarioDf$label, "_"),
            suffix,
            "FLOOD",
            sep = "_"
          ),
          speed = as.integer(round(scenarioDf$speed * speeds[i])),
          mode = scenarioDf$mode,
          stringsAsFactors = FALSE
        )
      }))

      pbc(
        id = "flood_lc",
        visible = TRUE,
        percent = 70,
        title = pBarTitle,
        text = ams("srv_flood_lc_progress_tables")
      )

      dbWriteTable(dbCon, tableLandCover, expandedLandCover, overwrite = TRUE, row.names = FALSE)
      dbWriteTable(dbCon, tableScenario, expandedScenario, overwrite = TRUE, row.names = FALSE)

      catRules <- tempfile()
      write.table(
        expandedLandCover[, c("class", "label")],
        file = catRules,
        row.names = FALSE,
        col.names = FALSE,
        sep = "\t",
        quote = FALSE
      )
      execGRASS("r.category", map = outMerged, rules = catRules)
      colorMerged <- amClassListInfo("rLandCoverFloodMerged", "colors")
      if (isNotEmpty(colorMerged)) {
        execGRASS("r.colors", map = outMerged, color = colorMerged[1])
      }

      amUpdateDataList(listen)
      listen$outFiles <- outputs$file

      pbc(
        id = "flood_lc",
        visible = TRUE,
        percent = 100,
        title = pBarTitle,
        text = ams("srv_flood_lc_process_done")
      )

      output$floodLcResult <- renderUI({
        tagList(
          tags$p(ams("srv_flood_lc_process_done")),
          tags$ul(HTML(paste("<li>", outputs$ui, "</li>", collapse = "")))
        )
      })

      updateTextInput(session, "floodLcTag", value = "")
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "flood_lc_generate")
