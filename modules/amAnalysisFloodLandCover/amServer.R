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

parseGrassColorSpec <- function(spec) {
  if (is.null(spec) || length(spec) == 0) {
    return(NULL)
  }
  spec <- as.character(spec[1])
  if (is.na(spec)) {
    return(NULL)
  }
  spec <- trimws(spec)
  if (!nzchar(spec)) {
    return(NULL)
  }
  parts <- strsplit(spec, "&", fixed = TRUE)[[1]]
  palette <- trimws(parts[1])
  if (!nzchar(palette)) {
    return(NULL)
  }
  extra <- parts[-1]
  flags <- character(0)
  if (length(extra) > 0) {
    flagChars <- paste(extra, collapse = "")
    flagChars <- gsub("[^A-Za-z]", "", flagChars)
    if (nzchar(flagChars)) {
      flags <- unique(strsplit(flagChars, "")[[1]])
    }
  }
  list(palette = palette, flags = flags)
}

applyGrassColor <- function(map, colorSpec) {
  parsed <- parseGrassColorSpec(colorSpec)
  if (is.null(parsed)) {
    return(invisible(NULL))
  }
  tryCatch(
    {
      execGRASS(
        "r.colors",
        map = map,
        color = parsed$palette,
        flags = parsed$flags
      )
    },
    error = function(e) {
      amDebugMsg(sprintf("Failed to apply color '%s' on map '%s': %s", colorSpec, map, e$message))
    }
  )
  invisible(NULL)
}

floodLcMinIntervals <- 2L
floodLcMaxIntervals <- 10L

floodLcDefaultIntervals <- function() {
  data.frame(
    upper = c(0, NA_real_),
    speed = c(1, 1),
    stringsAsFactors = FALSE
  )
}

sanitizeFloodLcIntervals <- function(df) {
  if (!is.data.frame(df) || nrow(df) < floodLcMinIntervals) {
    df <- floodLcDefaultIntervals()
  }
  if (!"upper" %in% names(df)) {
    df$upper <- NA_real_
  }
  if (!"speed" %in% names(df)) {
    df$speed <- 1
  }
  df <- df[, c("upper", "speed"), drop = FALSE]
  df$upper <- suppressWarnings(as.numeric(df$upper))
  df$speed <- suppressWarnings(as.numeric(df$speed))
  df$upper[!is.finite(df$upper)] <- NA_real_
  df$speed[!is.finite(df$speed)] <- NA_real_
  df <- df[seq_len(min(nrow(df), floodLcMaxIntervals)), , drop = FALSE]
  if (nrow(df) < floodLcMinIntervals) {
    df <- floodLcDefaultIntervals()
  }
  df$speed[is.na(df$speed)] <- NA_real_
  df$speed[1] <- 1
  df$upper[nrow(df)] <- NA_real_
  rownames(df) <- NULL
  df
}

floodLcIntervalsForDisplay <- function(df) {
  df <- sanitizeFloodLcIntervals(df)
  data.frame(
    interval = seq_len(nrow(df)),
    upper = df$upper,
    speed = df$speed,
    stringsAsFactors = FALSE
  )
}

collectFloodLcIntervals <- function() {
  tbl <- tabulator_to_df(input$floodLcIntervalsTable_data)
  if (is.data.frame(tbl) && nrow(tbl) > 0) {
    df <- data.frame(
      upper = tbl$upper,
      speed = tbl$speed,
      stringsAsFactors = FALSE
    )
  } else {
    df <- listen$floodLcIntervalsDf
    if (is.null(df)) {
      df <- floodLcDefaultIntervals()
    }
  }
  sanitizeFloodLcIntervals(df)
}
observe({
  if (is.null(listen$floodLcIntervalsDf)) {
    listen$floodLcIntervalsDf <- floodLcDefaultIntervals()
  }
},
  suspended = TRUE
) %>% amStoreObs(idModule, "flood_lc_init_intervals")

buildIntervalMetadata <- function(thresholds, speeds) {
  n <- length(speeds)
  offsets <- seq.int(0L, by = 5000L, length.out = n)
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
  labelsCategory <- character(n)
  if (n >= 1) {
    labelsSuffix[1] <- paste0("Under_", fmtSuffix[1])
    labelsHuman[1] <- if (!is.na(fmt[1])) sprintf("< %s", fmt[1]) else "< ?"
    labelsCategory[1] <- if (!is.na(fmt[1])) sprintf("Under_%s", fmt[1]) else "Under"
  }
  if (n > 2) {
    for (i in 2:(n - 1)) {
      labelsSuffix[i] <- paste0(fmtSuffix[i - 1], "_to_", fmtSuffix[i])
      labelsHuman[i] <- if (!any(is.na(fmt[c(i - 1, i)]))) {
        sprintf("%s - %s", fmt[i - 1], fmt[i])
      } else {
        "?"
      }
      labelsCategory[i] <- if (!any(is.na(fmt[c(i - 1, i)]))) {
        sprintf("Between_%s_and_%s", fmt[i - 1], fmt[i])
      } else {
        sprintf("Between_%d", offsets[i])
      }
    }
  }
  if (n >= 2) {
    labelsSuffix[n] <- paste0("Over_", fmtSuffix[n - 1])
    labelsHuman[n] <- if (!is.na(fmt[n - 1])) sprintf(">= %s", fmt[n - 1]) else ">= ?"
    labelsCategory[n] <- if (!is.na(fmt[n - 1])) sprintf("Over_%s", fmt[n - 1]) else "Over"
  }
  labelsSuffix[is.na(labelsSuffix) | labelsSuffix == ""] <- sprintf("Offset_%d", offsets[is.na(labelsSuffix) | labelsSuffix == ""])
  labelsHuman[is.na(labelsHuman) | labelsHuman == ""] <- sprintf("Offset %d", offsets[is.na(labelsHuman) | labelsHuman == ""])
  labelsCategory <- gsub("[^0-9A-Za-z]+", "_", labelsCategory)
  labelsCategory <- gsub("_+", "_", labelsCategory)
  labelsCategory <- gsub("^_+|_+$", "", labelsCategory)
  labelsCategory[labelsCategory == ""] <- sprintf("cat_%d", offsets[labelsCategory == ""])
  numericOnly <- grepl("^[0-9]+$", labelsCategory)
  labelsCategory[numericOnly] <- paste0("cat_", labelsCategory[numericOnly])
  list(
    thresholds = thresholds,
    speeds = speeds,
    offsets = offsets,
    labelSuffix = labelsSuffix,
    labelHuman = labelsHuman,
    labelFormatted = fmt,
    labelCategory = labelsCategory
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

# Render interval configuration table using tabulator
output$floodLcIntervalsTable <- render_tabulator({
  intervals <- isolate(floodLcIntervalsForDisplay(listen$floodLcIntervalsDf))
  tabulator(
    data = intervals,
    readOnly = FALSE,
    option = list(layout = "fitColumns"),
    columns = list(
      list(
        title = ams("toolbox_flood_lc_interval_column"),
        field = "interval",
        editor = FALSE,
        headerSort = FALSE
      ),
      list(
        title = ams("toolbox_flood_lc_upper_column"),
        field = "upper",
        sorter = "number",
        editor = "number"
      ),
      list(
        title = ams("toolbox_flood_lc_speed_column"),
        field = "speed",
        sorter = "number",
        editor = "number"
      )
    )
  )
})

updateFloodLcIntervalTable <- function(df = NULL) {
  if (is.null(df)) {
    df <- listen$floodLcIntervalsDf
  }
  df <- sanitizeFloodLcIntervals(df)
  listen$floodLcIntervalsDf <- df
  current <- tabulator_to_df(input$floodLcIntervalsTable_data)
  if (is.null(current)) {
    return(invisible(df))
  }
  proxy <- tabulator_proxy("floodLcIntervalsTable")
  display <- floodLcIntervalsForDisplay(df)
  tabulator_update_data(proxy, display)
  invisible(df)
}

# Sync backend when table data changes
observeEvent(input$floodLcIntervalsTable_data,
  {
    tbl <- tabulator_to_df(input$floodLcIntervalsTable_data)
    if (is.null(tbl) || nrow(tbl) == 0) {
      return()
    }
    current <- data.frame(
      upper = tbl$upper,
      speed = tbl$speed,
      stringsAsFactors = FALSE
    )
    sanitized <- sanitizeFloodLcIntervals(current)
    listen$floodLcIntervalsDf <- sanitized
    desired <- floodLcIntervalsForDisplay(sanitized)
    tblDisplay <- data.frame(
      interval = if ("interval" %in% names(tbl)) tbl$interval else seq_len(nrow(tbl)),
      upper = suppressWarnings(as.numeric(tbl$upper)),
      speed = suppressWarnings(as.numeric(tbl$speed)),
      stringsAsFactors = FALSE
    )
    tblDisplay$speed[1] <- 1
    tblDisplay$interval <- seq_len(nrow(tblDisplay))
    tblDisplay$upper[nrow(tblDisplay)] <- NA_real_
    if (!identical(desired, tblDisplay)) {
      updateFloodLcIntervalTable(sanitized)
    }
  },
  ignoreNULL = TRUE,
  suspended = TRUE
) %>% amStoreObs(idModule, "flood_lc_sync_table")

# Interval add/remove handlers
observeEvent(input$floodLcAddInterval,
  {
    df <- collectFloodLcIntervals()
    if (nrow(df) >= floodLcMaxIntervals) {
      return()
    }
    df <- rbind(df, data.frame(
      upper = NA_real_,
      speed = 1,
      stringsAsFactors = FALSE
    ))
    updateFloodLcIntervalTable(df)
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "flood_lc_add_interval")

observeEvent(input$floodLcRemoveInterval,
  {
    df <- collectFloodLcIntervals()
    if (nrow(df) <= floodLcMinIntervals) {
      return()
    }
    df <- df[-nrow(df), , drop = FALSE]
    updateFloodLcIntervalTable(df)
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "flood_lc_remove_interval")

# Toggle add/remove buttons based on interval count
observe(
  {
    df <- collectFloodLcIntervals()
    count <- nrow(df)
    amActionButtonToggle("floodLcAddInterval", session, disable = count >= floodLcMaxIntervals)
    amActionButtonToggle("floodLcRemoveInterval", session, disable = count <= floodLcMinIntervals)
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "flood_lc_toggle_interval_btns")

# Validation observer
observe(
  {
    amErrorAction(title = "Flood land cover validation", {
      intervals <- collectFloodLcIntervals()
      count <- nrow(intervals)
      dbCon <- isolate(grassSession$dbCon)
      thresholds <- if (count > 1) intervals$upper[seq_len(count - 1)] else numeric(0)
      speeds <- intervals$speed
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
      labelCategory <- intervalMeta$labelCategory
      labelCategory[is.na(labelCategory) | labelCategory == ""] <- sprintf("cat_%d", offsets[is.na(labelCategory) | labelCategory == ""])
      labelCategory <- gsub(":", "_", labelCategory, fixed = TRUE)
      ruleLines <- sprintf("%d:%s", as.integer(offsets), labelCategory)
      writeLines(ruleLines, con = rulesFile)
      execGRASS("r.category", map = outFlood, separator = ":", rules = rulesFile)
      colorFlood <- amClassListInfo("rFloodIntervals", "colors")
      if (isNotEmpty(colorFlood)) {
        applyGrassColor(outFlood, colorFlood[1])
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
        applyGrassColor(outMerged, colorMerged[1])
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
