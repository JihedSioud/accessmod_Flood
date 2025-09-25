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


tags$div(
  class = "row am-tab-content",
  tagList(
    sidebarPanel(
      width = 4,
      amCenterTitle(
        title = amt(id = "toolbox_flood_lc_title"),
        sub = amt(id = "toolbox_flood_lc_subtitle"),
        h = 3
      ),
      selectInput(
        "floodLcSelectFlood",
        label = amt(id = "toolbox_flood_lc_select_flood"),
        choices = ""
      ),
      selectInput(
        "floodLcSelectLandCover",
        label = amt(id = "toolbox_flood_lc_select_landcover"),
        choices = ""
      ),
      selectInput(
        "floodLcSelectScenario",
        label = amt(id = "toolbox_flood_lc_select_scenario"),
        choices = ""
      ),
      selectInput(
        "floodLcSelectLandCoverTable",
        label = amt(id = "toolbox_flood_lc_select_landcover_table"),
        choices = ""
      ),
      tags$hr(),
      h4(amt(id = "toolbox_flood_lc_intervals_heading")),
      tags$p(
        class = "help-block",
        amt(id = "toolbox_flood_lc_intervals_hint")
      ),
      tabulator_output(
        "floodLcIntervalsTable",
        height = "250px"
      ),
      div(
        class = "btn-group",
        actionButton(
          "floodLcAddInterval",
          label = amt(id = "toolbox_flood_lc_add_interval"),
          icon = icon("plus"),
          class = "btn btn-default"
        ),
        actionButton(
          "floodLcRemoveInterval",
          label = amt(id = "toolbox_flood_lc_remove_interval"),
          icon = icon("minus"),
          class = "btn btn-default"
        )
      ),
      tags$hr(),
      textInput(
        "floodLcTag",
        label = amt(id = "toolbox_flood_lc_tag_label"),
        value = ""
      ),
      uiOutput("floodLcValidation"),
      actionButton(
        "btnFloodLcGenerate",
        label = amt(id = "toolbox_flood_lc_generate_btn"),
        icon = icon("magic"),
        class = "btn btn-primary"
      )
    ),
    div(
      class = "col-xs-12 col-md-8 col-lg-6",
      h4(amt(id = "toolbox_flood_lc_result_heading")),
      uiOutput("floodLcResult")
    )
  )
)
