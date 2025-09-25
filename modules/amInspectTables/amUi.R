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


tags$div(
  class = "row am-tab-content",
  tagList(
    sidebarPanel(
      width = 4,
      amCenterTitle(
        title = amt(id = "inspect_tables_title"),
        h = 3
      ),
      selectInput(
        "inspectTableSelect",
        label = amt(id = "inspect_tables_select_label"),
        choices = ""
      ),
      numericInput(
        "inspectTableLimit",
        label = amt(id = "inspect_tables_rows_label"),
        value = config$maxRowPreview,
        min = 1,
        max = 1000,
        step = 5
      ),
      uiOutput("inspectTableMessage")
    ),
    div(
      class = "col-xs-12 col-md-8 col-lg-8",
      tabulator_output(
        "inspectTablePreview",
        height = "55vh"
      )
    )
  )
)

