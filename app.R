library(shiny)
library(htmltools)
library(markdown)
library(tidyverse)
library(readxl)
library(scales)
library(DT)
library(magrittr)
library(data.table)
library(shinyWidgets)

Krs <- read_excel("./Krs.xlsx") %>%
  mutate(Publication_year = (gsub("\\D", "", Reference)),
         Growth_form = factor(Growth_form, 
                              levels = c("Crop dicot", "Graminoid", "Tree", 
                                         "Shrub", "Succulent", "Forb", "Other")),
         TT = case_when(Treatment_type1 == "Other" ~ "Other",
                        Experimental_treatment == "No treatment" ~ "No treatment",
                        Treatment_type2 == "Other" ~ Treatment_level1,
                        T ~ paste(Treatment_level1,Treatment_level2)),
         TT = str_replace(TT, " NA", ""),
         TT = str_replace(TT, "Control Control", "Control")) %>%
  arrange(Growth_form, PFT) %>%
  mutate(PFT = factor(PFT, levels = unique(PFT))) %>%
  filter(Value_type == "treatment average") %>%
  select(-Value_type) 
kr <- read_excel("./kroot_kr.xlsx") %>%
  mutate(Publication_year = (gsub("\\D", "", Reference)),
         Growth_form = factor(Growth_form, 
                              levels = c("Crop dicot", "Graminoid", "Tree", 
                                         "Shrub", "Succulent", "Forb")),
         TT = case_when(Treatment_type1 == "Other" ~ "Other",
                        Experimental_treatment == "No treatment" ~ "No treatment",
                        Treatment_type2 == "Other" ~ Treatment_level1,
                        T ~ paste(Treatment_level1,Treatment_level2)),
         TT = str_replace(TT, " NA", ""),
         TT = str_replace(TT, "Control Control", "Control")) %>%
  arrange(Growth_form, PFT) %>%
  mutate(PFT = factor(PFT, levels = unique(PFT)),
         radial_axial = "kroot") %>%
  filter(Value_type == "treatment average") 
kx <- read_excel("./kx.xlsx") %>%
  mutate(Publication_year = (gsub("\\D", "", Reference)),
         TT = case_when(Treatment_type1 == "Other" ~ "Other",
                        Experimental_treatment == "No treatment" ~ "No treatment",
                        Treatment_type2 == "Other" ~ Treatment_level1,
                        T ~ paste(Treatment_level1,Treatment_level2)),
         TT = str_replace(TT, " NA", ""),
         TT = str_replace(TT, "Control Control", "Control")) %>%
  arrange(Growth_form, PFT) %>%
  mutate(PFT = factor(PFT, levels = unique(PFT)),
         radial_axial = "kx") %>%
  filter(Value_type == "treatment average")
Lpr_kx <- bind_rows(kr, kx) %>%
  select(-Value_type)
conductances <- list(Krs, Lpr_kx)
names(conductances) <-c("whole root system", "individual roots; root segments")
selection <- bind_rows(Krs %>%
                         select(PFT, Genus, Species, Driving_force),
                       Lpr_kx %>%
                         select(PFT, Genus, Species, Driving_force))
ui <- fluidPage(
  h1("Root hydraulic properties explorer"),
  sidebarLayout(
    sidebarPanel(
      titlePanel("Data selection"),
      selectInput('tissue', h3('1.Select by root tissue type'), 
                  choices = names(conductances)),
      conditionalPanel(condition = "input.tissue == 'individual roots; root segments'",
                       selectInput("radax", "Select the root hydraulic property",
                                   choices = c("kroot", "kx"), 
                                   multiple = F)),
      selectInput("criteria", h3("2. Select by species, genus or PFT"),
                  choices = c("Show all data", "Filter by species", 
                              "Filter by genus", "Filter by PFT")),
      conditionalPanel(condition = "input.criteria == 'Filter by PFT'",
                       selectInput("PFT", "Select the plant functional group",
                                   choices = unique(selection$PFT), 
                                   multiple = T)),
      conditionalPanel(condition = "input.criteria == 'Filter by genus'",
                       selectInput("Genus", "Select the genus",
                                   choices = unique(selection$Genus), 
                                   multiple = T)),
      conditionalPanel(condition = "input.criteria == 'Filter by species'",
                       selectInput("Species", "Select the species",
                                   choices = unique(selection$Species), 
                                   multiple = T)),
      uiOutput("picker"),
      selectInput("force", h3("3. Select by driving force used for measurement"),
                  unique(selection$Driving_force)),
      titlePanel("Data download"),
      downloadButton("downloadzip", "Download the entire dataset"),
      downloadButton('download',"Download all selected data"),
      downloadButton('download_sum',"Download data summary")),
    mainPanel(
      navbarPage(
        "",
        navbarMenu(
          strong("Data"),
          tabPanel(
            title = "All selected data",
            DTOutput("selected_table")
          ),
          tabPanel(
            title = "Data summary",
            DTOutput("summary_table")
          )
        ),
        navbarMenu(
          strong("Range of variability plots"),
          tabPanel(
            title = "Total reported range (Krs, kroot or kx)",
            plotly::plotlyOutput("range_plot",
                                 width = "100%",
                                 height = "700px")),
          textOutput("explanation_range"),
          tabPanel(
            title = "Total reported range (Krs_norm)",
            uiOutput("norm_plot"),
            textOutput("explanation_norm")),
          tabPanel(
            title = "Differences among PFTs",
            plotly::plotlyOutput("PFT_plot",
                                 width = "100%",
                                 height = "700px"),
            textOutput("explanation_PFT")
          )
        ),
          navbarMenu(
            strong("Definitions"),
            tabPanel(
              title = "Root hydraulic properties",
              includeHTML("./symbol_description.htm")
            ),
            tabPanel(
              title = "PFT classification",
              includeHTML("./PFTs.htm"))
          ),
          navbarMenu(
            strong("About the Database"),
            tabPanel(title = "Description and original data source",
                     h5(strong("Description")),
                     h5(HTML("The data presented in this database corresponds to a
                        systematic review of scientific articles in which root
                        hydraulic properties were determined empirically. The relevant
                        data presented in th publications was digitilized manually and
                        can be easily accesed thorugh this application. <br />
                        <br />
                        Data users are requested to acknowledge the original data source. 
                        The list of all reviewed publications can be accessed here")),
                     downloadButton('download_pub',"Download publication list")),
            tabPanel(title = "How to cite and license",
                     h4("How to cite"),
                     h5(strong("Root hydraulic properties: 
                   an exploration of their variability across scales")),
                     h5("JC Baca Cabrera, J Vanderborght1, V Couvreur, 
                             D Behrend, T Gaiser, TH Nguyen, G Lobet"),
                     h4(HTML("<br />License")),
                     h5(HTML(paste0("This database and the associated tools for exploration,
                                  visualization and data export are made available under the 
                                  Open Data Commons Attribution License<br />",
                                    tags$a(href='http://opendatacommons.org/licenses/by/1.0/',
                                           "http://opendatacommons.org/licenses/by/1.0/"),
                                    "<br /><br /> This means that your are free to:<br />
                                  <ul><li><em>share</em>: copy, distribute and use the database</li>
                                  <li><em>create</em>: produce works from the databaset</li>
                                  <li><em>adapt</em>: modify, transform and build upon the database,</li></ul>
                                  as long as you <em>attribute</em> any public use of the database, 
                                  or works produced from the database, in the manner specified in the license")))),
            tabPanel(title = "Data contribution and contact",
                     h4(HTML("Do you want to contribute?")),
                     h5(HTML(paste0("If you have any comments or would like to contribute data to the database,
                                    please contact us at the following e-mail adress: j.baca.cabrera@fz-juelich.de
                                    <br /><br />Alternatively, you can directly upload your data by using 
                                    the following link:<br />",
                                    tags$a(href='https://docs.google.com/spreadsheets/d/1xbqeJLD9vS2yBS9isxN90S8HcKW833qoLTQUAfpjMJI/edit?usp=sharing',
                                           "Data contribution")))))
                                )))))
  
  server <- function(input, output, session) {
    filt1 <- reactive({
      if (input$tissue == "whole root system") {
        conductances2 <- conductances %>%
          extract(input$tissue) %>%
          as.data.table(.)
        colnames(conductances2) <- gsub(".*\\.", "", colnames(conductances2))
        conductances2 %>%
          filter(Driving_force %in% input$force) %>%
          select(-Driving_force)
      }
      else {
        conductances2 <- conductances %>%
          extract(input$tissue) %>%
          as.data.table(.)
        colnames(conductances2) <- gsub(".*\\.", "", colnames(conductances2))
        conductances2 %>%
          filter(Driving_force %in% input$force,
                 radial_axial %in% input$radax) %>%
          select(-Driving_force)
      }
    })
    filtered <- reactive({
      if (input$criteria == "Show all data") {
        filt1() 
      }
      else if (input$criteria == "Filter by PFT") {
        filt1() %>%
          filter(PFT %in% input$PFT)
      }
      else if (input$criteria == "Filter by genus") {
        filt1() %>%
          filter(Genus %in% input$Genus)
      }
      else {
        filt1() %>%
          filter(Species %in% input$Species)
      }
    })
    ref_species  <- reactive({
      if (input$tissue == "whole root system") {
        filtered() %>%
          group_by(Reference, Species, Genus, PFT, TT) %>%
          summarise(Krs = mean(Krs),
                    Krs_area = mean(Krs_area),
                    Krs_DW = mean(Krs_DW),
                    Krs_FW = mean(Krs_FW),
                    Krs_length = mean(Krs_length)) %>%
          mutate(Treatment = ifelse(TT %in% c("Control", "Control NA", "Control Control",
                                              "Other", "No treatment"), 
                                    "Control", "Non control")) %>%
          ungroup(.) %>%
          select(-TT)
      }
      else if (input$tissue == "individual roots; root segments" &
               input$radax == "kroot") {
        filtered() %>%
          group_by(Reference, Species, Genus, PFT, TT, Root_section) %>%
          summarise(kroot = mean(kroot)) %>%
          mutate(Treatment = ifelse(TT %in% c("Control", "Control NA", "Control Control",
                                              "Other", "No treatment"), 
                                    "Control", "Non control")) %>%
          ungroup(.) %>%
          select(-TT)
      }
      else {
        filtered() %>%
          group_by(Reference, Species, Genus, PFT, TT, Root_section) %>%
          summarise(kx = mean(kx), 
                    kx_cs = mean(kx_cs)) %>%
          mutate(Treatment = ifelse(TT %in% c("Control", "Control NA", "Control Control",
                                              "Other", "No treatment"), 
                                    "Control", "Non control")) %>%
          ungroup(.) %>%
          select(-TT)
      }
    })
    PFT_averages <- reactive({
      if (input$tissue == "whole root system") {
        ref_species() %>%
          pivot_longer(cols = c(Krs:Krs_area)) %>%
          group_by(PFT, name) %>% 
          summarise(count = n(),
                    minimum = min(value, na.rm = T),
                    average = mean(value, na.rm = T),
                    maximum = max(value, na.rm = T)) %>%
          filter(average != is.na(average))
      }
      else if (input$tissue == "individual roots; root segments" &
               input$radax == "kroot") {
        ref_species() %>%
          group_by(PFT) %>% 
          summarise(count = n(),
                    minimum = min(kroot, na.rm = T),
                    average = mean(kroot, na.rm = T),
                    maximum = max(kroot, na.rm = T)) %>%
          filter(average != is.na(average))
      }
      else {
        ref_species() %>%
          pivot_longer(cols = kx:kx_cs) %>%
          group_by(PFT, name) %>% 
          summarise(count = n(),
                    minimum = min(value, na.rm = T),
                    average = mean(value, na.rm = T),
                    maximum = max(value, na.rm = T)) %>%
          filter(average != is.na(average))
      }
    })
    column_sel <- reactive({
      if (input$tissue == "whole root system") {
        c("Reference", "Species", "Genus", "Family", "PFT", "Krs", 
          "Krs_area", "Krs_DW", "Krs_FW", "Krs_vol",
          "Experimental_treatment", "Measurement_Method")
      }
      else if (input$tissue == "individual roots; root segments" &
               input$radax == "kroot") {
        c("Reference", "Species", "Genus", "Family", "PFT", "kroot", 
          "Experimental_treatment", "Measurement_Method", "kr_kroot")
      }
      else {
        c("Reference", "Species", "Genus", "Family","PFT", "kx", 
          "kx_cs", "Experimental_treatment", "Measurement_Method")
      }
    })
    output$picker <- renderUI({
      pickerInput(inputId = 'pick', 
                  label = 'Choose',
                  selected =  column_sel(),
                  choices = colnames(filtered() %>% 
                                       select(-TT,
                                              -PFT_level1, -PFT_level2)),
                  options = list(`actions-box` = TRUE), multiple = T)
    })
    #all selected data (for export)
    output$selected_table <- renderDT({filtered() %>% 
        select(input$pick)
    })
    output$download <- downloadHandler(
      filename = function() {"root_hydraulics.csv"},
      content = function(file) {
        write.csv(filtered() %>% 
                    select(input$pick), file, row.names = FALSE)
      }
    )
    #Summary tables
    data_summary <- reactive({
      if (input$tissue == "whole root system") {
        ref_species() %>%
          ungroup(.) %>%
          filter(Krs != is.na(Krs) |
                   Krs_area != is.na(Krs_area) |
                   Krs_DW != is.na(Krs_DW)) 
      }
      else if (input$tissue == "individual roots; root segments" &
               input$radax == "kroot") {
        ref_species() %>%
          ungroup(.) %>%
          filter(kroot != is.na(kroot)) 
      } 
      else {
        ref_species() %>%
          ungroup(.) %>%
          filter(kx != is.na(kx) |
                   kx_cs != is.na(kx_cs)) 
      } 
    })
    output$summary_table <- 
      renderDT({data_summary()
      })
    output$download_sum <- downloadHandler(
      filename = function() {"root_hydraulics.csv"},
      content = function(file) {
        write.csv(data_summary(), 
                  file, row.names = FALSE)
      }
    )
    output$downloadzip <- downloadHandler(
      filename <- function() {"output.zip"},
      content <- function(file) {
        file.copy("dataset.zip", file)
      }
    )
    output$download_pub <- downloadHandler(
      filename <- function() {"publication_list.xlsx"},
      content <- function(file) {
        file.copy("publications.xlsx", file)
      }
    )
    #Range of variation plots of the different root hydraulic properties
    output$range_plot <-
      plotly::renderPlotly({
        if (input$tissue == "whole root system") {
          filtered() %>% 
            group_by(Reference, Species, PFT) %>%
            summarise(Krs = mean(Krs, na.rm = T)) %>%
            filter(Krs != is.na(Krs)) %>%
            mutate(`Reference and species` = paste(Reference, Species, 
                                                   sep = "\n")) %>%
            ggplot(aes(PFT, Krs,
                       label = `Reference and species`,
                       color = PFT)) +
            geom_point(size = 2) +
            scale_color_manual(values = c("#bae4b3", "#bae4b3", "#74c476", "#74c476",
                                          "#006d2c", "#006d2c","#006d2c", "#d8b365",
                                          "#636363")) +
            scale_y_log10() +
            coord_flip() +
            theme_bw() +
            theme(legend.position = "none",
                  axis.title.y = element_blank(),
                  axis.title.x = element_text(size = 12),
                  axis.text.y = element_text(size = 11),
                  axis.text.x = element_text(size = 10),
                  strip.text = element_text(size = 10)) +
            ggtitle("Range of variation in Krs (m3 MPa-1 s-1)")
        }
        else if (input$tissue == "individual roots; root segments" &
                 input$radax == "kroot") {
          filtered() %>% 
            group_by(Reference, Species, PFT) %>%
            summarise(kroot = mean(kroot, na.rm = T)) %>%
            filter(kroot != is.na(kroot)) %>%
            mutate(`Reference and species` = paste(Reference, Species, 
                                                   sep = "\n")) %>%
            ggplot(aes(PFT, kroot,
                       label = `Reference and species`,
                       color = PFT)) +
            geom_point(size = 2) +
            scale_color_manual(values = c("#bae4b3", "#bae4b3", "#74c476", "#74c476",
                                          "#006d2c", "#d8b365", "#5ab4ac", "#636363")) +
            scale_y_log10() +
            coord_flip() +
            theme_bw() +
            theme(legend.position = "none",
                  axis.title.y = element_blank(),
                  axis.title.x = element_text(size = 12),
                  axis.text.y = element_text(size = 11),
                  axis.text.x = element_text(size = 10),
                  strip.text = element_text(size = 10)) +
            ggtitle("Range of variation in kroot (m MPa-1 s-1)")
        }
        else {
          filtered() %>% 
            pivot_longer(cols = c(kx, kx_cs)) %>%
            group_by(Reference, Species, PFT, name) %>%
            summarise(cond = mean(value, na.rm = T)) %>%
            filter(cond != is.na(cond)) %>%
            mutate(`Reference and species` = paste(Reference, Species, 
                                                   sep = "\n"),
                   PFT = factor(PFT, levels = c("Crop herbaceous", "C3 grass", "C4 grass",
                                                "Broadleaf tree", "Needle tree", "Tropical tree",
                                                "Shrub", "Succulent"))) %>%
            ggplot(aes(PFT, cond, label = `Reference and species`,
                       color = PFT)) +
            geom_point(size = 1.5) +
            scale_color_manual(values = c("#bae4b3", "#74c476", "#74c476","#006d2c", 
                                          "#006d2c","#006d2c", "#d8b365", "#5ab4ac")) +
            scale_y_log10() +
            facet_wrap(~name, scales = "free_x") +
            coord_flip() +
            theme_bw() +
            theme(legend.position = "none",
                  axis.title = element_blank(),
                  axis.text.y = element_text(size = 11),
                  axis.text.x = element_text(size = 10),
                  strip.text = element_text(size = 11)) +
            ggtitle("Range of variation in kx (m4 MPa-1 s-1) and kx_cs (m4 MPa-1 s-1)")
        }
      })
    output$norm_plot <- renderUI({
      if (input$tissue == "whole root system") {
        plotly::plotlyOutput("range_Krs",
                             width = "100%",
                             height = "700px")
      } 
      else {
        textOutput("warning_Krs") 
      }
    })
output$range_Krs <- 
  plotly::renderPlotly({
    filtered() %>%
      pivot_longer(cols = c(Krs_area, Krs_DW, Krs_FW, Krs_length)) %>% 
      group_by(Reference, Species, PFT, name) %>%
      summarise(cond = mean(value, na.rm = T)) %>%
      filter(cond != is.na(cond)) %>%
      mutate(`Reference and species` = paste(Reference, Species, 
                                             sep = "\n")) %>%
      ggplot(aes(PFT, cond,
                 label = `Reference and species`,
                 color = PFT)) +
      geom_point(size = 2) +
      scale_color_manual(values = c("#bae4b3", "#bae4b3", "#74c476", "#74c476",
                                    "#006d2c", "#006d2c","#006d2c", "#d8b365",
                                    "#636363")) +
      scale_y_log10() +
      facet_wrap(~name, scales = "free_x") +
      coord_flip() +
      theme_bw() +
      theme(legend.position = "none",
            axis.title = element_blank(),
            axis.text.y = element_text(size = 11),
            axis.text.x = element_text(size = 10),
            strip.text = element_text(size = 11)) +
      ggtitle("Range of variation in Krs_area, Krs_DW, Krs_FW and Krs_length")
  })
output$warning_Krs <- renderText("This graph only applies for the
                                   whole root system dataset")
#graphical comparison between PFT's
output$PFT_plot <-
  plotly::renderPlotly({
    if (input$tissue == "individual roots; root segments" & 
        input$radax == "kroot") {
      PFT_averages() %>%
        ggplot(aes(PFT, average)) +
        geom_segment(aes(x = , xend = PFT,
                         y = minimum, yend = maximum,
                         color = PFT), 
                     linewidth = 5, alpha = .5) +
        geom_point(size = 3.5, aes(color = PFT)) +
        scale_color_manual(values = c("#bae4b3", "#bae4b3", "#74c476", "#74c476",
                                      "#006d2c", "#d8b365", "#5ab4ac", "#636363")) +
        scale_y_log10() +
        coord_flip() +
        theme_bw() +
        theme(legend.position = "none",
              axis.title.y = element_blank(),
              axis.title.x = element_text(size = 12),
              axis.text.y = element_text(size = 11),
              axis.text.x = element_text(size = 10),
              strip.text = element_text(size = 10)) +
        ggtitle("kroot averages (m MPa-1 s-1) for all PFTs")
    }
    else if (input$tissue == "whole root system") {
      PFT_averages() %>%
        ggplot(aes(PFT, average)) +
        geom_segment(aes(x = , xend = PFT,
                         y = minimum, yend = maximum,
                         color = PFT), 
                     linewidth = 5, alpha = .5) +
        geom_point(size = 3.5, aes(color = PFT)) +
        scale_color_manual(values = c("#bae4b3", "#bae4b3", "#74c476", "#74c476",
                                      "#006d2c", "#006d2c","#006d2c", "#d8b365",
                                      "#636363")) +
        scale_y_log10() +
        facet_wrap(~name, scales = "free_x") +
        coord_flip() +
        theme_bw() +
        theme(legend.position = "none",
              axis.title = element_blank(),
              axis.text.y = element_text(size = 11),
              axis.text.x = element_text(size = 10),
              strip.text = element_text(size = 11)) +
        ggtitle("Averages in Krs (m3 MPa-1 s-1) and Krs_area (m MPa-1 s-1) for all PFTs")
    }
    else {
      PFT_averages() %>%
        ggplot(aes(PFT, average)) +
        geom_segment(aes(x = , xend = PFT,
                         y = minimum, yend = maximum,
                         color = PFT), 
                     linewidth = 5, alpha = .5) +
        geom_point(size = 3.5, aes(color = PFT)) +
        scale_color_manual(values = c("#bae4b3", "#74c476", "#74c476","#006d2c", 
                                      "#006d2c","#006d2c", "#d8b365", "#5ab4ac")) +
        scale_y_log10() +
        facet_wrap(~name, scales = "free_x") +
        coord_flip() +
        theme_bw() +
        theme(legend.position = "none",
              axis.title = element_blank(),
              axis.text.y = element_text(size = 11),
              axis.text.x = element_text(size = 10),
              strip.text = element_text(size = 11)) +
        ggtitle("Averages in kx (m4 MPa-1 s-1) and kx_cs (m2 MPa-1 s-1) for all PFTs")
    }
  })
output$symbology <-
  renderTable(sym_tab)
}
  shinyApp(ui = ui, server = server)