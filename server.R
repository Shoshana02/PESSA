
library(shiny)


function(input, output, session) {
    # ----------------1 homepage ------------------------------------------------------------------
    observeEvent(input$homebtn, {
        updateNavbarPage(session, "path_navbar", selected = "panel2")
    })
    
    # 2.1 box1_selectinfo_pathway -----------------------------------------------------
    updateSelectizeInput(session, inputId = "select.pathway", 
                         choices = pathway, 
                         #selected = 'HALLMARK-ADIPOGENESIS', 
                         server = TRUE)
    
    observe_helpers()#Function to show a modal dialog, observing each of the help icons in the app.
    
    observeEvent(input$select.pathway,{
      output$des <- renderText({pathwaylist$desc[which(pathwaylist$pathway==input$select.pathway)]
      })#show brief description
      output$web <- renderUI({
        tagList("URL link:", a("Jump to the GSEA-MsigDB Website",href=pathwaylist$website[which(pathwaylist$pathway==input$select.pathway)]))
      })#web link
    })
    
    ## if all data inputs are not blank, run following codes. 
    submitdata_btn_check <- reactive({
        !input$select.pathway == ""
    })
    observe({
        toggleState(id = "submitdata_btn",
                    condition = submitdata_btn_check())
    })
    
    ## shinyjs disables all boxes not used yet
    shinyjs::disable(id = "pathway_tablebox")
    shinyjs::disable(id = "pathway_plotbox")
    shinyjs::disable(id = "downloaddata_btn")
 
    # 2.2 box2_pathway_table-----------------------------------------------------
    # after clicking the SUBMIT button in box1 and pass the data check, modify the form displayed according to the input
    dataset <- eventReactive(input$submitdata_btn, {
        final_dt_test <- read_select_dt(input$select.pathway) %>% 
            mutate(across(c(CA, GSE, GPL, Survival), factor)) %>% 
            dplyr::arrange(CA) %>% #sort according to CA
            mutate(PLOT = create_btns(1:nrow(.))) %>%
            dplyr::select(PLOT, everything()) %>%#put the colulmn PLOT in front of others
            tibble::remove_rownames() %>%
            tibble::column_to_rownames('PLOT')
    })
    
    output$downloaddata_btn <- downloadHandler(
        filename = function(){
            paste(input$select.pathway,"_ResultsDatatable.csv",sep="")
        },
        content = function(file){
            sep <- ","
            write.table(dataset(), file, sep=sep, row.names = FALSE)
        }
    )
    
    ## after clicking the SUBMIT button in box1, expand box2 wiz Buttons
    observeEvent(input$submitdata_btn,{
        if (input$pathway_tablebox$collapsed) {
            updateBox("pathway_tablebox", action = "toggle")
        }
        shinyjs::enable(id = "pathway_tablebox")
        shinyjs::enable(id = "downloaddata_btn")
        table_frame <- htmltools::withTags(table(class = 'display',
                                                 thead(
                                                     tr(
                                                         th(rowspan = 2, 'Median|Optimal'),
                                                         th(rowspan = 2, 'ID'),
                                                         th(rowspan = 2, 'Geneset'),
                                                         th(rowspan = 2, 'Cancer'),
                                                         th(rowspan = 2, 'Dataset'),
                                                         th(rowspan = 2, 'Platform'),
                                                         #th(rowspan = 2, 'Treatment'),
                                                         th(rowspan = 2, 'Survival'),
                                                         th(class = 'dt-center', colspan = 3, 'KM results with median cutpoint'),
                                                         th(class = 'dt-center', colspan = 3, 'KM results with optimal cutpoint'),
                                                         th(class = 'dt-center', colspan = 4, 'COX results (continuous)'),
                                                         th(class = 'dt-center', colspan = 3, 'COX results with median cutpoint'),
                                                         th(class = 'dt-center', colspan = 3, 'COX results with optimal cutpoint'),
                                                           tr(lapply(rep(c('HR','95% CI','P value','HR','95% CI','P value',
                                                                'HR','95% CI','P value','Schoenfeld P value',
                                                                'HR','95% CI','P value','HR','95% CI','P value'),1
                                                                ),th)),
                                                       
                                                     )
                                                 )))
        output$pathwayresultdt <- shiny::renderUI({
            req(input$submitdata_btn)## WIZ OR W/O THIS LINE THE SHINY CAN WORK ANYWAY
            Sys.sleep(0.5) # add a larger number if spinner does not show
            DT::datatable(
                isolate(dataset()),
                container = table_frame,
                escape = FALSE, #don't excape any HTML in the table (i.e., the action button)
                filter = "top",
                # callback=JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Filter" )'),
                rownames = TRUE,
                extensions = "FixedColumns",
                selection = "none", #turn off row selection function otherwise u will select that row when clicking on the action button
                options = list(
                    # processing = FALSE,
                    # initComplete = JS("function(setting, json) { $('div.ms-Spinner.root-54').remove(); }"),
                    # dom = 'Bfltp', #defines the position of elements around the table
                    searchHighlight = TRUE,
                    search = list(regex = TRUE, caseInsensitive = TRUE),
                    pageLength = 10,
                    scrollX = TRUE,
                    autoWidth = TRUE,
                    fixedColumns = list(leftColumns = 1),
                    columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                      list(width = '120px', targets = c(0)), ## notice: the first column is c(0)
                                      list(width = '150px', targets = c(2)),
                                      list(width = '150px', targets = c(3)),## !!!notice: the CA column is c(?)
                                      #list(width = '90px', targets = c(4)),
                                      list(width = '90px', targets = c(5)),
                                      list(width = '70px', targets = c(9)),
                                      list(width = '70px', targets = c(12)),
                                      list(width = '70px', targets = c(15)),
                                      list(width = '70px', targets = c(18)),
                                      list(width = '70px', targets = c(22)),
                                      list(searchable = FALSE, targets = c(7:22)), # disable the filter function in columns "HR/CI/P"
                                      list(visible = FALSE, searchable = FALSE, targets = c(1)))## hide the column "id" 
                )
            )
        })
    })
    # 2.3 box3_KMcustomize_pathway  -----------------------------------------------------
    ## after clicking the button in DT, Upload the number of rows of data selected and the corresponding id to the server for processing
    dt_id_split_re <- eventReactive(input$current_id, {
        d <- dataset()
        dt_row <- which(stringr::str_detect(rownames(d), pattern = paste0("\\b", input$current_id, "\\b")))#"\\b"即单词边界(单词&符号的边界)
        dt_id <- as.character(d[dt_row, 1])
        dt_id_split <- strsplit(dt_id, split = "_")
    })
   symbol_show <- eventReactive(input$current_id, {
        d <- dataset()
        dt_row <- which(stringr::str_detect(rownames(d), pattern = paste0("\\b", input$current_id, "\\b")))
        dt_id <- as.character(d[dt_row, 2])
        dt_id
    })
    shiny::observeEvent(input$current_id, {
        dt_id_split <- dt_id_split_re()
        if (!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "choosem")) {
            output$DTselectinfo <- renderText(paste("You are interested in <b>", symbol_show(),  "</b>.", 
                                                    "<br>", "The dataset you have chosen is <b>", dt_id_split[[1]][3], "</b>(<b>", dt_id_split[[1]][2], "</b>). Its survival outcome is <b>", dt_id_split[[1]][5], "</b>.",
                                                    "<br>", "The cut-off method chosen is <b>Median</b>.",
                                                    "<br>",
                                                    "<br>", "<b> You can switch to the OPTIMAL cut-off method by clicking the BLACK button alongside. More colors can be customized below. </b>"))
            pathway_plot <- pathway_km_plot(cancertype = dt_id_split[[1]][2],
                                    gse = dt_id_split[[1]][3],
                                    gpl = dt_id_split[[1]][4],
                                    survival = dt_id_split[[1]][5],
                                    #pathway = paste0(dt_id_split[[1]][1],'_',''),
                                    pathway = dt_id_split[[1]][1],
                                    #gene_show = symbol_show(),
                                    cutoff = "m",
                                    col_1 = "#c35f50",
                                    col_2 = "#477aae")
            output$KMpathway_plot <- renderPlot({pathway_plot})
            output$download_KMpathway<- downloadHandler(
                filename = function(){
                    paste("KM_pathway_",dt_id_split_re()[[1]][1],".pdf",sep="")
                },
                content = function(file) {
                    cairo_pdf(file)
                    print(pathway_plot)
                    dev.off()
                }
            )
        }
        if (!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "chooseb")) {
            output$DTselectinfo <- renderText(paste("You are interested in <b>", symbol_show(), "</b>.",
                                                    "<br>", "The dataset you have chosen is <b>", dt_id_split[[1]][3], "</b>(<b>", dt_id_split[[1]][2], "</b>). Its survival outcome is <b>", dt_id_split[[1]][5], "</b>.",
                                                    "<br>", "The cut-off method chosen is <b>Optimal</b>.",
                                                    "<br>",
                                                    "<br>", "<b> You can switch to the MEDIAN cut-off method by clicking the PURPLE button alongside. More colors can be customized below. </b>"))
            pathway_plot <- pathway_km_plot(cancertype = dt_id_split[[1]][2],
                                    gse = dt_id_split[[1]][3],
                                    gpl = dt_id_split[[1]][4],
                                    survival = dt_id_split[[1]][5],
                                    pathway = dt_id_split[[1]][1],
                                    #gene_show = symbol_show(),
                                    cutoff = "b",
                                    col_1 = "#c35f50",
                                    col_2 = "#477aae")
            output$KMpathway_plot <- renderPlot({pathway_plot})
            output$download_KMpathway<- downloadHandler(
                filename = function(){
                    paste("KM_pathway_",dt_id_split_re()[[1]][1],".pdf",sep="")
                },
                content = function(file) {
                    cairo_pdf(file)
                    print(pathway_plot)
                    dev.off()
                }
            )
        }
        
        if (input$pathway_plotbox$collapsed) {
            updateBox("pathway_plotbox", action = "toggle")
        }
        shinyjs::enable(id = "pathway_plotbox")
        scroll("pathway_plotbox", block = "end")
        # runjs('
        #   window.scrollTo(0, document.body.scrollHeight);
        # ')
    })
    
    observeEvent(input$customize_pathwaybtn, {
        dt_id_split <- dt_id_split_re()
        pathway_plot <- pathway_km_plot(cancertype = dt_id_split[[1]][2],
                                gse = dt_id_split[[1]][3],
                                gpl = dt_id_split[[1]][4],
                                survival = dt_id_split[[1]][5],
                                pathway = dt_id_split[[1]][1],
                                #gene_show = symbol_show(),
                                cutoff = input$cutoff_KMpathway,
                                col_1 = input$pathway_low_color,
                                col_2 = input$pathway_high_color)
        output$KMpathway_plot <- renderPlot({pathway_plot})
        output$download_KMpathway<- downloadHandler(
            filename = function(){
                paste("KM_pathway_",dt_id_split_re()[[1]][1],".pdf",sep="")
            },
            content = function(file) {
                cairo_pdf(file)
                print(pathway_plot)
                dev.off()
            }
        )
    })
    
    observeEvent(input$customize_defalutbtn, {
        updateColorPickr(session, inputId = "pathway_low_color", value = "#c35f50")
        updateColorPickr(session, inputId = "pathway_high_color", value = "#477aae")
        updateRadioButtons(session, inputId = "cutoff_KMpathway", selected = "m")
    }) 
    # 3 box_KMinfodt_box  -----------------------------------------------------
    output$kminfodt <- renderDT({
        Sys.sleep(0.1)
        datatable(
            isolate(info_agg_dt[,c(1:6,8)]),
            escape = FALSE,#don't excape any HTML in the table (i.e., the action button)
            filter = c("top"),
            callback=JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Search" )'),
            rownames = FALSE,
            extensions = c('Buttons', 'FixedColumns'),
            selection="none",#turn off row selection function otherwise u will select that row when clicking on the action button
            options = list(
                # processing = FALSE,
                dom = 'Bfltp',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                searchHighlight = TRUE,
                search = list(regex = TRUE, caseInsensitive = TRUE),
                pageLength = 20,
                scrollX = TRUE,
                autoWidth = TRUE,
                fixedColumns = list(leftColumns = 1),
                columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                  list(width = '150px', targets = c(0)) ## !!!notice: the CA column is c(0)
                )
            )
        )
    })
    
    # 4 Comment box -----------------------------------------------------------
    observeEvent(input$submit_commentbtn, {
        
        req(input$comment)
        
        smtp <- emayili::server(
            host = "smtp.163.com",
            port = 25,
            username = "shiny_luopeng@163.com",
            password = "LFZKTEZQQLXDTOGK"
        )
        
        email <- envelope() %>%
            from("shiny_luopeng@163.com") %>%
            to("shiny_luopeng@163.com")%>%
            subject(paste("PESSA-SHINY_FEEDBACK: ", input$contact))%>%
            text(paste(input$comment, "from:", input$contact))
        
        smtp(email)
        
        show_alert(
            title = "Success",
            text = "Thanks, your response was submitted successfully! We will get back to you as soon as possible.",
            type = "success"
        )
    })
    
}