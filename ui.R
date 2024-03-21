
library(shiny)

navbarPage(
    id = 'path_navbar',
    title = div(
        HTML('<span style="font-size:180%;color:white;font-weight:bold;"> PESSA</span></a>'),
        tags$style(style = 'position:absolute; right:42px;'),
        tags$style(type="text/css", ".navbar-brand {padding-top: 23px;}"),
        tags$style(HTML("#panel1{font-size: 18px}")),
        tags$style(HTML("#panel2{font-size: 18px}")),
        tags$style(HTML("#panel3{font-size: 18px}")),
        tags$style(HTML("#panel4{font-size: 18px}"))
    ),
    theme = shinytheme('cosmo'),
    fluid = T,#TRUE to use a fluid layout
    windowTitle = "PESSA",#the browser window title
    ###### 插入依赖项 ######
    header = tagList(
        useShinydashboardPlus(),
        useShinyjs(),
        use_shinyscroll()
    ),
    #----------------tabPanel1.home page----------------------
    
    tabPanel(h4(id = "panel1", "Home"),
             value = "panel1",
             fluidRow(
                 column(12,
                        tags$h1("PESSA"),
                        h4("PESSA (Pathway Enrichment Score-based Survival Analysis) is a large-scale interactive web tool dedicated to pan-cancer survival analysis and visualization by using results from single sample gene set enrichment analysis(ssGSEA). With PESSA, users can quickly explore the impact of target pathway on survival outcomes in different tumors, assisting clinicians and researchers in further investigating the mechanism of tumor development and improving clinical decision-making.",
                           br())
                 )
             ),
             tags$hr(),
             fluidRow(column(7,
                             align = "center",
                             tags$img(src="www/PANCAN.png", width="90%", alt="Something went wrong...Please refresh page.")),
                      tags$style(HTML(".intro_text {margin-top: 30px;}")),
                      column(5,
                             div(class = "intro_text",
                                 tags$h3(strong("How does PESSA work?")),
                                 tags$br(),
                                 h4("STEP1: Select Data to Be Processed"),
                                 h4("STEP2: View Analysis Results"),
                                 h4("STEP3: Costomize and Download Your Plot"),
                                 tags$br(),
                                 actionButton(inputId = "homebtn", label = h4(strong("Get Started Now >>"))))
                      )
             ),
             tags$hr(),
             fluidRow(column(5,
                             HTML('<h3><b>Updates</b></h3>',
                                  '<h4>20/03/23 Version 1.0.0 of PESSA released.</h4>',
                                  '<h4>08/02/24 Version 1.1.0 of PESSA released.</h4>')),
                      column(6,
                             class = 'footer-container',
                             HTML('<div style="text-align: center;"><div style="display:inline-block;"><script type="text/javascript" src="//rf.revolvermaps.com/0/0/2.js?i=5imqx70rap3&amp;m=7&amp;s=90&amp;c=ff0000&amp;t=1" async="async"></script></div> </div>
                                  <div style="text-align: center;"><p>Copyright © 2023. All rights reserved.</p></div>'))
                      
             ),
             #fluidRow(column(6,
              #               class = 'footer-container',
               #              HTML('<div style="text-align: right;"><div style="display:inline-block"><script type="text/javascript" src="//rf.revolvermaps.com/0/0/2.js?i=50cmlrir964&amp;m=7&amp;s=130&amp;c=ffffff&amp;t=1" async="async"></script></div> </div>
                #                  <div style="text-align: center;"><p>Copyright © 2023. All rights reserved.</p></div>'))),
            tags$style(HTML(".footer-container {width: 100%; /*for nothing*/
                             bottom: 0; /*for nothing*/
                             left: 0; /*for nothing*/
                             position: static;
                             padding: 0;/*IMPORTANT! W/0 PADDING*/;
                             }")) 
            
    ),
    
    #-------------------------tabPanel2.PLOT------------------------------ 
    tabPanel(h4(id = "panel2", "PLOT!"),
             value = "panel2",
             # 2.1.1 box1: select & filter input data ----------------------------------------
             fluidRow(
                 box(id = "pathway_selectbox",
                     title = strong("STEP1: Select Data to Be Processed", style = 'font-size:18px;color:white;'),
                     icon = icon("upload"),
                     collapsible = FALSE,
                     collapsed = FALSE,
                     status = "black", solidHeader = TRUE, width = 12,
                     fluidRow(
                         column(6, selectizeInput(inputId = "select.pathway",
                                                  label = "Select a gene set",
                                                  choices = NULL, 
                                                  multiple = FALSE,
                                                  options = list(
                                                      placeholder = "Type the gene set name of your interests",
                                                      maxItems = 1
                                                  )) %>% helper(type = "inline",
                                                                title = "GENE SET SELECTION",
                                                                content = c("Please make sure you type the <b>right</b> gene set.",
                                                                            "You can check it further in the ABOUT page.")),
                                 actionButton(inputId = "submitdata_btn", label = "Analyze!", style = 'margin-top:25px'),
                                downloadButton(outputId = "downloaddata_btn", class = "downloadbtn", label = "DOWNLOAD RESULTS", style = 'margin-top:25px')
                                ),
                         column(6, 
                                box(textOutput("des"),title = strong("Brief Description"), status = "success", width = 12,   
                                    HTML('<p>For more information, please refer to the website</p>'),
                                    uiOutput("web") 
                         )
                     )
                 ))
             ),
             # 2.1.2 box2: show results datatable ----------------------------------------
             fluidRow(
                 shiny::includeScript("www/script.js"),# js for buttons
                  tags$style(HTML("div.datatables {width: auto; height: auto;}")),
                 box(id = "pathway_tablebox",
                     title = strong("STEP2: View Analysis Results", style = 'font-size:18px;color:white;'),
                     icon = icon("table"),
                     collapsible = TRUE,
                     collapsed = TRUE,
                     status = "black", solidHeader = TRUE, width = 12,
                     # fluidRow(column(12, Spinner(id = "test", size = 3, label = "Loading, please wait..."))),
                     # fluidRow(column(12, dataTableOutput("kmresultdt")))
                     fluidRow(column(12, shinycssloaders::withSpinner(shiny::uiOutput("pathwayresultdt"), color = "black")))
                 )
             ),
             # 2.1.3 box3: show the plot and download ----------------------------------------
             fluidRow(
                 tags$style(HTML(".panel {border: 0;-webkit-box-shadow: 0 0 0;box-shadow: 0 0 0;}
                           .panel-default > .panel-heading {color: blue;background-color: transparent;border-color: transparent;}")),
                 box(id = "pathway_plotbox",
                     title = strong("STEP3: View, Costomize and Download Your Plot", style = 'font-size:18px;color:white;'),
                     icon = icon("square-check"),
                     collapsible = TRUE,
                     collapsed = TRUE,
                     status = "black", solidHeader = TRUE, width = 12,
                     fluidRow(column(12,wellPanel(htmlOutput("DTselectinfo")))),
                     fluidRow(column(1, downloadButton(outputId = "download_KMpathway", label = "Download", icon=icon("download")))),
                     br(),
                     fluidRow(column(3, shinyBS::bsCollapsePanel("CUSTOMIZE OPTIONS>>",
                                                                 fluidRow(column(6, colorPickr(inputId = "pathway_low_color", label = "LOW:", selected = "#c35f50", theme = "monolith", update ="change")),
                                                                          column(6, colorPickr(inputId = "pathway_high_color", label = "HIGH:", selected = "#477aae", theme = "monolith", update ="change"))),
                                                                 fluidRow(column(12, radioButtons(inputId = "cutoff_KMpathway", label = "Choose a method for the cutpoint", choiceNames = list("Median", "Optimal (maximally selected rank statistics)"), choiceValues = list("m", "b"),inline = FALSE, selected = "m"))),
                                                                 fluidRow(column(2, actionButton(inputId = "customize_pathwaybtn", label = "proceed!")),
                                                                          column(2, offset = 2, actionBttn(inputId = "customize_defalutbtn", label = "Default", style = "minimal", color = "primary", size = "sm")))
                     )),
                     column(5, offset = 1, shinycssloaders::withSpinner(plotOutput("KMpathway_plot"), color = "black")))
                 )
             )),
             # 3. tab_panel_3_DATA -------------------------------------------------
             tabPanel(h4(id = "panel3", "Data"),
                      value = "panel3",
                      fluidRow(
                          box(id = "pathway_infotbbox",
                              title = strong("DATASETS DESCRIPTION", style = 'font-size:18px;color:white;'),
                              icon = icon("table"),
                              collapsible = FALSE,
                              collapsed = FALSE,
                              status = "black", solidHeader = TRUE, width = 12,
                              fluidRow(column(12, shinycssloaders::withSpinner(dataTableOutput("kminfodt"), color = "black")))
                          )
                      )),
             # 4. tab_panel_4_about ----------------------------------------------------
             tabPanel(h4(id = "panel4", "About"),
                      value = "panel4",
                      fluidRow(
                          box(id = "contactbox",
                              title = strong("Contact", style = 'font-size:18px;color:white;'),
                              icon = icon("users"),
                              collapsible = FALSE,
                              collapsed = FALSE,
                              status = "black", solidHeader = TRUE, width = 12,
                              HTML('<p>Should you have any questions, please feel free to contact us.</p>',
                                   '<p>Peng Luo: <a href="mailto:luopeng@smu.edu.cn">luopeng@smu.edu.cn</a> </p>',
                                   '<p>Hong Yang: <a href="mailto:smuyanghong@i.smu.edu.cn">smuyanghong@i.smu.edu.cn</a></p>',
                                   '<p>Ying Shi: <a href="mailto:shoshanashi@i.smu.edu.cn">shoshanashi@i.smu.edu.cn</a></p>',
                                   '<br/>',
                                   '<p>Our lab has a long-standing interest in cancer biomedical research and bioinformatics. We recently developed several other Shiny web tools focusing on solving various scientific questions.</p>',
                                   '<p><a href="http://www.pancansurvplot.com/" target="_blank">PanCanSurvPlot</a>: A Web Server for Pan-cancer Transcriptome Survival Analysis and Visualization. doi: <a href=" https://doi.org/10.1101/2022.12.25.521884" target="_blank">10.1101/2022.12.25.521884[preprint]</a></p>',
                                   '<p><b>Citation:</b> Lin, A., Yang, H., Shi, Y., Cheng, Q., Liu, Z., Zhang, J., & Luo, P. (2022). PanCanSurvPlot: A Large-scale Pan-cancer Survival Analysis Web Application. BioRxiv, 2022.12.25.521884. https://doi.org/10.1101/2022.12.25.521884</p>',
                                   '<br/>',
                                   '<p><a href="http://www.camoip.net" target="_blank">CAMOIP</a>: A Web Server for Comprehensive Analysis on Multi-omics of Immunotherapy in Pan-cancer. doi: <a href="https://doi.org/10.1093/bib/bbac129" target="_blank">10.1093/bib/bbac129</a></p>',
                                   '<p><b>Citation:</b> Lin, A., Qi, C., Wei, T., Li, M., Cheng, Q., Liu, Z., Luo, P., & Zhang, J. (2022). CAMOIP: a web server for comprehensive analysis on multi-omics of immunotherapy in pan-cancer. Briefings in Bioinformatics, 23(3). https://doi.org/10.1093/bib/bbac129</p>',
                                   '<br/>',
                                   '<p><a href="https://smuonco.Shinyapps.io/Onlinemeta/" target="_blank">Onlinemeta</a>: A Web Server for Meta-Analysis Based On R-shiny. doi: <a href="https://doi.org/10.1101/2022.04.13.488126" target="_blank">10.1101/2022.04.13.488126[preprint]</a></p>',
                                   '<p><b>Citation:</b> Yi, Y., Lin, A., Zhou, C., Jian, Z., Wang, S., & Luo, P. (2022). Onlinemeta: A Web Serve For Meta-Analysis Based On R-shiny. BioRxiv, 2022.04.13.488126. https://doi.org/10.1101/2022.04.13.488126</p>'
                              )
                          )
                      ),
                      fluidRow(
                          box(id = "tutorialbox",
                              title = strong("Tutorial Video", style = 'font-size:18px;color:white;'),
                              icon = icon("youtube"),
                              collapsible = TRUE,
                              collapsed = TRUE,
                              status = "black", solidHeader = TRUE, width = 12,
                              fluidRow(column(6, offset=3, HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/mF3FV77lLZ4?si=bb4-zAZ4_vlVTU2-" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>'))),
                              fluidRow(column(6, offset=3, HTML('<p>For users who can not access, the tutorial video is also available on <a href="https://www.bilibili.com/video/BV11h4y1U7Vk" target="_blank">Bilibili</a>.</p>')))
                          )
                      ),
                      fluidRow(
                          box(id = "commentbox",
                              title = strong("Comment Box", style = 'font-size:18px;color:white;'),
                              icon = icon("envelope"),
                              collapsible = TRUE,
                              collapsed = TRUE,
                              status = "black", solidHeader = TRUE, width = 12,
                              useSweetAlert("borderless", ie = F),#"sweetalert2", "minimal", "dark", "bootstrap-4", "material-ui", "bulma","borderless"
                              fluidRow(column(8,
                                              textInput(inputId = "contact", label = "Name/Email (optional)", width = "60%"),
                                              textAreaInput(inputId = "comment", label = labelMandatory("Comment"), placeholder = "Enter your comment here", width = "100%", height = "100px"),
                                              actionButton("submit_commentbtn", "Submit Comment")
                              ))
                          )
                      ),
                      fluidRow(
                          box(id = "updatesbox",
                              title = strong("Update History", style = 'font-size:18px;color:white;'),
                              icon = icon("file-pen"),
                              collapsible = TRUE,
                              collapsed = FALSE,
                              status = "black", solidHeader = TRUE, width = 12,
                              HTML('<p>20/03/23 Version 1.0.0beta of PESSA released.</p>',
                                   '<p>08/02/24 Version 1.1.0beta of PESSA released.</p>'
                              )
                          )
                      ),
                      fluidRow(
                          box(id = "FAQbox",
                              title = strong("FAQ", style = 'font-size:18px;color:white;'),
                              icon = icon("comments"),
                              collapsible = TRUE,
                              collapsed = FALSE,
                              status = "black", solidHeader = TRUE, width = 12,
                              HTML('<h5> <b>1. Why does the gene sets I type in return no results?</b> </h5>',
                                   '<p>The gene sets we provided here mostly come from MSigDB(HGNC-approved) <a href="https://www.gsea-msigdb.org/gsea/msigdb/human/genesets.jsp?collection=H" target="_blank">Hallmark gene sets</a>, <a href="https://www.gsea-msigdb.org/gsea/msigdb/human/genesets.jsp?collection=CP" target="_blank">Canonical pathways</a> and <a href="https://www.gsea-msigdb.org/gsea/msigdb/human/genesets.jsp?collection=GO" target="_blank">Gene Ontology gene sets</a>. The whole list of gene sets we provided can be found <a href="www/data/genesetlist.csv" download="genesetlist.csv">here</a>.</p>',
                                   '<h5> <b>2. What are the full forms of the survival outcomes provided?</b> </h5>',
                                   '<p>Altogether, there are 13 different survival outcomes available. Their abbreviations and corresponding full forms are shown below.</p>',
                                   '<p> BCR: Biochemical Recurrence Free Survival</p>',
                                   '<p> CSS: Cancer Specific Survival</p>',
                                   '<p> DFI: Disease Free Interval</p>',
                                   '<p> DFS: Disease Free Survival</p>',
                                   '<p> DMFS: Distant Metastasis Free Survival</p>',
                                   '<p> DRFS: Distant Relapse Free Survival</p>',
                                   '<p> DSS: Disease Specific Survival</p>',
                                   '<p> FFS: Failure Free Survival</p>',
                                   '<p> MFS: Metastasis Free Survival</p>',
                                   '<p> OS: Overall Survival</p>',
                                   '<p> PFI: Progression Free Interval</p>',
                                   '<p> PFS: Progression Free Survival</p>',
                                   '<p> RFS: Recurrence Free Survival</p>',
                                   '<h5> <b>3. Why is there no median survival line shown in the K-M plot I have made?</b> </h5>',
                                   '<p> Because of the specific cutoff point you chose, the median survival has not yet been reached, and more than half of the patients are still alive.</p>'
                              )
                          )
                      )
             )
    )