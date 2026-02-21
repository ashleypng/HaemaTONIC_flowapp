
# ============================================================
# R Shiny App: Flow Cytometry Panels by WHO 2022 / ELN 2022
# Diagnostic Categories — Sections A through G
# Created by Ashley NG. Version 1. February 2026.
# ============================================================
# Install if needed:
# install.packages(c("shiny", "DT", "dplyr", "shinythemes", "readr"))

library(shiny)
library(DT)
library(dplyr)
library(readr)
library(shinythemes)

# ----------------------------------------------------------
# 1. Embedded data (paste CSV content or load from file)
#    Here we build the data frames directly so the app is
#    fully self-contained in one file.
# ----------------------------------------------------------

secA <- data.frame(
  Section = "A",
  Section_Title = "AML: Genetically-Defined Entities",
  Entity = c(
    "APL — AML with PML-RARA [t(15;17)]",
    "AML with RUNX1-RUNX1T1 [t(8;21)]",
    "AML with CBFB-MYH11 [inv(16)/t(16;16)]",
    "AML with NPM1 mutation",
    "AML with CEBPA mutation (bZIP in-frame)",
    "AML with KMT2A rearrangement",
    "AML with MECOM rearrangement [inv(3)/t(3;3)]",
    "AML with DEK-NUP214 [t(6;9)]",
    "AML with BCR-ABL1",
    "AML with mutated TP53 (new WHO 2022)"
  ),
  Positive_Characteristic = c(
    "CD33 (bright), CD13, CD64 (M3v), CD9",
    "CD34, CD117, HLA-DR, CD13, CD15 (bright), CD56",
    "CD34, CD117, HLA-DR, CD13, CD33, CD11b, CD64, CD14",
    "CD33 (bright), CD13, HLA-DR, CD117, CD123",
    "CD34, CD117, HLA-DR, CD13, CD33, CD7 (frequent aberrant)",
    "CD34+/-, CD117, HLA-DR, CD33, CD64, CD11b, CD14, CD36",
    "CD34, CD117, CD13, CD33, CD41a, CD61 (platelet co-expression)",
    "CD34, CD117, HLA-DR, CD13, CD33",
    "CD34, CD117, HLA-DR, CD13, CD33, CD19, CD25, CD66c",
    "CD34, HLA-DR, CD13, CD33"
  ),
  Negative_Absent = c(
    "HLA-DR (ABSENT — hallmark), CD34, CD117, CD11b, CD15, CD16",
    "CD33 (dim/weak), CD14",
    "—",
    "CD34 (characteristically absent)",
    "CD14, CD64",
    "CD15 (often)",
    "HLA-DR (often absent/dim)",
    "—",
    "—",
    "CD117 (often low/absent)"
  ),
  Cross_lineage_Aberrant = c(
    "—",
    "CD19+, CyCD79a+ (cross-lineage B)",
    "Eosinophil-associated CD16",
    "CD56+ (subset), CD7+ (subset)",
    "CD7+ common aberrant co-expression",
    "CD19+ (KMT2A-AFF1), CD56+",
    "Megakaryocytic markers aberrantly co-expressed",
    "CD7+, basophilic markers",
    "CD19+, CD25+, CD66c+ (lymphoid cross-lineage)",
    "—"
  ),
  Key_Notes = c(
    "HLA-DR-/CD34-/CD117- = fast-track ATRA + PML-RARA testing; REPORT URGENTLY",
    "CD19 co-expression highly characteristic; CD56+ = worse prognosis",
    "Mixed myelomonocytic; eosinophilic component in BM",
    "CD34-negativity + CD33bright = request NPM1 testing",
    "CD7 aberrancy common; lacks monocytic differentiation",
    "Five immunophenotypic subgroups identified",
    "Thrombocytosis phenotype; megakaryocytic aberrancy clue",
    "Basophilia + CD7 co-expression are characteristic",
    "Resembles Ph+ ALL; molecular/FISH required",
    "No specific immunophenotype; FC contributes blast % only"
  ),
  Blast_Threshold = rep("No minimum (WHO 2022); ≥10% (ICC 2022)", 10),
  Reference = c(
    "Béné et al. Leukemia 2011; Döhner et al. Blood 2022",
    "van Dongen et al. Leukemia 2012; WHO 2022",
    "WHO 2022; ELN 2022",
    "WHO 2022; ELN 2022",
    "WHO 2022; ICC 2022",
    "WHO 2022",
    "WHO 2022",
    "WHO 2022",
    "WHO 2022; ICC 2022",
    "WHO 2022; ICC 2022"
  ),
  stringsAsFactors = FALSE
)

secB <- data.frame(
  Section = "B",
  Section_Title = "AML: Differentiation-Defined (No qualifying genetics)",
  Entity = c(
    "AML, myelodysplasia-related (AML-MR)",
    "AML with minimal differentiation (M0)",
    "AML without maturation (M1)",
    "AML with maturation (M2)",
    "Acute myelomonocytic leukaemia (M4)",
    "Acute monoblastic/monocytic leukaemia (M5)",
    "Acute erythroid leukaemia / pure erythroid (M6)",
    "Acute megakaryoblastic leukaemia (M7)"
  ),
  Positive_Characteristic = c(
    "CD34, CD117, HLA-DR, CD13, CD33; dysplastic patterns",
    "CD34, CD117, HLA-DR, CD13/CD33 (dim)",
    "CyMPO+, CD34, CD117, HLA-DR, CD13, CD33",
    "CD34, CD117, HLA-DR, CyMPO, CD13, CD33, CD15, CD11b",
    "CD34, CD117, HLA-DR, CD13, CD33, CD64, CD14, CD11b, CD36, CD4",
    "CD64 (bright), CD14, CD11b, CD11c, CD36, HLA-DR, CD33",
    "CD235a (GlyA), CD71, CD117 (early), CD36",
    "CD41a (GPIIb), CD42b (GPIbα), CD61 (GPIIIa), CD36"
  ),
  Negative_Absent = c(
    "—",
    "CyMPO (weak/neg by FC), CD14, CD15, CD65",
    "CD14, CD15, CD11b (absent/minimal)",
    "CD14 (absent/minimal)",
    "—",
    "CD34 (often absent), CyMPO (often negative)",
    "CD34 (often absent), HLA-DR, CyMPO, CD13/CD33",
    "CyMPO"
  ),
  Cross_lineage_Aberrant = c(
    "CD7, CD56, CD19 (LAIPs)",
    "NuTdT+ (subset), CD7+",
    "CD7, CD19 (subset)",
    "CD56, CD19 (subset)",
    "—",
    "—",
    "—",
    "—"
  ),
  Key_Notes = c(
    "Defined by MDS-related genetics; FC shows MDS-like aberrancies in maturing compartments",
    "Cytochemical/EM MPO required; FC alone insufficient",
    "≥3% blast MPO positivity required",
    "Aberrant CD19 in t(8;21) subset",
    "Dual granulocytic + monocytic differentiation required",
    "CD64bright + CD14+; ≥2 monocytic markers required (WHO 2022)",
    "≥80% erythroid cells (≥30% proerythroblasts); HLA-DR-/CD34-",
    "≥1 platelet glycoprotein required; Down syndrome-related AML"
  ),
  Blast_Threshold = c("≥20%","≥20%","≥20%","≥20%","≥20%","≥20%",
                      "≥80% erythroid; ≥30% proerythroblasts","≥20%"),
  Reference = rep("WHO 2022; ELN 2022", 8),
  stringsAsFactors = FALSE
)

secC <- data.frame(
  Section = "C",
  Section_Title = "MDS/AML Borderzone and MDS",
  Entity = c(
    "MDS/AML (ICC 2022 only)",
    "MDS-IB2 (WHO 2022 — 10–19% blasts)",
    "MDS-IB1 (WHO 2022 — 5–9% blasts)",
    "MDS, low blasts (MDS-LB, <5%)",
    "MDS with SF3B1 mutation (WHO 2022)",
    "MDS with del(5q) (WHO 2022)",
    "MDS with biallelic TP53 inactivation (WHO 2022)",
    "Hypoplastic MDS"
  ),
  Blast_Percentage = c("10–19%","10–19%","5–9%","<5%","<5%","<5%","Variable","<5%"),
  Key_FC_Findings = c(
    "MDS-like dysplastic pattern + increased blasts; LAIP/DfN MRD applicable",
    "MDS pattern + blast increase; FC for blast enumeration",
    "Dysplastic granulocytes/monocytes; reduced B-progenitors; aberrant CD34+ cells",
    "Aberrant CD13/CD16 granulocytic maturation; abnormal monocyte pattern; reduced CD34+/CD19+ ratio",
    "Ring sideroblast-associated; CD71/CD235a erythroid dysplasia; often unremarkable granulocyte FC",
    "Non-specific FC; hypolobated megakaryocytes (morphology); FC mainly blast count + dysplasia",
    "Often complex karyotype; no specific FC immunophenotype beyond MDS scoring",
    "Hypocellular BM; FC on available aspirate using full IMDSFlow panel"
  ),
  IMDSFlow_Compartments = c(
    "All 5 compartments","All 5 compartments","All 5 compartments",
    "Granulocytic, monocytic, B-progenitor, erythroid",
    "Erythroid compartment key",
    "Mainly megakaryocytic + MPC",
    "All compartments","All compartments where possible"
  ),
  Aberrancy_Rule = rep("≥3 aberrations across ≥2 compartments = consistent with MDS (IMDSFlow)", 8),
  Reference = c(
    "ICC 2022; Westers et al. Leukemia 2012",
    "WHO 2022","WHO 2022",
    "WHO 2022; Westers et al. Leukemia 2012",
    "WHO 2022","WHO 2022","WHO 2022",
    "WHO 2022; Porwit et al. Leukemia 2014"
  ),
  stringsAsFactors = FALSE
)

secD <- data.frame(
  Section = "D",
  Section_Title = "Acute Lymphoblastic Leukaemia / Lymphoma",
  Entity = c(
    "BCP-ALL, Pro-B",
    "BCP-ALL, Common (c-ALL)",
    "BCP-ALL, Pre-B",
    "BCP-ALL with BCR-ABL1 (Ph+)",
    "BCP-ALL, Ph-like (BCR-ABL1-like)",
    "BCP-ALL with KMT2A rearrangement",
    "BCP-ALL with DUX4 rearrangement (new WHO/ICC 2022)",
    "T-ALL, NOS",
    "ETP-ALL (Early T-cell Precursor ALL — formal WHO 2022 entity)"
  ),
  Positive_Defining = c(
    "CD19, CD34, NuTdT, HLA-DR, CyCD79a",
    "CD19, CD10 (bright), CD34, NuTdT, CD38, CD58",
    "CD19, CD10, CyIgM+, NuTdT",
    "CD19, CD10, CD34, CD13, CD33, CD25, CD66c",
    "CD19, CD10, CD34; CD25+ (CRLF2-rearranged)",
    "CD19, CD34+/-, NuTdT, NG2/7.1+, CD15+",
    "CD19, CD34, NuTdT, CD371 (CLL-1/CLEC12A)",
    "CyCD3, CD7, NuTdT, CD34+/-",
    "CyCD3, CD7, CD34, CD117; myeloid cross-lineage: CD13+, CD33+, CD11b+, CD65+"
  ),
  Negative_Key = c(
    "CD10, CyIgM, SmIgM",
    "CyIgM, SmIgM",
    "SmIgM",
    "—",
    "—",
    "CD10 (often absent — key clue for KMT2A)",
    "—",
    "SmIgM, CyIgM",
    "CD1a (absent), CD8 (absent), CD5 (absent or weak ≤75% blasts)"
  ),
  Molecular_FC_Markers = c(
    "KMT2A rearrangement common; NG2/7.1+",
    "TEL-AML1 [t(12;21)]: CD9+, CD20-/dim",
    "TCF3-PBX1 [t(1;19)]: CD9+, CD34 often absent",
    "CD25+, CD66c+, CD13/CD33 cross-lineage",
    "CD25+ and TSLPR/CRLF2 by FC for Ph-like screening; CD371+",
    "NG2/7.1 highly specific for KMT2A; CD15+",
    "CD371 (CLL-1) is characteristic marker",
    "CD1a (cortical); CD4/CD8 double-pos (cortical); CD4-/CD8- (early/ETP)",
    "CD13, CD33, CD117, CD11b — myeloid markers MANDATORY for ETP diagnosis"
  ),
  Key_Notes = c(
    "CD10-negative Pro-B; most common in infants",
    "Most common ALL in children; CD10bright is hallmark",
    "CyIgM positivity defines Pre-B from Common ALL",
    "Myeloid cross-lineage markers characteristic; CD25 and CD66c key discriminators",
    "Gene expression profile similar to Ph+ ALL; molecular testing required",
    "CD10 negativity in CD19+ ALL is a strong clue for KMT2A",
    "New WHO/ICC 2022 entity; CD371 is key FC marker; favourable prognosis",
    "Stage by CD34/CD1a/CD4/CD8: Pro-T, Pre-T, Cortical, Mature",
    "Must distinguish from T/myeloid MPAL. ETP: CyCD3+ + myeloid markers but NOT meeting full myeloid EGIL criteria"
  ),
  Reference = c(
    "WHO 2022; Craig & Foon Blood 2008",
    "WHO 2022; van Dongen et al. Leukemia 2012",
    "WHO 2022",
    "WHO 2022; van Dongen et al. Leukemia 2012",
    "WHO 2022; ICC 2022",
    "WHO 2022",
    "WHO 2022; ICC 2022",
    "WHO 2022; van Dongen et al. Leukemia 2012",
    "WHO 2022; van Dongen et al. Leukemia 2012"
  ),
  stringsAsFactors = FALSE
)

secE <- data.frame(
  Section = "E",
  Section_Title = "MPAL / ALAL (Acute Leukaemias of Ambiguous Lineage)",
  Entity = c(
    "MPAL, T/myeloid",
    "MPAL, B/myeloid",
    "MPAL, B/T (rare)",
    "ALAL with BCR-ABL1 (new WHO 2022)",
    "ALAL with KMT2A rearrangement (new WHO 2022)"
  ),
  Lineage_1_Criteria = c(
    "CyCD3+ (strong) or SmCD3+ (strong)",
    "Strong CD19+ + ≥1 of: CD10 (strong), CD79a, CD22",
    "Strong CyCD79a or CyCD22 + B markers",
    "By above MPAL/lineage criteria",
    "By above MPAL/lineage criteria"
  ),
  Lineage_2_Criteria = c(
    "CyMPO+ OR ≥2 of: CD11c, CD14, CD64, NSE, lysozyme",
    "CyMPO+ OR ≥2 monocytic markers (CD14, CD64, CD11c)",
    "CyCD3+ or SmCD3+ (strong)",
    "By above MPAL/lineage criteria",
    "By above MPAL/lineage criteria"
  ),
  Key_Notes = c(
    "ETP-ALL must be excluded first — ETP has CyCD3 + myeloid markers but does NOT meet full myeloid EGIL criteria",
    "Most common MPAL subtype; both lineages on SAME blast population",
    "Extremely rare; requires both lineages definitively assigned",
    "Classified as ALAL regardless of dominant lineage; molecular testing mandatory",
    "Classified as ALAL regardless of dominant lineage"
  ),
  Reference = rep("WHO 2022; EGIL (Bain et al. Leukemia 1998)", 5),
  stringsAsFactors = FALSE
)

secF <- data.frame(
  Section = "F",
  Section_Title = "Mature B-cell Neoplasms / CLL",
  Entity = c(
    "CLL (Chronic Lymphocytic Leukaemia)",
    "MCL (Mantle Cell Lymphoma)",
    "Hairy Cell Leukaemia (HCL)",
    "Marginal Zone Lymphoma (MZL)",
    "Follicular Lymphoma (FL)"
  ),
  Positive_Characteristic = c(
    "CD5, CD23, CD19, CD200+, CD43+",
    "CD5, CD19, CD20 (bright), CD22, FMC7, CD79b, CyclinD1+, SOX11+",
    "CD19, CD20 (bright), CD22, CD11c, CD25, CD103, CD123, TRAP+, CD200+",
    "CD19, CD20 (bright), CD22, CD79b, FMC7",
    "CD19, CD20, CD10, BCL2+, BCL6+"
  ),
  Negative_Weak = c(
    "FMC7 (neg), CD22 (weak), CD79b (weak/neg), sIg (weak), CD20 (dim)",
    "CD23 (ABSENT — key distinction from CLL), CD200 (negative)",
    "CD5, CD23",
    "CD5 (usually neg), CD10, CD23",
    "CD5, CD23"
  ),
  Scoring_Prognostic = c(
    "Matutes score 4–5; CD38 ≥30% = poor prognosis; ZAP-70; CD49d",
    "MIPI score; SOX11 IHC essential; SOX11-neg = indolent subset",
    "CD103/CD25/CD11c/CD123 co-expression = characteristic quad; BRAF V600E >95%",
    "Matutes score 0–1; sIg moderate-to-strong; light chain restriction",
    "CD10+ B-cell; FLIPI score; histologic grade 1–3"
  ),
  Reference = c(
    "Matutes et al. Leukemia 1994; Moreau et al. 1997; WHO 2022",
    "WHO 2022",
    "WHO 2022; Craig & Foon Blood 2008",
    "WHO 2022",
    "WHO 2022"
  ),
  stringsAsFactors = FALSE
)

secG <- data.frame(
  Section = "G",
  Section_Title = "CMML (Chronic Myelomonocytic Leukaemia — post WHO 2022)",
  Entity = c(
    "CMML — monocytosis defining markers",
    "CMML — aberrant monocyte markers",
    "CMML — blast enumeration markers",
    "Post-2022 structural changes affecting FC interpretation"
  ),
  FC_Marker = c(
    "CD14, CD64, HLA-DR, CD33, CD16+/-",
    "CD2+ (aberrant), CD56+ (aberrant), CD14/CD16 pattern abnormal",
    "CD34, CD117, CD45",
    "Various — see Key Finding"
  ),
  Key_Finding = c(
    "Absolute monocytosis ≥0.5×10⁹/L + monocytes ≥10% WBC; >94% classical monocytes (CD14bright/CD16-) in PB supports CMML over reactive monocytosis",
    "Aberrant CD2 and CD56 on monocytes supports clonality; CD56+ correlates with aggressive disease",
    "CD34+ myeloblast enumeration in BM critical for CMML subclassification (CMML-0/1/2)",
    "WHO 2022: no blast threshold for genetic AML; ETP-ALL now formal entity; ALAL with BCR-ABL1/KMT2A new; TP53-mutated AML new; ICC 2022: MDS/AML 10–19% blasts"
  ),
  Blast_Threshold = c(
    "BM <10% (CMML-0: PB<2%; CMML-1: PB 2–4%; CMML-2: BM 5–19% or PB 5–9%)",
    "As above",
    "As above",
    "Entity-dependent — see Sections A–F"
  ),
  Reference = c(
    "WHO 2022; Patnaik et al. Am J Hematol 2024",
    "WHO 2022; Patnaik et al. Am J Hematol 2024",
    "WHO 2022",
    "WHO 2022; ICC 2022; Döhner et al. Blood 2022"
  ),
  stringsAsFactors = FALSE
)

# ----------------------------------------------------------
# 2. Combine all sections with consistent columns
# ----------------------------------------------------------
all_data <- dplyr::bind_rows(secA, secB, secC, secD, secE, secF, secG)
all_data[is.na(all_data)] <- ""

section_labels <- c(
  "A — AML: Genetically-Defined Entities",
  "B — AML: Differentiation-Defined",
  "C — MDS/AML Borderzone and MDS",
  "D — Acute Lymphoblastic Leukaemia",
  "E — MPAL / ALAL",
  "F — Mature B-cell Neoplasms / CLL",
  "G — CMML (post WHO 2022)"
)

# ----------------------------------------------------------
# 3. UI
# ----------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(tags$style(HTML("
    body { font-size: 13px; }
    .navbar { background-color: #2c3e50; }
    h2 { color: #2c3e50; font-weight: 700; margin-bottom: 4px; }
    h4 { color: #7f8c8d; font-weight: 400; margin-top: 0; }
    .section-badge {
      display: inline-block;
      background: #2980b9;
      color: white;
      border-radius: 4px;
      padding: 2px 10px;
      font-size: 12px;
      margin-bottom: 8px;
    }
    .well { background-color: #f8f9fa; border: 1px solid #dee2e6; }
    .btn-primary { background-color: #2980b9; border-color: #2980b9; }
    #entity_search { border: 1px solid #2980b9; }
    .footer {
      margin-top: 30px;
      padding: 12px;
      background: #ecf0f1;
      border-radius: 4px;
      font-size: 11px;
      color: #7f8c8d;
    }
  "))),
  
  # Header
  fluidRow(
    column(12,
           tags$div(style = "padding: 18px 0 6px 0;",
                    tags$h2("🔬 Flow Cytometry Immunophenotyping Reference"),
                    tags$h4("WHO 2022 / ICC 2022 / ELN 2022 Diagnostic Categories — Sections A–G"),
                    tags$h5("Version 1. Created by Ashley Ng, February 2026")
           ),
           tags$hr()
    )
  ),
  
  # Controls row
  fluidRow(
    column(4,
           wellPanel(
             tags$label("Section", style="font-weight:600; color:#2c3e50;"),
             selectInput(
               "section_select",
               label = NULL,
               choices = c("All Sections", section_labels),
               selected = "All Sections",
               width = "100%"
             )
           )
    ),
    column(4,
           wellPanel(
             tags$label("Search entity / marker", style="font-weight:600; color:#2c3e50;"),
             textInput(
               "entity_search",
               label = NULL,
               value = "",
               placeholder = "e.g. APL, CD33, NPM1, CLL, ETP...",
               width = "100%"
             )
           )
    ),
    column(4,
           wellPanel(
             tags$label("Rows shown", style="font-weight:600; color:#2c3e50;"),
             selectInput(
               "page_length",
               label = NULL,
               choices = c("10" = 10, "25" = 25, "50" = 50, "All" = -1),
               selected = 10,
               width = "100%"
             )
           )
    )
  ),
  
  # Section badge + record count
  fluidRow(
    column(12,
           uiOutput("section_badge"),
           uiOutput("record_count")
    )
  ),
  
  tags$br(),
  
  # Table
  fluidRow(
    column(12,
           DTOutput("main_table")
    )
  ),
  
  # Footer
  fluidRow(
    column(12,
           tags$div(class = "footer",
                    "References: WHO Classification of Haematolymphoid Tumours, 5th Ed. 2022 | ",
                    "ICC 2022 (Arber et al.) | ELN 2022 (Döhner et al., Blood 2022) | ",
                    "EuroFlow (van Dongen et al., Leukemia 2012) | ELN WP10 (Béné et al., Leukemia 2011) | ",
                    "IMDSFlow (Westers et al., Leukemia 2012) | EGIL (Bain et al., Leukemia 1998)"
           )
    )
  )
)

# ----------------------------------------------------------
# 4. Server
# ----------------------------------------------------------
server <- function(input, output, session) {
  
  # Map section label to letter
  section_letter <- reactive({
    sel <- input$section_select
    if (sel == "All Sections") return(NULL)
    substr(sel, 1, 1)
  })
  
  # Filtered data
  filtered_data <- reactive({
    df <- all_data
    
    # Section filter
    if (!is.null(section_letter())) {
      df <- df[df$Section == section_letter(), ]
    }
    
    # Text search across all columns
    search_term <- trimws(input$entity_search)
    if (nchar(search_term) > 0) {
      mask <- apply(df, 1, function(row) {
        any(grepl(search_term, row, ignore.case = TRUE))
      })
      df <- df[mask, ]
    }
    
    df
  })
  
  # Section badge
  output$section_badge <- renderUI({
    sel <- input$section_select
    col <- if (sel == "All Sections") "#2980b9" else "#27ae60"
    tags$span(class = "section-badge",
              style = paste0("background:", col, ";"),
              sel
    )
  })
  
  # Record count
  output$record_count <- renderUI({
    n <- nrow(filtered_data())
    tags$span(
      style = "margin-left: 10px; font-size: 12px; color: #7f8c8d;",
      paste0(n, " record", ifelse(n == 1, "", "s"), " found")
    )
  })
  
  # Determine visible columns by section
  cols_to_show <- reactive({
    sec <- section_letter()
    if (is.null(sec)) {
      # All sections — show minimal universal columns
      c("Section", "Section_Title", "Entity", "Reference")
    } else if (sec == "A") {
      c("Entity","Positive_Characteristic","Negative_Absent",
        "Cross_lineage_Aberrant","Key_Notes","Blast_Threshold","Reference")
    } else if (sec == "B") {
      c("Entity","Positive_Characteristic","Negative_Absent",
        "Cross_lineage_Aberrant","Key_Notes","Blast_Threshold","Reference")
    } else if (sec == "C") {
      c("Entity","Blast_Percentage","Key_FC_Findings",
        "IMDSFlow_Compartments","Aberrancy_Rule","Reference")
    } else if (sec == "D") {
      c("Entity","Positive_Defining","Negative_Key",
        "Molecular_FC_Markers","Key_Notes","Reference")
    } else if (sec == "E") {
      c("Entity","Lineage_1_Criteria","Lineage_2_Criteria","Key_Notes","Reference")
    } else if (sec == "F") {
      c("Entity","Positive_Characteristic","Negative_Weak",
        "Scoring_Prognostic","Reference")
    } else if (sec == "G") {
      c("Entity","FC_Marker","Key_Finding","Blast_Threshold","Reference")
    } else {
      names(all_data)
    }
  })
  
  # Main table
  output$main_table <- renderDT({
    df <- filtered_data()
    cols <- cols_to_show()
    # keep only cols that exist in df
    cols <- intersect(cols, names(df))
    df_show <- df[, cols, drop = FALSE]
    
    # Clean column names for display
    names(df_show) <- gsub("_", " ", names(df_show))
    
    page_len <- as.integer(input$page_length)
    
    datatable(
      df_show,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = page_len,
        scrollX = TRUE,
        autoWidth = FALSE,
        columnDefs = list(
          list(width = "220px", targets = 0),   # Entity
          list(width = "180px", targets = 1),
          list(width = "180px", targets = 2)
        ),
        dom = "Bfrtip",
        buttons = c("copy","csv","excel","pdf"),
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().table().header()).css({",
          "    'background-color': '#2c3e50',",
          "    'color': 'white'",
          "  });",
          "}"
        )
      ),
      extensions = "Buttons",
      class = "stripe hover compact",
      escape = FALSE
    ) %>%
      formatStyle(
        columns = seq_len(ncol(df_show)),
        fontSize = "12px",
        verticalAlign = "top"
      )
  })
}

# ----------------------------------------------------------
# 5. Run
# ----------------------------------------------------------
shinyApp(ui = ui, server = server)
