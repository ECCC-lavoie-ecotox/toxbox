#' Get integrated data 
#'
#' @description This function is a quick integration procedure among all Excel files.
#'
#' @return a data.frame of contaminants
#' @export 
dbs_integration <- function(){
    contaminants_mapping <- list(
        pasl_herons = list(
            path = "Z:/01-Projets et suivis/PASL/GrandHeron/Base de donnees GBHE oeufs.xlsx",
            sheets = list(
                PFC = c("PFBA", "PFDS"),
                OC = c("1.2.4.5-Tetrachlorobenzene", "TCPM"),
                PCB = c("PCB18/17", "Aroclor1260"),
                BFR = c("BDE-7", "anti-DP"),
                `non-ortho PCBs` = c("PCB-81", "PCB-169"),
                `PCDDs & PCDFs` = c("2378-TCDD", "OCDF"),
                Toxaphene = c("Total toxaphene", "B9-1025"),
                FAME = c("Caproic Acid", "Docosahexaenoic Acid (DHA)"),
                THg = c("THg-dw", "THg-ww"),
                SI = c("d13C", "d34S")
            )
        ),
        pasl_eiders = list(
            path = "Z:/01-Projets et suivis/PASL/EiderDuvet/Base de donnees COEI.xlsx",
            sheets = list(
                PFC = c("PFBA", "PFDS"),
                OC = c("1,2,4,5-Tetrachlorobenzene", "Mirex"),
                PCB = c("PCB17/18", "PCB209"),
                BFR = c("b-TBECH/BDE15", "BB101"),
                THg = c("THg_dw", "THg_ww")
            )
        ),
        pasl_gulls = list(
            path = "Z:/01-Projets et suivis/PASL/GoelandArgenté/Base de donnees HERG.xlsx",
            sheets = list(
                PFC = c("PFBA", "PFDS"),
                OC = c("1,2,4,5-Tetrachlorobenzene", "Mirex"),
                PCB = c("PCB17/18", "PCB209"),
                BFR = c("b-TBECH/BDE15", "BB101"),
                THg = c("THg", "THg"),
                SI = c("d13C", "d34S")
            )
        ),
        pasl_gannets = list(
            path = "Z:/01-Projets et suivis/PASL/FouBassan/Stats_NOGA_Temporal2022/Integration_ST LAWRENCE_Gannets Trends 1969-2019_OC-PCB-FR Metals D-F FAME CNS.xlsx",
            sheets = list(
                OC = c("1245TCB", "Mirex"),
                PCB = c("PCB18/17", "Aroclor1260"),
                BFR = c("BDE-15_B-TBECH", "anti-DP"),
                `Non-ortho PCBs` = c("PCB 81", "PCB 169"),
                `PCDDs & PCDFs` = c("2378-TCDD", "OCDF"),
                Metal = c("THg", "Al"),
                FAME = c("caproic acid", "docosahexaenoic acid (DHA)"),
                SI = c("d15N", "CN")
            )
        ),
        msc_lacombe = list(
            path = "Z:/01-Projets et suivis/Kyle Elliott/RoseLacombe/BD_Rose_MSc.xlsx",
            sheets = list(
                BD_Rose_MSc = c("THg_dw", "Phe")
            )
        )
        # Manque le sampleID
        # msc_lavoie1 = list(
        #     path = "Z:/01-Projets et suivis/MSc-RLavoie/MSc_RLavoie_HgSIA_FoodWeb.xlsx",
        #     sheets = list(
        #         `HgSIA` = c("THg-ww", "CN")
        #     )
        # ),
        # Manque l'année de d'échantillonage
        # msc_lavoie2 = list(
        #     path = "Z:/01-Projets et suivis/MSc-RLavoie/MSc_RLavoie_OC-PCBs-BFR-Hg-SI_Oeufs.xlsx",
        #     sheets = list(
        #         `OCs-PCBs-BFRs` = c(
        #             "Moisture (%)", "Lipid %", "p,p'-DDE", "Hexachlorobenzene", "t-Nonachlor", "Oxychlordane", "Dieldrin", 
        #             "PCB 153", "PCB 138", "PCB 180", "PCB 118", "PCB 187", "BDE-47", "BDE-99", "BDE-100", "BDE-153", "BDE-154/BB-153", 
        #             "d15N", "d13C", "PercC", "PercN", "C:N ratio", "THg  (ng/g)", "MeHg  (ng/g)", "%MeHg", 
        #             "ΣDDT", "ΣCHLOR","ΣCBz", "Dieldrin", "Σmirex", "ΣHCH", "TCPM", "Octachlorostyrene",
        #             "HexaCBs", "HeptaCBs", "PentaCBs", "OctaCBs", "TetraCBs", "NonaCBs", "TriCBs",
        #             "TetraBDEs", "PentaBDEs", "HexaBDEs", "TriBDEs", "DecaBDEs", "HeptaBDEs"
        #         )
        #     )
        # )
    )

    purrr::map(
        # Loop over species dataset
        contaminants_mapping, \(f){
            # Copy the excel file locally in the temp folder
            tmp_file <- tempfile()
            file.copy(f$path, tmp_file, overwrite = TRUE)
            # Loop over sheets
            purrr::map2(f$sheets, names(f$sheets), \(s, n){
                readxl::read_excel(tmp_file, sheet = n) |>
                    dplyr::select("Year", "Location", "SampleID", "Species", s[1]:s[2]) |>
                    dplyr::mutate(source = f$path, conpound_family = n) |>
                    dplyr::mutate(across(everything(), as.character)) |>
                    tidyr::pivot_longer(cols = s[1]:s[2], names_to = "variable") |>
                    dplyr::filter(!is.na(value))
            }) |> dplyr::bind_rows()
    }) |> dplyr::bind_rows()
}


