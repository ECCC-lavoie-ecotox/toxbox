#' Measurements integration from scattered Excel files.
#'
#' @description Function performing measurements integration procedure from scattered Excel files.
#'
#' @return a data.frame of all contaminant measurements
#' @export 
itgr_measurements <- function(){
    contaminants_mapping <- list(
        pasl_herons = list(
            path = "Z:/01-Projets et suivis/PASL/GrandHeron/Base de donnees GBHE oeufs.xlsx",
            sheets = list(
                PFC = c("PFBA", "PFDS"),
                OC = c("% Lipids", "TCPM"),
                PCB = c("% Lipids", "Aroclor1260"),
                BFR = c("% Lipids", "anti-DP"),
                `non-ortho PCBs` = c("% Lipids", "PCB-169"),
                `PCDDs & PCDFs` = c("% Lipids", "OCDF"),
                Toxaphene = c("Total toxaphene", "B9-1025"),
                FAME = c("% Lipid", "Docosahexaenoic Acid (DHA)"),
                THg = c("% Moisture", "THg-ww"),
                SI = c("d13C", "d34S")
            )
        ),
        pasl_eiders = list(
            path = "Z:/01-Projets et suivis/PASL/EiderDuvet/Base de donnees COEI.xlsx",
            sheets = list(
                PFC = c("PFBA", "PFDS"),
                OC = c("% Lipids", "Mirex"),
                PCB = c("% Lipids", "PCB209"),
                BFR = c("% Moisture", "BB101"),
                THg = c("% Moisture", "THg_ww")
            )
        ),
        pasl_gulls = list(
            path = "Z:/01-Projets et suivis/PASL/GoelandArgenté/Base de donnees HERG.xlsx",
            sheets = list(
                PFC = c("PFBA", "PFDS"),
                OC = c("% Lipids", "Mirex"),
                PCB = c("% Lipids", "PCB209"),
                BFR = c("% Lipids", "BB101"),
                THg = c("% Moisture", "THg"),
                SI = c("d13C", "d34S")
            )
        ),
        pasl_gannets = list(
            path = "Z:/01-Projets et suivis/PASL/FouBassan/Stats_NOGA_Temporal2022/Integration_ST LAWRENCE_Gannets Trends 1969-2019_OC-PCB-FR Metals D-F FAME CNS.xlsx",
            sheets = list(
                OC = c("Moist", "Mirex"),
                PCB = c("Moist", "Aroclor1260"),
                BFR = c("Moist", "anti-DP"),
                `Non-ortho PCBs` = c("PCB 81", "PCB 169"),
                `PCDDs & PCDFs` = c("Moisture", "OCDF"),
                Metal = c("Moist", "Al"),
                FAME = c("Lipid", "docosahexaenoic acid (DHA)"),
                SI = c("d15N", "CN"),
                SImean = c("d15N", "CN")
            )
        ),
        msc_lacombe = list(
            path = "Z:/01-Projets et suivis/Kyle Elliott/RoseLacombe/BD_Rose_MSc.xlsx",
            sheets = list(
                BD_Rose_MSc = c("THg_dw", "d15N_Phe")
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
                    dplyr::mutate(dplyr::across(everything(), as.character)) |>
                    tidyr::pivot_longer(cols = s[1]:s[2], names_to = "variable") |>
                    dplyr::filter(!is.na(value))
            }) |> dplyr::bind_rows()
        }) |> dplyr::bind_rows()

}


