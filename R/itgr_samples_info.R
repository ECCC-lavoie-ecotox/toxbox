#' Samples informations integration from scattered Excel files
#'
#' @description Function performing sample informations integration from scattered Excel files.
#'
#' @return a data.frame of all sample information
#' @export 
itgr_samples_info <- function(){

    #### Field sample: Grand Héron (GBHE)
    path <- "Z:/01-Projets et suivis/PASL/GrandHeron/Base de donnees GBHE oeufs.xlsx"
    tmp_file <- tempfile()
    file.copy(path, tmp_file, overwrite = TRUE)

    GBHE_field_lab_sample <- readxl::read_excel(tmp_file, "Sample Info") |>
        tidyr::pivot_longer(`Pooled from`:"...35", values_to = "id_field_sample", values_drop_na = TRUE) |>
        dplyr::select(
            id_field_sample,
            id_site = Location,
            id_lab_sample = SampleID,
            id_project = Project,
            age = Age,
            tissue = Tissue,
            collection_date = CollectionDate,
            received_date = ReceivedDate,
            id_source_report = ReportCode
        ) |> dplyr::distinct() |>
        dplyr::mutate(id_species = "GBHE") |>
        dplyr::mutate(collection_date = as.Date(collection_date)) |>
        dplyr::mutate(source = path)

    #### Field sample: Eiders (COEI)
    path <- "Z:/01-Projets et suivis/PASL/EiderDuvet/Base de donnees COEI.xlsx"
    file.copy(path, tmp_file, overwrite = TRUE)
    COEI_field_lab_sample <- readxl::read_excel(tmp_file, "Sample Info") |>
        dplyr::select(
            id_field_sample = ClientID,
            id_site = Location,
            id_lab_sample = SampleID,
            id_project = Project,
            age = Age,
            tissue = Tissue,
            collection_date = CollectionDate,
            received_date = ReceivedDate,
            id_source_report = ReportCode
        ) |> dplyr::distinct() |>
        dplyr::mutate(id_species = "COEI") |>
        dplyr::mutate(collection_date = as.Date(collection_date)) |>
        dplyr::mutate(source = path)
    
    #### Field sample: Gulls (HERG)
    path <- "Z:/01-Projets et suivis/PASL/GoelandArgenté/Base de donnees HERG.xlsx"
    file.copy(path, tmp_file, overwrite = TRUE)
    HERG_field_lab_sample <- readxl::read_excel(tmp_file, "Sample Info") |>
        tidyr::pivot_longer(`Pooled from`:"...19", values_to = "pool_sample", values_drop_na = TRUE) |>
        dplyr::mutate(ClientID = ifelse(stringr::str_detect(tolower(ClientID), "pool"), pool_sample, ClientID)) |>
        dplyr::select(
            id_field_sample = ClientID,
            id_site = Location,
            id_lab_sample = SampleID,
            id_project = Project,
            age = Age,
            tissue = Tissue,
            collection_date = CollectionDate,
            received_date = ReceivedDate,
            id_source_report = ReportCode
        ) |> 
        dplyr::distinct() |>
        dplyr::mutate(id_species = "HERG") |>
        dplyr::mutate(collection_date = as.Date(collection_date)) |>
        dplyr::mutate(source = path)

    #### Field sample: Gannets (NAGO)
    path <- "Z:/01-Projets et suivis/PASL/FouBassan/Stats_NOGA_Temporal2022/Integration_ST LAWRENCE_Gannets Trends 1969-2019_OC-PCB-FR Metals D-F FAME CNS.xlsx"
    file.copy(path, tmp_file, overwrite = TRUE)
    NOGA_field_lab_sample <- readxl::read_excel(tmp_file, "Analyses") |>
        dplyr::select(
            id_field_sample = USOXCapture_ID,
            id_lab_sample = USOX_Analyse_ID,
            id_source_report = RapportLab_ID
        ) |> 
        dplyr::distinct() |>
        dplyr::left_join(
            readxl::read_excel(tmp_file, "Specimens") |>
                dplyr::select(
                    id_field_sample = USOXCapture_ID,
                    id_project = Projet_ID,
                    age = Age,
                    tissue = Tissu,
                    collection_date = DateRecolteSpecimen
                ) 
        ) |> dplyr::distinct() |>
        dplyr::mutate(id_site = "Ile Bonaventure") |>
        dplyr::mutate(id_species = "NOGA") |>
        dplyr::mutate(collection_date = as.Date(collection_date)) |>
        dplyr::mutate(source = path)

    #### Field sample: MSC Rose
    path <- "Z:/01-Projets et suivis/Kyle Elliott/RoseLacombe/BD_Rose_MSc.xlsx"
    file.copy(path, tmp_file, overwrite = TRUE)
    MScLacombe_field_lab_sample <- readxl::read_excel(tmp_file, "BD_Rose_MSc") |>
        dplyr::select(
            id_field_sample = ClientID,
            id_site = Location,
            id_lab_sample = SampleID,
            collection_date = Year,
            id_species = Species
        ) |>
        dplyr::mutate(
            id_project = "MscLacombe",
            collection_date = paste0(collection_date, "-01-01")
        ) |>
        dplyr::mutate(collection_date = as.Date(collection_date)) |>
        dplyr::mutate(source = path)
    
    dplyr::bind_rows(
        GBHE_field_lab_sample, 
        COEI_field_lab_sample, 
        HERG_field_lab_sample, 
        NOGA_field_lab_sample, 
        MScLacombe_field_lab_sample
    ) |> dplyr::distinct()
}


