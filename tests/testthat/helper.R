
test_db <- function(mockData = FALSE, env = parent.frame()) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

    DBI::dbExecute(con, "DROP TABLE IF EXISTS 'species';")
    DBI::dbExecute(con, "CREATE TABLE 'species' (
            species_id TEXT, 
            genus TEXT, 
            species TEXT NOT NULL,
            PRIMARY KEY (species_id)
        );")

    if(mockData){
        DBI::dbExecute(con, "INSERT INTO 'species' (species_id, genus, species) VALUES ('TSN', 'Lupus', 'Lupus lupus');")
        DBI::dbExecute(con, "INSERT INTO 'species' (species_id, genus, species) VALUES ('TSN2', 'Alces', 'Alces alces');")
        DBI::dbExecute(con, "INSERT INTO 'species' (species_id, genus, species) VALUES ('TSN3', 'Castor', 'Castor canadensis');")
    }

    withr::defer(DBI::dbDisconnect(con), env)
    return(con)
}
