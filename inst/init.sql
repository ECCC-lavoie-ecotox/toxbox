PRAGMA foreign_keys = ON;

CREATE TABLE species
-- Create a reference table for each species involved in study
(
    id_species TEXT PRIMARY KEY, 
    genus TEXT NOT NULL,
    species TEXT NOT NULL,
    vernacular TEXT NOT NULL
);

CREATE TABLE location 
-- Create reference table for each site study location
(
    id_site TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    lat REAL,
    lon REAL,
    srid INTEGER
    -- FOREIGN KEY(user_id) REFERENCES users(id),
    -- FOREIGN KEY(product_id) REFERENCES products(id)
);

CREATE TABLE field_sample 
-- Create a new table which document all field samples
(
    id_field_sample INTEGER PRIMARY KEY,
    id_site TEXT NOT NULL,
    id_species TEXT NOT NULL,
    project_id TEXT NOT NULL
);

CREATE TABLE lab_sample 
-- Create a new table which document all lab sample
-- Lab sample could be one or multiple field sample pooled
(
    id_lab_sample INTEGER PRIMARY KEY,
    project_id TEXT NOT NULL,
    project_manager TEXT NOT NULL UNIQUE,
    tissue TEXT NOT NULL,
    collection_date
);

-- Create a new table named 'orders'
CREATE TABLE lab_measurement (
    id INTEGER PRIMARY KEY,
    user_id INTEGER,
    product_id INTEGER,
    quantity INTEGER NOT NULL,
    order_date TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY(user_id) REFERENCES users(id),
    FOREIGN KEY(product_id) REFERENCES products(id)
);

-- Create a new table named 'products'
CREATE TABLE analyte (
    project_id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    price REAL NOT NULL,
    quantity INTEGER NOT NULL
);

-- Create a new table named 'products'
CREATE TABLE project (
    project_id INTEGER PRIMARY KEY,
    manager TEXT NOT NULL
);

