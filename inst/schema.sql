-- DuckDB Schema for Evoland Plus
-- Based on Database Backend.md documentation

-- Install required extensions
INSTALL spatial;
LOAD spatial;
INSTALL json;
LOAD json;

-- Configuration table
CREATE TABLE config_t (
    config JSON NOT NULL,
    r_obj BLOB
);

-- Coordinates and spatial reference table
CREATE TABLE coords_t (
    id_coord INTEGER PRIMARY KEY,
    lon DOUBLE NOT NULL,
    lat DOUBLE NOT NULL,
    elevation DOUBLE,
    region VARCHAR,
    geom_polygon GEOMETRY
);

-- Time periods metadata
CREATE TABLE periods_t (
    id_period INTEGER PRIMARY KEY,
    start_date DATE NOT NULL,
    end_date DATE NOT NULL,
    is_extrapolated BOOLEAN NOT NULL DEFAULT FALSE
);

-- Land Use/Land Cover metadata
CREATE TABLE lulc_meta_t (
    id_lulc INTEGER PRIMARY KEY,
    name VARCHAR NOT NULL,
    pretty_name VARCHAR NOT NULL,
    description TEXT
);

-- Land Use/Land Cover data
CREATE TABLE lulc_data_t (
    id_coord INTEGER NOT NULL,
    id_lulc INTEGER NOT NULL,
    id_period INTEGER NOT NULL,
    date DATE,
    PRIMARY KEY (id_coord, id_lulc, id_period),
    FOREIGN KEY (id_coord) REFERENCES coords_t(id_coord),
    FOREIGN KEY (id_lulc) REFERENCES lulc_meta_t(id_lulc),
    FOREIGN KEY (id_period) REFERENCES periods_t(id_period)
);

-- Predictor metadata
CREATE TABLE pred_meta_t (
    id_pred INTEGER PRIMARY KEY,
    name VARCHAR NOT NULL,
    pretty_name VARCHAR NOT NULL,
    description TEXT,
    orig_format VARCHAR,
    url VARCHAR,
    unit VARCHAR,
    factor_levels MAP(INTEGER, VARCHAR)
);

-- Predictor data tables for different data types
CREATE TABLE pred_data_t_float (
    id_pred INTEGER NOT NULL,
    id_coord INTEGER NOT NULL,
    id_period INTEGER NOT NULL,
    value DOUBLE NOT NULL,
    PRIMARY KEY (id_pred, id_coord, id_period),
    FOREIGN KEY (id_pred) REFERENCES pred_meta_t(id_pred),
    FOREIGN KEY (id_coord) REFERENCES coords_t(id_coord),
    FOREIGN KEY (id_period) REFERENCES periods_t(id_period)
);

CREATE TABLE pred_data_t_int (
    id_pred INTEGER NOT NULL,
    id_coord INTEGER NOT NULL,
    id_period INTEGER NOT NULL,
    value INTEGER NOT NULL,
    PRIMARY KEY (id_pred, id_coord, id_period),
    FOREIGN KEY (id_pred) REFERENCES pred_meta_t(id_pred),
    FOREIGN KEY (id_coord) REFERENCES coords_t(id_coord),
    FOREIGN KEY (id_period) REFERENCES periods_t(id_period)
);

CREATE TABLE pred_data_t_bool (
    id_pred INTEGER NOT NULL,
    id_coord INTEGER NOT NULL,
    id_period INTEGER NOT NULL,
    value BOOLEAN NOT NULL,
    PRIMARY KEY (id_pred, id_coord, id_period),
    FOREIGN KEY (id_pred) REFERENCES pred_meta_t(id_pred),
    FOREIGN KEY (id_coord) REFERENCES coords_t(id_coord),
    FOREIGN KEY (id_period) REFERENCES periods_t(id_period)
);

-- Transition metadata
CREATE TABLE trans_meta_t (
    id_trans INTEGER PRIMARY KEY,
    id_lulc_anterior INTEGER NOT NULL,
    id_lulc_posterior INTEGER NOT NULL,
    cardinality INTEGER NOT NULL,
    frequency_rel DOUBLE NOT NULL,
    frequency_abs DOUBLE NOT NULL,
    is_viable BOOLEAN NOT NULL DEFAULT TRUE,
    UNIQUE (id_lulc_anterior, id_lulc_posterior),
    FOREIGN KEY (id_lulc_anterior) REFERENCES lulc_meta_t(id_lulc),
    FOREIGN KEY (id_lulc_posterior) REFERENCES lulc_meta_t(id_lulc)
);

-- Transition-predictor relationships (many-to-many)
CREATE TABLE trans_preds_t (
    id_pred INTEGER NOT NULL,
    id_trans INTEGER NOT NULL,
    PRIMARY KEY (id_pred, id_trans),
    FOREIGN KEY (id_pred) REFERENCES pred_meta_t(id_pred),
    FOREIGN KEY (id_trans) REFERENCES trans_meta_t(id_trans)
);

-- Intervention metadata
CREATE TABLE intrv_meta_t (
    id_intrv INTEGER PRIMARY KEY,
    id_period_list INTEGER[] NOT NULL,
    id_trans_list INTEGER[] NOT NULL,
    name VARCHAR NOT NULL,
    pretty_name VARCHAR NOT NULL,
    description TEXT,
    has_mask BOOLEAN NOT NULL DEFAULT FALSE,
    params JSON
);

-- Intervention data (masks and parameters)
CREATE TABLE intrv_data_t (
    id_intrv INTEGER NOT NULL,
    id_coord INTEGER NOT NULL,
    params MAP(VARCHAR, VARCHAR),
    PRIMARY KEY (id_intrv, id_coord),
    FOREIGN KEY (id_intrv) REFERENCES intrv_meta_t(id_intrv),
    FOREIGN KEY (id_coord) REFERENCES coords_t(id_coord)
);

-- Transition models storage
CREATE TABLE trans_models_t (
    id_trans INTEGER NOT NULL,
    id_period INTEGER NOT NULL,
    model_family VARCHAR NOT NULL,
    model_params MAP(VARCHAR, VARCHAR) NOT NULL,
    goodness_of_fit MAP(VARCHAR, DOUBLE) NOT NULL,
    model_obj_part BLOB,
    model_obj_full BLOB,
    FOREIGN KEY (id_trans) REFERENCES trans_meta_t(id_trans),
    FOREIGN KEY (id_period) REFERENCES periods_t(id_period)
);

-- Allocation parameters
CREATE TABLE alloc_params_t (
    id_trans INTEGER NOT NULL,
    id_period INTEGER NOT NULL,
    alloc_params MAP(VARCHAR, VARCHAR) NOT NULL,
    goodness_of_fit MAP(VARCHAR, DOUBLE) NOT NULL,
    PRIMARY KEY (id_trans, id_period),
    FOREIGN KEY (id_trans) REFERENCES trans_meta_t(id_trans),
    FOREIGN KEY (id_period) REFERENCES periods_t(id_period)
);

-- Create indexes for common query patterns
CREATE INDEX idx_lulc_data_coord ON lulc_data_t(id_coord);
CREATE INDEX idx_lulc_data_period ON lulc_data_t(id_period);
CREATE INDEX idx_pred_data_float_coord ON pred_data_t_float(id_coord);
CREATE INDEX idx_pred_data_float_period ON pred_data_t_float(id_period);
CREATE INDEX idx_pred_data_int_coord ON pred_data_t_int(id_coord);
CREATE INDEX idx_pred_data_int_period ON pred_data_t_int(id_period);
CREATE INDEX idx_pred_data_bool_coord ON pred_data_t_bool(id_coord);
CREATE INDEX idx_pred_data_bool_period ON pred_data_t_bool(id_period);
CREATE INDEX idx_coords_geom_polygon ON coords_t USING RTREE(geom_polygon);
