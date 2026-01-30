-- DuckDB Schema for Evoland Plus
-- Based on Database Backend.md documentation

INSTALL spatial;
LOAD spatial;

-- used for storing arbitrary reporting information, such as authors etc
CREATE TABLE reporting_t (
    key VARCHAR PRIMARY KEY,
    value VARCHAR
);

-- Coordinates and spatial reference table
CREATE TABLE coords_t (
    id_coord INTEGER PRIMARY KEY,
    lon DOUBLE NOT NULL,
    lat DOUBLE NOT NULL,
    elevation DOUBLE,
    geom_polygon GEOMETRY
);

-- Time periods metadata
CREATE TABLE periods_t (
    id_period INTEGER PRIMARY KEY,
    start_date DATE NOT NULL,
    end_date DATE NOT NULL,
    is_extrapolated BOOLEAN NOT NULL DEFAULT FALSE
);

-- Runs metadata (hierarchical scenarios)
CREATE TABLE runs_t (
    id_run INTEGER PRIMARY KEY,
    parent_id_run INTEGER,
    description VARCHAR NOT NULL,
    FOREIGN KEY (parent_id_run) REFERENCES runs_t (id_run)
);

-- Land Use/Land Cover metadata
CREATE TABLE lulc_meta_t (
    id_lulc INTEGER PRIMARY KEY,
    name VARCHAR NOT NULL,
    pretty_name VARCHAR NOT NULL,
    description TEXT,
    src_classes INTEGER[]
);

CREATE VIEW lulc_meta_long_v AS
SELECT
    id_lulc,
    name,
    unnest(src_classes) AS src_class
FROM lulc_meta_t;

-- Land Use/Land Cover data
CREATE TABLE lulc_data_t (
    id_run INTEGER NOT NULL DEFAULT 0,
    id_coord INTEGER NOT NULL,
    id_lulc INTEGER NOT NULL,
    id_period INTEGER NOT NULL,
    PRIMARY KEY (id_run, id_coord, id_period),
    FOREIGN KEY (id_run) REFERENCES runs_t (id_run),
    FOREIGN KEY (id_coord) REFERENCES coords_t (id_coord),
    FOREIGN KEY (id_lulc) REFERENCES lulc_meta_t (id_lulc),
    FOREIGN KEY (id_period) REFERENCES periods_t (id_period)
);

CREATE SEQUENCE seq_id_pred START 1;

-- Predictor metadata
CREATE TABLE pred_meta_t (
    id_pred INTEGER PRIMARY KEY DEFAULT nextval('seq_id_pred'),
    name VARCHAR NOT NULL UNIQUE,
    pretty_name VARCHAR NOT NULL,
    description TEXT,
    orig_format VARCHAR,
    sources STRUCT(url VARCHAR, md5sum VARCHAR)[],
    unit VARCHAR,
    factor_levels MAP(INTEGER, VARCHAR)
);

CREATE VIEW pred_sources_v AS
SELECT DISTINCT
    unnest(sources).url AS url,
    unnest(sources).md5sum AS md5sum
FROM pred_meta_t
WHERE sources IS NOT NULL;

-- Predictor data tables for different data types
CREATE TABLE pred_data_t_float (
    id_run INTEGER NOT NULL DEFAULT 0,
    id_pred INTEGER NOT NULL,
    id_coord INTEGER NOT NULL,
    id_period INTEGER NOT NULL,
    value FLOAT NOT NULL,
    PRIMARY KEY (id_run, id_pred, id_coord, id_period),
    FOREIGN KEY (id_run) REFERENCES runs_t (id_run),
    FOREIGN KEY (id_pred) REFERENCES pred_meta_t (id_pred),
    FOREIGN KEY (id_coord) REFERENCES coords_t (id_coord),
    FOREIGN KEY (id_period) REFERENCES periods_t (id_period)
);

CREATE TABLE pred_data_t_int (
    id_run INTEGER NOT NULL DEFAULT 0,
    id_pred INTEGER NOT NULL,
    id_coord INTEGER NOT NULL,
    id_period INTEGER NOT NULL,
    value INTEGER NOT NULL,
    PRIMARY KEY (id_run, id_pred, id_coord, id_period),
    FOREIGN KEY (id_run) REFERENCES runs_t (id_run),
    FOREIGN KEY (id_pred) REFERENCES pred_meta_t (id_pred),
    FOREIGN KEY (id_coord) REFERENCES coords_t (id_coord),
    FOREIGN KEY (id_period) REFERENCES periods_t (id_period)
);

CREATE TABLE pred_data_t_bool (
    id_run INTEGER NOT NULL DEFAULT 0,
    id_pred INTEGER NOT NULL,
    id_coord INTEGER NOT NULL,
    id_period INTEGER NOT NULL,
    value BOOLEAN NOT NULL,
    PRIMARY KEY (id_run, id_pred, id_coord, id_period),
    FOREIGN KEY (id_run) REFERENCES runs_t (id_run),
    FOREIGN KEY (id_pred) REFERENCES pred_meta_t (id_pred),
    FOREIGN KEY (id_coord) REFERENCES coords_t (id_coord),
    FOREIGN KEY (id_period) REFERENCES periods_t (id_period)
);

-- Transition metadata
CREATE SEQUENCE seq_id_trans START 1;

CREATE TABLE trans_meta_t (
    id_trans INTEGER PRIMARY KEY DEFAULT nextval('seq_id_trans'),
    id_lulc_anterior INTEGER NOT NULL,
    id_lulc_posterior INTEGER NOT NULL,
    cardinality INTEGER NOT NULL,
    frequency_rel DOUBLE NOT NULL,
    frequency_abs DOUBLE NOT NULL,
    is_viable BOOLEAN NOT NULL DEFAULT TRUE,
    UNIQUE (id_lulc_anterior, id_lulc_posterior),
    FOREIGN KEY (id_lulc_anterior) REFERENCES lulc_meta_t (id_lulc),
    FOREIGN KEY (id_lulc_posterior) REFERENCES lulc_meta_t (id_lulc)
);

-- Transition-predictor relationships (many-to-many)
CREATE TABLE trans_preds_t (
    id_run INTEGER NOT NULL DEFAULT 0,
    id_pred INTEGER NOT NULL,
    id_trans INTEGER NOT NULL,
    PRIMARY KEY (id_run, id_pred, id_trans),
    FOREIGN KEY (id_run) REFERENCES runs_t (id_run),
    FOREIGN KEY (id_pred) REFERENCES pred_meta_t (id_pred),
    FOREIGN KEY (id_trans) REFERENCES trans_meta_t (id_trans)
);

-- Transition rates
CREATE TABLE trans_rates_t (
    id_run INTEGER NOT NULL DEFAULT 0,
    id_period INTEGER NOT NULL,
    id_trans INTEGER NOT NULL,
    rate DOUBLE NOT NULL,
    PRIMARY KEY (id_run, id_period, id_trans),
    FOREIGN KEY (id_run) REFERENCES runs_t (id_run),
    FOREIGN KEY (id_period) REFERENCES periods_t (id_period),
    FOREIGN KEY (id_trans) REFERENCES trans_meta_t (id_trans)
);

-- Intervention metadata
CREATE TABLE intrv_meta_t (
    id_intrv INTEGER PRIMARY KEY,
    id_period_list INTEGER[],
    id_trans_list INTEGER[],
    pre_allocation BOOLEAN NOT NULL,
    name VARCHAR NOT NULL,
    pretty_name VARCHAR NOT NULL,
    description TEXT,
    sources STRUCT(url VARCHAR, md5sum VARCHAR)[],
    params MAP(VARCHAR, VARCHAR)
);

-- Intervention masks
CREATE TABLE intrv_masks_t (
    id_run INTEGER NOT NULL DEFAULT 0,
    id_intrv INTEGER NOT NULL,
    id_coord INTEGER NOT NULL,
    PRIMARY KEY (id_run, id_intrv, id_coord),
    FOREIGN KEY (id_run) REFERENCES runs_t (id_run),
    FOREIGN KEY (id_intrv) REFERENCES intrv_meta_t (id_intrv),
    FOREIGN KEY (id_coord) REFERENCES coords_t (id_coord)
);

-- Transition models storage
CREATE TABLE trans_models_t (
    id_run INTEGER NOT NULL DEFAULT 0,
    id_trans INTEGER NOT NULL,
    model_family VARCHAR NOT NULL,
    model_params MAP(VARCHAR, VARCHAR) NOT NULL,
    goodness_of_fit MAP(VARCHAR, DOUBLE) NOT NULL,
    fit_call VARCHAR NOT NULL,
    model_obj_part BLOB,
    model_obj_full BLOB,
    PRIMARY KEY (id_run, id_trans, fit_call),
    FOREIGN KEY (id_run) REFERENCES runs_t (id_run),
    FOREIGN KEY (id_trans) REFERENCES trans_meta_t (id_trans)
);

-- Allocation parameters
CREATE TABLE alloc_params_t (
    id_run INTEGER NOT NULL DEFAULT 0,
    id_trans INTEGER NOT NULL,
    mean_patch_size DOUBLE NOT NULL,
    patch_size_variance DOUBLE NOT NULL,
    patch_isometry DOUBLE NOT NULL,
    frac_expander DOUBLE NOT NULL,
    frac_patcher DOUBLE NOT NULL,
    PRIMARY KEY (id_run, id_trans),
    FOREIGN KEY (id_run) REFERENCES runs_t (id_run),
    FOREIGN KEY (id_trans) REFERENCES trans_meta_t (id_trans)
);

-- Neighbors table
CREATE TABLE neighbors_t (
    id_coord_origin INTEGER NOT NULL,
    id_coord_neighbor INTEGER NOT NULL,
    distance DOUBLE NOT NULL,
    distance_class INTEGER,
    PRIMARY KEY (id_coord_origin, id_coord_neighbor),
    FOREIGN KEY (id_coord_origin) REFERENCES coords_t (id_coord),
    FOREIGN KEY (id_coord_neighbor) REFERENCES coords_t (id_coord)
);
