-- DuckDB Schema for Evoland Plus
-- Based on Database Backend.md documentation

-- TODO coord polygons aren't actually yet implemented, could get rid of this now?
INSTALL spatial;
LOAD spatial;

-- used for storing arbitrary reporting information, such as authors etc
CREATE TABLE reporting_t (
    key VARCHAR PRIMARY KEY,
    value VARCHAR
);

-- Coordinates and spatial reference table
-- region column could be added to this table as enum, but only using ALTER TABLE
-- we first need to know which regions exist to declare the enum
CREATE TABLE coords_t (
    id_coord UINT32 PRIMARY KEY,
    lon DOUBLE NOT NULL,
    lat DOUBLE NOT NULL,
    elevation DOUBLE,
    geom_polygon GEOMETRY
);

-- Time periods metadata
CREATE TABLE periods_t (
    id_period UINT8 PRIMARY KEY,
    start_date DATE NOT NULL,
    end_date DATE NOT NULL,
    is_extrapolated BOOLEAN NOT NULL DEFAULT FALSE
);

-- Land Use/Land Cover metadata
CREATE TABLE lulc_meta_t (
    id_lulc UINT8 PRIMARY KEY,
    name VARCHAR NOT NULL,
    pretty_name VARCHAR NOT NULL,
    description TEXT,
    src_classes INTEGER[]
);

CREATE VIEW lulc_meta_long_v as (
    select
        id_lulc,
        name,
        unnest(src_classes) as src_class
    from
        lulc_meta_t
);

-- Land Use/Land Cover data
CREATE TABLE lulc_data_t (
    id_coord UINT32 NOT NULL,
    id_lulc UINT8 NOT NULL,
    id_period UINT8 NOT NULL,
    PRIMARY KEY (id_coord, id_lulc, id_period),
);

CREATE SEQUENCE seq_id_pred START 1;
-- Predictor metadata
CREATE TABLE pred_meta_t (
    id_pred UINT8 PRIMARY KEY default nextval('seq_id_pred'),
    name VARCHAR NOT NULL UNIQUE,
    pretty_name VARCHAR NOT NULL,
    description TEXT,
    orig_format VARCHAR,
    -- list of structs, can be unnest() ed
    sources struct(url varchar, md5sum varchar)[],
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
    id_pred UINT8 NOT NULL,
    id_coord UINT32 NOT NULL,
    id_period UINT8 NOT NULL,
    value FLOAT NOT NULL,
    PRIMARY KEY (id_pred, id_coord, id_period),
);

CREATE TABLE pred_data_t_int (
    id_pred UINT8 NOT NULL,
    id_coord UINT32 NOT NULL,
    id_period UINT8 NOT NULL,
    value INTEGER NOT NULL,
    PRIMARY KEY (id_pred, id_coord, id_period),
);

CREATE TABLE pred_data_t_bool (
    id_pred UINT8 NOT NULL,
    id_coord UINT32 NOT NULL,
    id_period UINT8 NOT NULL,
    value BOOLEAN NOT NULL,
    PRIMARY KEY (id_pred, id_coord, id_period),
);

-- Transition metadata
CREATE SEQUENCE seq_id_trans START 1;
CREATE TABLE trans_meta_t (
    id_trans UINT8 PRIMARY KEY default nextval('seq_id_trans'),
    id_lulc_anterior UINT8 NOT NULL,
    id_lulc_posterior UINT8 NOT NULL,
    cardinality INTEGER NOT NULL,
    frequency_rel DOUBLE NOT NULL,
    frequency_abs DOUBLE NOT NULL,
    is_viable BOOLEAN NOT NULL DEFAULT TRUE,
    UNIQUE (id_lulc_anterior, id_lulc_posterior),
);

-- Transition-predictor relationships (many-to-many)
CREATE TABLE trans_preds_t (
    id_pred UINT8 NOT NULL,
    id_trans UINT8 NOT NULL,
);

-- Intervention metadata
CREATE TABLE intrv_meta_t (
    id_intrv UINT8 PRIMARY KEY,
    id_period_list UINT8[],
    id_trans_list UINT8[],
    pre_allocation BOOLEAN NOT NULL,
    name VARCHAR NOT NULL,
    pretty_name VARCHAR NOT NULL,
    description TEXT,
    sources struct(url varchar, md5sum varchar)[],
    params MAP(VARCHAR, VARCHAR)
);

-- Intervention masks
CREATE TABLE intrv_masks_t (
    id_intrv UINT8 NOT NULL,
    id_coord UINT32 NOT NULL,
);

-- Transition models storage
CREATE TABLE trans_models_t (
    id_trans UINT8 NOT NULL,
    id_period UINT8 NOT NULL,
    model_family VARCHAR NOT NULL,
    model_params MAP(VARCHAR, VARCHAR) NOT NULL,
    goodness_of_fit MAP(VARCHAR, DOUBLE) NOT NULL,
    model_obj_part BLOB,
    model_obj_full BLOB,
    PRIMARY KEY (id_trans, id_period),
);

-- Allocation parameters
CREATE TABLE alloc_params_t (
    id_trans UINT8 NOT NULL,
    id_period UINT8 NOT NULL,
    alloc_params MAP(VARCHAR, DOUBLE) NOT NULL,
    goodness_of_fit MAP(VARCHAR, DOUBLE) NOT NULL,
    PRIMARY KEY (id_trans, id_period),
);

