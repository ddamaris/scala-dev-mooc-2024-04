--setup database
drop DATABASE IF EXISTS demo;
create DATABASE demo;
\c demo;

drop table if exists public.product;

create table if not exists public.product (
    id BIGINT NOT NULL,
    title TEXT,
    PRIMARY KEY(id)
);

CREATE TABLE IF NOT EXISTS public.akka_projection_offset_store (
    projection_name VARCHAR(255) NOT NULL,
    projection_key VARCHAR(255) NOT NULL,
    current_offset VARCHAR(255) NOT NULL,
    manifest VARCHAR(4) NOT NULL,
    mergeable BOOLEAN NOT NULL,
    last_updated BIGINT NOT NULL,
    PRIMARY KEY(projection_name, projection_key)
);

CREATE INDEX IF NOT EXISTS akka_projection_name_index ON public.akka_projection_offset_store (projection_name);

CREATE TABLE IF NOT EXISTS public.akka_projection_management (
    projection_name VARCHAR(255) NOT NULL,
    projection_key VARCHAR(255) NOT NULL,
    paused BOOLEAN NOT NULL,
    last_updated BIGINT NOT NULL,
    PRIMARY KEY(projection_name, projection_key)
);