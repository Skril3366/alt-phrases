FROM postgres:16.3-alpine3.20

COPY schema/init.sql /docker-entrypoint-initdb.d/01-init.sql
COPY testEnv/dummy_data.sql /docker-entrypoint-initdb.d/02-dummy_data.sql
