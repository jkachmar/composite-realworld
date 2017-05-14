.PHONY: init drop

init:
	psql -f ./resources/sql/init.sql
	psql -d crw_db -c 'CREATE EXTENSION "uuid-ossp";'

drop:
	psql -f ./resources/sql/drop.sql
