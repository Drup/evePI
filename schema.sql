
DROP TABLE IF EXISTS users;
CREATE TABLE users (
       id BIGSERIAL PRIMARY KEY,
       name text UNIQUE NOT NULL,	  
       password text NOT NULL
);

DROP TABLE IF EXISTS projects;
CREATE TABLE projects (
       id BIGSERIAL PRIMARY KEY,
		 name text UNIQUE NOT NULL,
		 description text NOT NULL
);

DROP TABLE IF EXISTS projects_tree;
CREATE TABLE projects_tree (
		 project_id bigint NOT NULL references projects(id) on delete cascade,
		 product_id BIGSERIAL PRIMARY KEY,
		 parent_id bigint DEFAULT NULL references projects_tree(product_id) on delete cascade,
		 notes text DEFAULT NULL
);

DROP TABLE IF EXISTS products ;
CREATE TABLE products (
		 id bigint NOT NULL references projects_tree(product_id) on delete cascade,
		 typeid integer NOT NULL
) ;

DROP TABLE IF EXISTS planets;
CREATE TABLE planets (
		 id BIGSERIAL PRIMARY KEY, 
		 user_id bigint NOT NULL references users(id) on delete cascade,
		 project_id bigint DEFAULT NULL references projects(id) on delete SET NULL,
		 product_id bigint DEFAULT NULL references projects_tree(product_id) on delete SET NULL,
		 location text NOT NULL,
		 type text NOT NULL,
		 notes text DEFAULT NULL
);

DROP TABLE IF EXISTS projects_users ;
CREATE TABLE projects_users (
		 project_id bigint NOT NULL references projects(id) on delete cascade,
		 user_id bigint NOT NULL references users(id) on delete cascade
);

DROP TABLE IF EXISTS projects_admins ;
CREATE TABLE projects_admins (
		 project_id bigint NOT NULL references projects(id) on delete cascade,
		 user_id bigint NOT NULL references users(id) on delete cascade
);
