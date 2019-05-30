CREATE EXTENSION IF NOT EXISTS "uuid-ossp";


CREATE TABLE "users" (
  id            uuid PRIMARY KEY NOT NULL,
  password      VARCHAR(255) NOT NULL,
  email         VARCHAR(255) NOT NULL UNIQUE,
  firstname     VARCHAR(255) NOT NULL,
  lastname      VARCHAR(255)
);