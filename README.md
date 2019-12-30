# Users Service

A simple Users Service made in Haskell.

## API :

#### Sign in

POST /login

```bash
curl -X POST \
  http://localhost:4001/login \
  -H 'Content-Type: application/json' \
  -d '{
	"email": "email@email.com",
	"password": "password"
}'
```

#### Sign up

POST /register

```bash
curl -X POST \
  http://localhost:4001/register \
  -H 'Content-Type: application/json' \
  -d '{
  "email": "email@email.com",
  "firstName": "user",
  "lastName": "user",
  "password" : "password"
}'
```

#### Get user informations

GET /user

```bash
curl -X GET \
  http://localhost:4001/user \
  -H 'Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6InRlc3QyQHRlc3QuY29tIiwiZXhwIjo4NjQwMCwibGFzdE5hbWUiOiJ0ZXN0MiIsImlzcyI6ImF1dGhzZXJ2aWNlIiwiZmlyc3ROYW1lIjoidGVzdDEiLCJpZCI6IjQzYTAyZWVjLTJmMWEtNDU4Zi05M2I2LWIyNzcxM2NlNDQ0ZSJ9.S7QzGxqkX4Z9NyqhQLrvOmhNPm1GTJUHS_ADYNMFXIw'
```

## Configuration

There's environment variables to customise your service :

| Name         | Description                       | Default                                                   |
| ------------ | --------------------------------- | --------------------------------------------------------- |
| PORT         | The listening port of the service | 4001                                                      |
| AUTH_SERVICE | Auth service HTTP endpoint        | http://localhost:4002                                     |
| PG_URl       | PGSQL connectionstring            | postgres://dbUser:dbPassword@localhost:5432/haskell-users |
