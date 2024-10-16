# Alt Phrases

This is a backend prototype, implementing https://gist.github.com/qnikst/32eb56c7b3f854a1455cd71ddfd717e2

## Running application

```sh
stack run
```

## Running Test Environment

> Test environment provides all the additional services that are required to run
> the application + sample data for them.

```sh
cd ./testEnv
docker-compose up -d
```

## Example use cases

Create new user:

```sh
curl -i -X POST "http://localhost:8080/api/v1/auth/register"\
    -d '{"username": "1", "password": "2"}'\
    --header 'Content-type: application/json'
```

Create new phrase:

```sh
curl -i -X POST "http://localhost:8080/api/v1/phrase"\
    -d '{"text": "test", "groupId": 1, "shouldSpellcheck": false}'\
    --header "Authorization: Basic YWRtaW46YWRtaW4="\
    --header "Content-type: application/json"
```
