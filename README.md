# ophoto
organize your photos

## Build and Run
```bash
stack setup
stack build
stack exec ophoto-exe
```

## Files structure
photos/
thumbnails/
to-import/
database.sqlite3

## API
GET `base-path`/photos/?sortBy=`field`&sortOrder=`ascending|descending`
GET `base-path`/photos/`photo-id`
POST `base-path`/photos/
