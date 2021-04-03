# omedia
organize your media

## Requirements

* [libejdb2](https://github.com/Softmotions/ejdb#use-cases)
* [libvips](https://github.com/libvips/libvips)

## Build and Run
```bash
cabal v2-run
```

## Files structure
media/
thumbnails/
to-import/
database.ejdb

## API
GET `base-path`/media/?sortBy=`field`&sortOrder=`ascending|descending`
GET `base-path`/media/`media-id`
POST `base-path`/media/
