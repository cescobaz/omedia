# omedia
organize your media

## requirements

* [libejdb2](https://github.com/Softmotions/ejdb)
* [libvips](https://github.com/libvips/libvips)
* [libexif](https://github.com/libexif/exif/)

## build and run
```bash
cabal run omedia-exe
```

## files structure
media/
thumbnails/
to-import/
database.ejdb

## API
GET `base-path`/media/?sortBy=`field`&sortOrder=`ascending|descending`
GET `base-path`/media/`media-id`
POST `base-path`/media/
