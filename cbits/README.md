## Requirements

* [libvips](https://github.com/libvips/libvips)
* [libexif](https://github.com/libexif/exif/)

## Build

```bash
cc -g -Wall image_vips.c `pkg-config vips --cflags --libs`
```

```bash
cmake .
cmake --build .
```

## Development

Generate `compile_commands.json` for `clangd`
```bash
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1
```
