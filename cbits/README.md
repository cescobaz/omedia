## Requirements

[]()

## Build

```bash
cc -g -Wall image_vips.c `pkg-config vips --cflags --libs`
```

```bash
cmake .
cmake --build .
```
