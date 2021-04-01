## Requirements

[]()

## Build

```bash
cc -g -Wall image_vips.c `pkg-config vips --cflags --libs`
```

```bash
cc -g -Wall image_wand.c `pkg-config MagickWand --cflags --libs`
```
