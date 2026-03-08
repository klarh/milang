# BUG: Where-clause record bindings with function params become unbound in imported modules

## Summary

When a function in an imported module creates a record in a where-clause binding
using function parameters, and that record binding is then used as an argument to
an FFI call, the compiler generates code where the binding is unbound at runtime.

## Compiler Version

```
20260307233620-6a56345
```

## Minimal Reproduction

**lib.mi:**
```milang
sdl = import' "/usr/include/SDL2/SDL_ttf.h" ({
  filter = ["SDL_RenderDrawRect"];
  flags = "-lSDL2 -lSDL2_ttf"
})

draw ctx x y = r
  dst = {x = x; y = y; w = 100; h = 20}
  r = sdl.SDL_RenderDrawRect ctx dst
```

**main.mi:**
```milang
lib = import "lib.mi"

sdl = import' "/usr/include/SDL2/SDL.h" ({
  filter = ["SDL_Init", "SDL_Quit", "SDL_CreateWindow", "SDL_DestroyWindow",
            "SDL_CreateRenderer", "SDL_DestroyRenderer",
            "SDL_RenderClear", "SDL_RenderPresent", "SDL_Delay",
            "SDL_INIT_VIDEO",
            "SDL_RENDERER_ACCELERATED", "SDL_RENDERER_PRESENTVSYNC",
            "SDL_WINDOW_SHOWN", "SDL_WINDOWPOS_CENTERED"];
  flags = "-lSDL2"
})

main world = r
  d0 = sdl.SDL_Init sdl.SDL_INIT_VIDEO
  win = sdl.SDL_CreateWindow "test" sdl.SDL_WINDOWPOS_CENTERED sdl.SDL_WINDOWPOS_CENTERED 200 200 sdl.SDL_WINDOW_SHOWN
  ren = sdl.SDL_CreateRenderer win (0 - 1) (sdl.SDL_RENDERER_ACCELERATED + sdl.SDL_RENDERER_PRESENTVSYNC)
  d1 = sdl.SDL_RenderClear ren
  d2 = lib.draw ren 10 10
  d3 = sdl.SDL_RenderPresent ren
  d4 = sdl.SDL_Delay 500
  d5 = sdl.SDL_DestroyRenderer ren
  d6 = sdl.SDL_DestroyWindow win
  r = sdl.SDL_Quit 0
```

**Result:** `unbound variable: dst`

## Precise Trigger Conditions

All three conditions must be true for the bug to occur:

1. The function is defined in an **imported module** (not the main file)
2. A where-clause binding constructs a **record** using **function parameters**:
   `dst = {x = x; y = y; w = 100; h = 20}` (x and y are function params)
3. That binding is used as an **argument to an FFI call**:
   `r = sdl.SDL_RenderDrawRect ctx dst`

## What Works (not affected)

- **Literal records**: `dst = {x = 10; y = 10; w = 100; h = 20}` → OK
- **Non-record bindings with params**: `dst = x + y` → OK
- **Inline record in FFI call**: `r = sdl.SDL_RenderDrawRect ctx ({x = x; y = y; w = 100; h = 20})` → OK
- **Same function in main file** (not imported): → OK
- **Where-clause bindings that don't reference function params**: → OK
- **Function call returning record with params**: `dst = rect x y 100 20` → same bug

## Workaround

Inline record construction directly into FFI calls instead of assigning to a
named where-clause binding:

```milang
-- FAILS:
draw ctx x y = r
  dst = {x = x; y = y; w = 100; h = 20}
  r = sdl.SDL_RenderDrawRect ctx dst

-- WORKS:
draw ctx x y = sdl.SDL_RenderDrawRect ctx ({x = x; y = y; w = 100; h = 20})
```

## Impact

This bug affects `gui.mi`'s `draw_text` function, which constructs SDL_Rect and
SDL_Color records from function parameters. The workaround is to inline all record
construction into the FFI calls.

## Notes

- The number of function parameters does NOT matter (tested with 1-6 args)
- Struct annotations (`ffi.struct`) do NOT affect whether the bug triggers
- The bug is NOT related to pattern matching in other functions in the same module
- Other non-record where-clause bindings referencing function params work fine
