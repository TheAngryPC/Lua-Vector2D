# Lua Vector2D

A mostly Godot-like implementation of 2D vectors and associated math. Annotated
for use with Lua LSPs.

Best used with LuaJIT / LOVE2D. Lua PUC-Rio sees a 10x to 100x decrease in performance.

## Usage

Create a new 2D vector just like in Godot.

```lua
local a  = Vector2(1, 1)
```

## Installation

Copy the `vector2.lua` file wherever you desire. Then require the package like
any other:

```lua
local Vector2 = require 'vector2'
```

## Benchmarking

```bash
lua bench.lua
```

```bash
luajit bench.lua
```
