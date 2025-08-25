local args = {}
for _, a in ipairs(arg) do
	local k, v = a:match("^%-%-(%w+)=?(.*)$")
	if k then
		args[k] = (v ~= "" and v) or true
	end
end

local ITERS = tonumber(args.iters or 5e6)
local REPEATS = tonumber(args.repeats or 1)
local WARMUP = tonumber(args.warmup or 1)
local CSV = args.csv and true or false
local MODNAME = tostring(args.mod or "vector2")

-- --------- Load module ----------
local ok, Vector2 = pcall(require, MODNAME)
if not ok then
	error("Could not require '" .. MODNAME .. "': " .. tostring(Vector2))
end

-- --------- Runtime info ----------
local runtime = (_G.jit and jit.version) and ("LuaJIT " .. jit.version) or _VERSION
local function header()
	if CSV then
		print("runtime,test,iters,time_s,ops_per_s,delta_kb,notes")
	else
		print(string.rep("=", 78))
		print(("Runtime: %s   | Module: %s"):format(runtime, MODNAME))
		print(string.rep("-", 78))
	end
end

-- --------- Timing/memory utils ----------
local clock = os.clock
local function now()
	return clock()
end

local function bench_once(name, fn, iters)
	collectgarbage()
	collectgarbage()
	local kb_before = collectgarbage("count")
	local t0 = now()
	local note = fn(iters) -- allow fn to return a note (e.g., checksum)
	local t1 = now()
	local kb_after = collectgarbage("count")
	local dt = t1 - t0
	local ops = iters / (dt > 0 and dt or 1e-12)
	local dkb = kb_after - kb_before
	return dt, ops, dkb, note
end

local function bench(name, fn, iters, repeats)
	for _ = 1, WARMUP do
		bench_once(name, fn, math.min(1e5, iters))
	end
	local best = { time = math.huge, ops = 0, dkb = 0, note = nil }
	for _ = 1, repeats do
		local dt, ops, dkb, note = bench_once(name, fn, iters)
		if dt < best.time then
			best = { time = dt, ops = ops, dkb = dkb, note = note }
		end
	end
	if CSV then
		print(
			("%s,%s,%d,%.6f,%.3f,%.1f,%s"):format(
				runtime,
				name,
				iters,
				best.time,
				best.ops,
				best.dkb,
				best.note and tostring(best.note) or ""
			)
		)
	else
		io.write(
			("%-28s iters=%-8.0f time=%6.3fs  ops/s=%-10.3f  Δmem=%7.1f KB"):format(
				name,
				iters,
				best.time,
				best.ops,
				best.dkb
			)
		)
		if best.note then
			io.write(("  (%s)"):format(tostring(best.note)))
		end
		io.write("\n")
	end
end

-- --------- Random data (stable seed) ----------
math.randomseed(1234567)
local VCOUNT = 100000
local R = {}
for i = 1, VCOUNT do
	local x = (math.random() * 2000 - 1000)
	local y = (math.random() * 2000 - 1000)
	local a = (math.random() * 2 * math.pi - math.pi)
	R[i] = { x = x, y = y, a = a }
end

-- --------- Helpers ----------
local function V(i)
	local r = R[(i - 1) % VCOUNT + 1]
	return Vector2(r.x, r.y)
end
local function A(i)
	local r = R[(i - 1) % VCOUNT + 1]
	return r.a
end

-- Ensure a small side effect so loops aren't completely dead
local sink = 0
local function consume(v)
	if type(v) == "table" then
		sink = sink + (v.x or 0) + (v.y or 0)
	else
		sink = sink + (v or 0)
	end
end

-- --------- Correctness smoke tests (fast) ----------

local function assert_close(a, b, eps)
	if math.abs(a - b) > (eps or 1e-9) then
		error(("assert_close %.12f vs %.12f"):format(a, b))
	end
end

local function sanity()
	-- equality & arithmetic
	assert(Vector2(1, 2) == Vector2(1, 2))
	assert(Vector2(1, 2) + Vector2(3, 4) == Vector2(4, 6))
	assert(Vector2(3, 4):length() == 5)

	-- angle (atan2 form)
	assert_close(Vector2.angle(Vector2(0, 1)), math.atan(1, 0)) -- ~pi/2

	-- distance / projection
	local d = Vector2.distanceTo(Vector2(0, 0), Vector2(3, 4))
	assert_close(d, 5)
	local p = Vector2.project(Vector2(3, 4), Vector2(1, 0))
	assert_close(p.x, 3)
	assert_close(p.y, 0)

	-- reflect about vertical line (direction (0,1)) -> x flips, y stays
	local r = Vector2.reflect(Vector2(1, -1), Vector2(0, 1))
	assert_close(r.x, -1)
	assert_close(r.y, -1)

	-- slide along normal (0,1): removes y component
	local sl = Vector2.slide(Vector2(3, 4), Vector2(0, 1))
	assert_close(sl.x, 3)
	assert_close(sl.y, 0)

	-- finite/zero checks
	assert(Vector2.isZeroApprox(Vector2(0, 0)))
	assert(Vector2.isFinite(Vector2(1, 1)))
end

-- --------- Bench suite ----------
local suite = {
	{
		name = "__call constructor",
		iters = ITERS,
		fn = function(n)
			for i = 1, n do
				consume(Vector2(i % 10, -(i % 7)))
			end
		end,
	},
	{
		name = "copy constructor",
		iters = ITERS,
		fn = function(n)
			local v = Vector2(1, 2)
			for i = 1, n do
				consume(Vector2(v))
			end
		end,
	},
	{
		name = "add (alloc per op)",
		iters = ITERS,
		fn = function(n)
			local a, b = Vector2(1, 2), Vector2(3, 4)
			for i = 1, n do
				a = a + b
				consume(a)
			end
		end,
	},
	{
		name = "mul scalar right v*s",
		iters = ITERS,
		fn = function(n)
			local a = Vector2(0.5, 2.0)
			for i = 1, n do
				a = a * 1.001
				consume(a)
			end
		end,
	},
	{
		name = "mul scalar left s*v",
		iters = ITERS,
		fn = function(n)
			local a = Vector2(0.5, 2.0)
			for i = 1, n do
				a = 1.001 * a
				consume(a)
			end
		end,
	},
	{
		name = "dot (no alloc)",
		iters = ITERS,
		fn = function(n)
			local a, b = Vector2(3.4, -2.1), Vector2(-1.7, 0.25)
			local s = 0
			for i = 1, n do
				s = s + a:dot(b)
			end
			return ("sum=%.3f"):format(s)
		end,
	},
	{
		name = "length / lengthSquared",
		iters = ITERS,
		fn = function(n)
			local v = Vector2(1.234, -5.678)
			local s = 0
			for i = 1, n do
				s = s + v:length() + v:lengthSquared()
			end
			return ("sum=%.3f"):format(s)
		end,
	},
	{
		name = "normalized()",
		iters = ITERS,
		fn = function(n)
			local v = Vector2(3, 4)
			for i = 1, n do
				v = Vector2.normalized(v)
				consume(v)
			end
		end,
	},
	{
		name = "rotated()",
		iters = ITERS,
		fn = function(n)
			local v, a = Vector2(10, 0), 0.0005
			for i = 1, n do
				v = Vector2.rotated(v, a)
				consume(v)
			end
		end,
	},
	{
		name = "angle()/angleTo()/angle2P",
		iters = ITERS,
		fn = function(n)
			local sum = 0
			for i = 1, n do
				local v1, v2 = V(i), V(i + 777)
				sum = sum + Vector2.angle(v1) + Vector2.angleTo(v1, v2) + Vector2.angleToPoint(v1, v2)
			end
			return ("sum=%.3f"):format(sum)
		end,
	},
	{
		name = "clamp (number,number)",
		iters = ITERS,
		fn = function(n)
			local lo, hi = -100, 100
			for i = 1, n do
				local r = R[(i - 1) % VCOUNT + 1]
				consume(Vector2.clamp(Vector2(r.x, r.y), lo, hi))
			end
		end,
	},
	{
		name = "clamp (vec,vec)",
		iters = ITERS,
		fn = function(n)
			local minv, maxv = Vector2(-50, -75), Vector2(50, 75)
			for i = 1, n do
				local r = R[(i - 1) % VCOUNT + 1]
				consume(Vector2.clamp(Vector2(r.x, r.y), minv, maxv))
			end
		end,
	},
	{
		name = "project/reflect/slide",
		iters = ITERS,
		fn = function(n)
			local v = Vector2(3, 4)
			local line = Vector2(2, 1)
			local s = 0
			for i = 1, n do
				local p = Vector2.project(v, line)
				local r = Vector2.reflect(v, line)
				local sl = Vector2.slide(v, line)
				s = s + p.x + r.y + sl.x
			end
			return ("mix=%.3f"):format(s)
		end,
	},
	{
		name = "slerp() (heavier)",
		iters = math.floor(ITERS / 10),
		fn = function(n)
			local a, b = Vector2(10, 0), Vector2(0, 5)
			local sum = 0
			for i = 1, n do
				local t = (i % 1000) / 1000
				local v = Vector2.slerp(a, b, t)
				sum = sum + v.x
			end
			return ("sum=%.3f"):format(sum)
		end,
	},
	{
		name = "directionTo/moveToward",
		iters = ITERS,
		fn = function(n)
			local s = 0
			for i = 1, n do
				local a, b = V(i), V(i + 333)
				local d = Vector2.directionTo(a, b)
				local m = Vector2.moveToward(a, b, 0.01)
				s = s + d.x + m.y
			end
			return ("sum=%.3f"):format(s)
		end,
	},
	{
		name = "snapped/round/ceil/floor",
		iters = ITERS,
		fn = function(n)
			local s = 0
			for i = 1, n do
				local v = V(i)
				local w = Vector2.snapped(v, 0.25)
				local r = Vector2.round(v)
				local c = Vector2.ceil(v)
				local f = Vector2.floor(v)
				s = s + w.x + r.y + c.x + f.y
			end
			return ("sum=%.3f"):format(s)
		end,
	},
	{
		name = "fromAngle()/orthogonal()",
		iters = ITERS,
		fn = function(n)
			local s = 0
			for i = 1, n do
				local v = Vector2.fromAngle(A(i))
				local o = Vector2.orthogonal(v)
				s = s + v.x * o.y - v.y * o.x
			end
			return ("area=%.3f"):format(s)
		end,
	},
	{
		name = "batch add+normalized (100k)",
		iters = 1,
		fn = function(_)
			local out = {}
			for i = 1, 100000 do
				local r = R[i]
				out[i] = Vector2.normalized(Vector2(r.x, r.y) + Vector2(r.y, r.x))
			end
			return ("out=%d"):format(#out)
		end,
	},
	{
		name = "comparisons (__lt/__le)",
		iters = ITERS,
		fn = function(n)
			local a, b = Vector2(3, 4), Vector2(5, 12)
			local c = 0
			for i = 1, n do
				if a < b then
					c = c + 1
				end
				if a <= b then
					c = c + 1
				end
			end
			return ("c=%d"):format(c)
		end,
	},
}

-- --------- Run ----------
local function main()
	header()
	sanity()
	for _, t in ipairs(suite) do
		bench(t.name, t.fn, t.iters, REPEATS)
	end
	if not CSV then
		print(("\nSink check (ignore): %g"):format(sink))
	end
end

main()
