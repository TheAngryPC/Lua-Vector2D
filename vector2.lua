local abs = math.abs
local ceil = math.ceil
local floor = math.floor
local max = math.max
local min = math.min
local sin = math.sin
local cos = math.cos
local atan = math.atan
local sqrt = math.sqrt
local huge = math.huge

---@class Vector2
---@field x number The vector's x component.
---@field y number The vector's y component.
---@field protected _isVector boolean Private attribute that helps with typing.
---@operator add(Vector2): Vector2
---@operator sub(Vector2): Vector2
---@operator mul(Vector2): Vector2
---@operator div(Vector2): Vector2
---@operator unm: Vector2
---@operator call: Vector2
local Vector2 = {}
Vector2.__index = Vector2

local function new(x, y)
	return setmetatable({ x = x or 0, y = y or 0, _isVector = true }, Vector2)
end

setmetatable(Vector2, {
	__call = function(_, xOrFrom, yOrNil)
		-- hot, zero-branch fast path: two numbers
		if yOrNil ~= nil then
			-- assumes numbers; remove the or 0 to avoid extra ops if you validate elsewhere
			return new(xOrFrom, yOrNil)
		end
		-- optional copy branch (costly). If you can live without Vector2(vec), delete it.
		if xOrFrom and xOrFrom._isVector then
			return new(xOrFrom.x, xOrFrom.y)
		end
		error("Vector2(x, y) expected or Vector2(vec)")
	end,
})

function Vector2.__add(a, b)
	return new(a.x + b.x, a.y + b.y)
end

function Vector2.__sub(a, b)
	return new(a.x - b.x, a.y - b.y)
end

function Vector2.__unm(a)
	return new(-a.x, -a.y)
end

function Vector2.__mul(a, b)
	if type(a) == "number" then
		return new(a * b.x, a * b.y)
	elseif type(b) == "number" then
		return new(a.x * b, a.y * b)
	else
		return new(a.x * b.x, a.y * b.y)
	end
end

function Vector2.__div(a, b)
	if type(a) == "number" then
		return new(a / b.x, a / b.y)
	elseif type(b) == "number" then
		return new(a.x / b, a.y / b)
	else
		return new(a.x / b.x, a.y / b.y)
	end
end

function Vector2.__eq(a, b)
	return a.x == b.x and a.y == b.y
end

function Vector2.__lt(a, b)
	local ax, ay, bx, by = a.x, a.y, b.x, b.y
	return ax * ax + ay * ay < bx * bx + by * by
end

function Vector2.__le(a, b)
	local ax, ay, bx, by = a.x, a.y, b.x, b.y
	return ax * ax + ay * ay <= bx * bx + by * by
end

function Vector2.__tostring(v)
	return "(" .. v.x .. ", " .. v.y .. ")"
end

---Returns a new vector with all components set to their absolute values.
---@param v Vector2
---@return Vector2
function Vector2.abs(v)
	return new(abs(v.x), abs(v.y))
end

function Vector2:add(otherVector2)
	return new(self.x + otherVector2.x, self.y + otherVector2.y)
end

---Returns the vector's angle with respect to the positive X-axis or vector, in radians.
---@param v Vector2
---@return number
function Vector2.angle(v)
	return atan(v.y, v.x)
end

---Returns the signed angle to the given vector, in radians.
---@param a Vector2
---@param b Vector2
---@return number
function Vector2.angleTo(a, b)
	return atan(a.x * b.y - a.y * b.x, a.x * b.x + a.y * b.y)
end

---Returns the angle between the line connecting the two points and the X-axis, in radians.
---@param a Vector2
---@param b Vector2
---@return number
function Vector2.angleToPoint(a, b)
	return atan(b.y - a.y, b.x - a.x)
end

---Returns the aspect ratio of this vector, the ratio of x to y.
---@param v Vector2
---@return number
function Vector2.aspect(v)
	return v.x / v.y
end

---Returns the derivative at the given `t`` on the Bezier curve defined by this vector(v0) and the given v1, v2, and endVec points.
---@param v0 Vector2
---@param v1 Vector2
---@param v2 Vector2
---@param endVec Vector2
---@param t number
---@return Vector2
function Vector2.bezierDerivative(v0, v1, v2, endVec, t)
	local it = 1 - t
	local t2 = t * t
	local it2 = it * it
	local x = 3 * it2 * (v1.x - v0.x) + 6 * it * t * (v2.x - v1.x) + 3 * t2 * (endVec.x - v2.x)
	local y = 3 * it2 * (v1.y - v0.y) + 6 * it * t * (v2.y - v1.y) + 3 * t2 * (endVec.y - v2.y)
	return new(x, y)
end

---Returns the point at the given `t` on the Bezier curve defined by this vector(v0) and the given v1, v2, and endVec points.
---@param v0 Vector2
---@param v1 Vector2
---@param v2 Vector2
---@param endVec Vector2
---@param t number
---@return Vector2
function Vector2.bezierInterpolate(v0, v1, v2, endVec, t)
	local it = 1 - t
	local it2 = it * it
	local t2 = t * t
	local t3 = t2 * t
	local it3 = it2 * it
	local x = it3 * v0.x + 3 * it2 * t * v1.x + 3 * it * t2 * v2.x + t3 * endVec.x
	local y = it3 * v0.y + 3 * it2 * t * v1.y + 3 * it * t2 * v2.y + t3 * endVec.y
	return new(x, y)
end

---Returns the vector "bounced off" from a line defined by the given normal perpendicular to the line.
---@param v Vector2
---@param normal Vector2
---@return Vector2
function Vector2.bounce(v, normal)
	local dot = v:dot(normal)
	return new(v.x - 2 * dot * normal.x, v.y - 2 * dot * normal.y)
end

---Returns a new vector with all components rounded up.
---@param v Vector2
---@return Vector2
function Vector2.ceil(v)
	return new(ceil(v.x), ceil(v.y))
end

---Returns the 2D analog of the cross product for this vector(a) and `b`.
---
---This is the signed area of the parallelogram formed by the two vectors.
---If the second vector is clockwise from the first vector, then the cross
---product is the positive area. If counter-clockwise, the cross product is
---the negative area. If the two vectors are parallel, this returns zero,
---making it useful for testing if two vectors are parallel.
---@param a Vector2
---@param b Vector2
---@return number
function Vector2.cross(a, b)
	return a.x * b.y - a.y * b.x
end

---Returns the clamped vector.
---@param v Vector2
---@param minimum number | Vector2
---@param maximum number | Vector2
---@return Vector2
function Vector2.clamp(v, minimum, maximum)
	local x = v.x
	local y = v.y
	if type(minimum) == "number" and type(maximum) == "number" then
		x = min(x, maximum)
		x = max(x, minimum)
		y = min(y, maximum)
		y = max(y, minimum)
	elseif minimum._isVector and maximum._isVector then
		x = min(x, maximum.x)
		x = max(x, minimum.x)
		y = min(y, maximum.y)
		y = max(y, minimum.y)
	else -- TODO: Use better error logging / handling
		error(
			"\n\nVALUE ERROR:\nmethod `Vector2.clamp(v, minimum, maximum)` paramaters `minimum` and `maximum` "
				.. "must be type <number> or <Vector2>\n\n"
				.. "got types:\n"
				.. "\tminimum=<"
				.. type(minimum)
				.. ">\n"
				.. "\tmaximum=<"
				.. type(maximum)
				.. ">\n"
		)
	end
	return new(x, y)
end

---Performs a cubic interpolation between this vector and `b` using `preA` and `preB`
---as handles, and returns the result at position `weight`. `weight` is on the range
---of 0.0 to 1.0, representing the amount of interpolation.
---@param a Vector2
---@param b Vector2
---@param preA Vector2
---@param preB Vector2
---@param weight number
---@return Vector2
function Vector2.cubicInterpolate(a, b, preA, preB, weight)
	local t = weight
	local t2 = t * t
	local t3 = t2 * t
	local x = 0.5
		* (
			(2 * a.x)
			+ (-preA.x + b.x) * t
			+ (2 * preA.x - 5 * a.x + 4 * b.x - preB.x) * t2
			+ (-preA.x + 3 * a.x - 3 * b.x + preB.x) * t3
		)
	local y = 0.5
		* (
			(2 * a.y)
			+ (-preA.y + b.y) * t
			+ (2 * preA.y - 5 * a.y + 4 * b.y - preB.y) * t2
			+ (-preA.y + 3 * a.y - 3 * b.y + preB.y) * t3
		)
	return new(x, y)
end

---Performs a cubic interpolation between this vector and `b` using `preA` and `preB`
---as handles, and returns the result at position `weight`. `weight` is on the range
---of 0.0 to 1.0, representing the amount of interpolation.
---
---It can perform smoother interpolation that `cubicInterpolate` by the time values.
---@param a Vector2
---@param b Vector2
---@param preA Vector2
---@param preB Vector2
---@param weight number
---@param bT number
---@param preAT number
---@param postBT number
---@return Vector2
function Vector2.cubicInterpolateInTime(a, b, preA, preB, weight, bT, preAT, postBT)
	local t = weight
	local t2 = t * t
	local t3 = t2 * t
	local p0 = preA
	local p1 = a
	local p2 = b
	local p3 = preB
	local t0 = preAT
	local t1 = 0
	local t2v = bT
	local t3v = postBT

	local m1x = (p1.x - p0.x) / (t1 - t0) - (p2.x - p0.x) / (t2v - t0) + (p2.x - p1.x) / (t2v - t1)
	local m1y = (p1.y - p0.y) / (t1 - t0) - (p2.y - p0.y) / (t2v - t0) + (p2.y - p1.y) / (t2v - t1)
	local m2x = (p2.x - p1.x) / (t2v - t1) - (p3.x - p1.x) / (t3v - t1) + (p3.x - p2.x) / (t3v - t2v)
	local m2y = (p2.y - p1.y) / (t2v - t1) - (p3.y - p1.y) / (t3v - t1) + (p3.y - p2.y) / (t3v - t2v)

	local acoef = 2 * t3 - 3 * t2 + 1
	local bcoef = -2 * t3 + 3 * t2
	local ccoef = t3 - 2 * t2 + t
	local dcoef = t3 - t2

	local x = acoef * p1.x + bcoef * p2.x + ccoef * m1x + dcoef * m2x
	local y = acoef * p1.y + bcoef * p2.y + ccoef * m1y + dcoef * m2y
	return new(x, y)
end

---Returns the normalized vector pointing from this vector to `b`.
---@param a Vector2
---@param b Vector2
function Vector2.directionTo(a, b)
	return Vector2.normalized(b - a)
end

---Returns the squared distance between `a` and `b`. This method runs faster than
---distanceTo. When comparing Vector2's this should be the prefered method.
---@param a Vector2
---@param b Vector2
---@return number
function Vector2.distanceSquaredTo(a, b)
	local dx = b.x - a.x
	local dy = b.y - a.y
	return dx * dx + dy * dy
end

---Returns the distance from `a` to `b`.
---@param a Vector2
---@param b Vector2
function Vector2.distanceTo(a, b)
	return sqrt(Vector2.distanceSquaredTo(a, b))
end

---Returns the dot product of this vector and with. This can be used to compare
---the angle between two vectors. For example, this can be used to determine whether
---an enemy is facing the player.
---
---The dot product will be 0 for a right angle (90 degrees), greater than 0 for
---angles narrower than 90 degrees and lower than 0 for angles wider than 90 degrees.
---
---When using unit (normalized) vectors, the result will always be between -1.0
---(180 degree angle) when the vectors are facing opposite directions, and 1.0
---(0 degree angle) when the vectors are aligned.
---
---Note: a.dot(b) is equivalent to b.dot(a).
---@param otherVector2 Vector2
---@return number
function Vector2:dot(otherVector2)
	return self.x * otherVector2.x + self.y * otherVector2.y
end

---Returns a new vector with all components rounded down.
---@param v Vector2
---@return Vector2
function Vector2.floor(v)
	return new(floor(v.x), floor(v.y))
end

---Creates a unit Vector2 rotated to the given angle in radians.
---@param angle number
---@return Vector2
function Vector2.fromAngle(angle)
	return new(cos(angle), sin(angle))
end

---Returns <true> if this vector (a) and (b) are approximately equal.
---@param a Vector2
---@param b Vector2
---@return boolean
function Vector2.isEqualApprox(a, b)
	return abs(a.x - b.x) <= 1e-5 and abs(a.y - b.y) <= 1e-5
end

---Returns <true> if this vector is less than <inf>.
---@param v Vector2
---@return boolean
function Vector2.isFinite(v)
	local x, y = v.x, v.y
	return x == x and y == y and x ~= huge and y ~= huge and x ~= -huge and y ~= -huge
end

---Returns <true> if this vector's length is equal to 1.
---@param v Vector2
---@return boolean
function Vector2.isNormalized(v)
	return abs(v:length() - 1) <= 1e-5
end

---Returns <true> if the vector is approximately equal to (0, 0).
---@param v Vector2
---@return boolean
function Vector2.isZeroApprox(v)
	return abs(v.x) <= 1e-5 and abs(v.y) <= 1e-5
end

---Returns the magnitude of the vector.
---@return number
function Vector2:length()
	return sqrt(self.x * self.x + self.y * self.y)
end

---Returns the squared magnitude of the vector.
---
---This is far more performant than Vector2.length() and should be used when
---compairing the lengths of two vectors.
---@return number
function Vector2:lengthSquared()
	return self.x * self.x + self.y * self.y
end

---Returns the result of the linear interpolation between this vector (a) and (b) by amount
---`weight`. `wieght` is on the range of 0.0 to 1.0, representing the amount of interpolation.
---@param a Vector2
---@param b Vector2
---@param weight number
---@return Vector2
function Vector2.lerp(a, b, weight)
	local ax, ay, bx, by = a.x, a.y, b.x, b.y
	return new(ax + (bx - ax) * weight, ay + (by - ay) * weight)
end

---Returns the vector with a maximum length by limiting its length to `length`. If the vector
---is non-finite, the result in undefined.
---@param v Vector2
---@param length number
---@return Vector2
function Vector2.limitLength(v, length)
	local len = v:length()
	local l = length or 1
	if len <= l then
		return new(v.x, v.y)
	end
	if len == 0 then
		return new(0, 0)
	end
	local s = l / len
	return new(v.x * s, v.y * s)
end

---Returns the component-wise maximum of this and `with`, equivalent to
---`Vector2(max(a.x, b.x), max(a.y, b.y))`
---@param a Vector2
---@param b number | Vector2
---@return Vector2
function Vector2.max(a, b)
	if type(b) == "number" then
		return new(max(a.x, b), max(a.y, b))
	elseif b and b._isVector then
		return new(max(a.x, b.x), max(a.y, b.y))
	else
		error("\n\nVALUE ERROR:\nmethod `Vector2.max(a, b)` parameter `b` must be type <number> or <Vector2>\n")
	end
end

---@alias axisValues
---|'"x"' The X axis.
---|'"y"' The Y axis.

---Returns the axis of the vector's highest value. If all components are equal, this method returns x.
---@param v Vector2
---@return axisValues
function Vector2.maxAxis(v)
	if v.x >= v.y then
		return "x"
	else
		return "y"
	end
end

---Returns the component-wise minimum of (a) and (b), equivalent to
---Vector2(min(a.x, b.x), min(a.y, b.y))
---@param a Vector2
---@param b Vector2
---@return Vector2
function Vector2.min(a, b)
	return new(min(a.x, b.x), min(a.y, b.y))
end

---Returns the axis of the vector's lowest value. If all components are equal, this method returns y.
---@param v Vector2
---@return axisValues
function Vector2.minAxis(v)
	if v.x < v.y then
		return "x"
	else
		return "y"
	end
end

---Returns a new vector moved toward (b) by the fixed `delta` amount. Will not go past final value.
---@param a Vector2
---@param b Vector2
---@param delta number
---@return Vector2
function Vector2.moveToward(a, b, delta)
	local diff = b - a
	local dist = diff:length()
	if dist <= delta or dist == 0 then
		return Vector2(b.x, b.y)
	end
	return a + Vector2.normalized(diff) * delta
end

---Returns the result of scaling the vector to unit length. Equivalent to vec / vec.length().
---Returns (0, 0) if vec.length() == 0. See also `isNormalized()`.
---@param v Vector2
---@return Vector2
function Vector2.normalized(v)
	local len = v:length()
	if len == 0 then
		return new(0, 0)
	end
	return new(v.x / len, v.y / len)
end

---Returns a perpendicular vector rotated 90 degrees counter-clockwise compared to the
---original, with the same length.
---@param v Vector2
function Vector2.orthogonal(v)
	return new(-v.y, v.x)
end

---Returns a vector composed of the posmod of the vector's components and mod.
---@param v Vector2
---@param mod number
---@return Vector2
function Vector2.posmod(v, mod)
	return new(((v.x % mod) + mod) % mod, ((v.y % mod) + mod) % mod)
end

---Returns a new vector resulting from projecting this vector onto the given vector (b).
---The resulting new vector is parallel to (b). See alse `slide()`.
---@param a Vector2
---@param b Vector2
---@return Vector2
function Vector2.project(a, b)
	local bx, by = b.x, b.y
	local denom = bx * bx + by * by
	if denom == 0 then
		return new(0, 0)
	end
	local s = (a.x * bx + a.y * by) / denom
	return new(bx * s, by * s)
end

---Returns the result of reflecting the vector from a line defined by the given directions
---vector `line`
---@param v Vector2
---@param line Vector2
---@return Vector2
function Vector2.reflect(v, line)
	local lx, ly = line.x, line.y
	local denom = lx * lx + ly * ly
	if denom == 0 then
		return new(-v.x, -v.y)
	end
	local s = (v.x * lx + v.y * ly) / denom
	return new(lx * (2 * s) - v.x, ly * (2 * s) - v.y)
end

---Returns the result of rotating this vector by `angle` (in radians).
---@param v Vector2
---@param angle number
---@return Vector2
function Vector2.rotated(v, angle)
	local c = cos(angle)
	local s = sin(angle)
	return new(v.x * c - v.y * s, v.x * s + v.y * c)
end

local function round_away(n)
	return n >= 0 and floor(n + 0.5) or -floor(-n + 0.5)
end

---Returns a new vector with all components rounded to the nearest interger, with halfwat class
---rounded away from 0.
---@param v Vector2
---@return Vector2
function Vector2.round(v)
	return new(round_away(v.x), round_away(v.y))
end

---Returns the scalar-wise multiplation result.
---@param magnitude number
---@return Vector2
function Vector2:scalar(magnitude)
	return new(self.x * magnitude, self.y * magnitude)
end

---Returns a new vector with each component set to 1.0 if it's positive, -1.0 if it's negative,
---and 0.0 if it's zero.
---@param v Vector2
function Vector2.sign(v)
	local sx = v.x > 0 and 1 or (v.x < 0 and -1 or 0)
	local sy = v.y > 0 and 1 or (v.y < 0 and -1 or 0)
	return new(sx, sy)
end

---Returns the result of spherical linear interpolation between this vector and (b), by the amount
---`weight`. `weight` is on the range of 0.0 to 1.0, representing the amount of interpolation.
---
---This method also handles interpolating the lengths if the input vectors have different
---lengths. For the special case of one or both input vectors having a zero length, this method
---behaves like `lerp()`
---@param a Vector2
---@param b Vector2
---@param weight number
function Vector2.slerp(a, b, weight)
	local lenA = a:length()
	local lenB = b:length()
	if lenA == 0 or lenB == 0 then
		return Vector2.lerp(a, b, weight)
	end
	local ua = a / lenA
	local ub = b / lenB
	local angle = Vector2.angleTo(ua, ub)
	if abs(angle) < 1e-6 then
		return Vector2.lerp(a, b, weight)
	end
	local sinAngle = sin(angle)
	local t1 = sin((1 - weight) * angle) / sinAngle
	local t2 = sin(weight * angle) / sinAngle
	local dir = ua * t1 + ub * t2
	local len = lenA + (lenB - lenA) * weight
	return dir * len
end

---Returns a new vector resulting from sliding this vector along a line with normal. The
---resulting new vector is perpendicular to normal, and is equivalent to this vector minus
---its projection on normal.
---@param v Vector2
---@param normal Vector2
function Vector2.slide(v, normal)
	local nx, ny = normal.x, normal.y
	local vn = (v.x * nx + v.y * ny) / (nx * nx + ny * ny)
	return new(v.x - vn * nx, v.y - vn * ny)
end

---Returns a new vector with each component snapped to the nearest multiple of the corresponding
---component in `step()`. This can also be used to round the components to an arbitrary number
---of decimals.
---@param v Vector2
---@param step number | Vector2
function Vector2.snapped(v, step)
	if type(step) == "number" then
		if step == 0 then
			return new(v.x, v.y)
		end
		local x = floor(v.x / step + 0.5) * step
		local y = floor(v.y / step + 0.5) * step
		return new(x, y)
	elseif step and step._isVector then
		local xStep = step.x
		local yStep = step.y
		local x = xStep ~= 0 and floor(v.x / xStep + 0.5) * xStep or v.x
		local y = yStep ~= 0 and floor(v.y / yStep + 0.5) * yStep or v.y
		return new(x, y)
	else
		error(
			"\n\nVALUE ERROR:\nmethod `Vector2.snapped(v, step)` parameter `step` must be type <number> or <Vector2>\n"
		)
	end
end

function Vector2:sub(otherVector2)
	return new(self.x - otherVector2.x, self.y - otherVector2.y)
end

Vector2.UP = new(0, -1)
Vector2.DOWN = new(0, 1)
Vector2.LEFT = new(-1, 0)
Vector2.RIGHT = new(1, 0)
Vector2.ZERO = new(0, 0)
Vector2.ONE = new(1, 1)
Vector2.INF = new(huge, huge)

return Vector2
