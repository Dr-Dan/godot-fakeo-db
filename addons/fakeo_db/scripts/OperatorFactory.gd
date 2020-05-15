extends Resource

const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")

static func gt(item):
	return Operators.GT.new(item)
	
static func lt(item):
	return Operators.LT.new(item)

static func eq(item):
	return Operators.Eq.new(item)

static func gteq(item):
	return or_([gt(item), eq(item)])
	
static func lteq(item):
	return or_([lt(item), eq(item)])

static func and_(items: Array):
	return Operators.And.new(items)

static func or_(items: Array):
	return Operators.Or.new(items)

static func not_(item):
	return Operators.Not.new(item)

static func in_(item):
	return Operators.In.new(item)

static func func_(obj:Object, func_name:String, args=[]):
	var fn = funcref(obj, func_name)
	if args.empty():
		return Operators.FuncOp.new(fn)
	return Operators.FuncOp.new(fn, args)

static func op(item:String, arg):
	match item:
		"<":
			return lt(arg)
		">":
			return gt(arg)
		"=", "==":
			return eq(arg)			
		"and", "&", "&&":
			return and_(arg)
		"or", "|", "||":
			return or_(arg)			
		"not", "!":
			return not_(arg)
		"in":
			return in_(arg)
	return null
