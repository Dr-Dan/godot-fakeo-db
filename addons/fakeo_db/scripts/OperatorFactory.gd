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

static func cmp_func(func_ref: FuncRef):
	return Operators.CmpFunction.new(func_ref)

static func cmp_func_args(func_ref: FuncRef, args):
	return Operators.CmpFunctionWithArgs.new(func_ref, args)

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