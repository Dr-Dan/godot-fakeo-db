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
	return Operators.Func.new(fn, args)

static func func_as_args(obj:Object, func_name:String):
	var fn = funcref(obj, func_name)
	return Operators.FuncAsArgs.new(fn)


static func expr(expr_str:String, fields=null, target=null):
	if fields is Dictionary:
		return Operators.ExprArgsDict.new(expr_str, fields, target)
	elif fields is Array:
		return Operators.ExprArgs.new(expr_str, fields, target)
		
#	if fields == null:
	return Operators.Expr.new(expr_str, target)
#	return Operators.ExprArgs.new(expr_str, fields, target)

static func expr_dp(expr_str:String, fields:Array, target=null):
	return Operators.ExprArgsDeep.new(expr_str, fields, target)
	
static func open(field):
	if field is Array:
		return Operators.OpenMulti.new(field)
	return Operators.Open.new(field)
	
static func open_dp(field):
	if field is Array:
		return Operators.OpenMultiDeep.new(field)
	return Operators.OpenDeep.new(field)

static func dict(preds, _any=false, _fail_missing=true):
	return Operators.DictCompare.new(preds, _any, _fail_missing)

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
