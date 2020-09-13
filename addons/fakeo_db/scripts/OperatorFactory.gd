extends Resource

const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")

static func comp(ops:Array, exit_op=null, is_predicate=false):
	return Operators.OperatorIterator.new(ops, exit_op, is_predicate)

static func value(val):
	return Operators.Value.new(val)

static func even():
	return Operators.Even.new()
	
# static func gt(item0, item1=null):
# 	if item1:
# 		return Operators.OpCaller2(Operators.GT, item0, item1)
# 	else:
# 		return Operators.OpCaller1(Operators.GT, item0)
	
static func gt(item):
	return Operators.GT.new(item)
	
static func lt(item):
	return Operators.LT.new(item)

static func eq(item):
	return Operators.Eq.new(item)

static func neq(item):
	return not_(Operators.Eq.new(item))

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

static func is_(type):
	return Operators.Is.new(type)
	
static func is_var(type:int):
	return Operators.IsVariantType.new(type)

static func contains(item):
	return Operators.Contains.new(item)

static func has(item):
	return Operators.HasField.new(item)

static func func_(obj:Object, func_name:String, args=[]):
	var fn = funcref(obj, func_name)
	return Operators.Func.new(fn, args)

static func func_as_args(obj:Object, func_name:String):
	var fn = funcref(obj, func_name)
	return Operators.FuncAsArgs.new(fn)

# this is just and?!
#static func if_then(if_op, then_op):
#	var o = [if_op]
#	if then_op is Array:
#		for to in then_op:
#			o.append(to)
#	else: o.append(then_op)
#	return and_(o)

# should do while something in item is true
#static func do_while(ops:Array, exit_op=null):
#	return op_iter(ops, exit_op)

static func expr(expr_str:String, fields=null, target=null):
	if fields is Dictionary:
		return Operators.ExprArgsDict.new(expr_str, fields, target)
	elif fields is Array:
		return Operators.ExprArgsDeep.new(expr_str, fields, target)
	return Operators.Expr.new(expr_str, target)

# static func expr_dp(expr_str:String, fields:Array, target=null):
# 	return Operators.ExprArgsDeep.new(expr_str, fields, target)


static func open(field):
	if field is Array:
		return Operators.OpenMultiDeep.new(field)
	return Operators.OpenDeep.new(field)
	
# static func open_dp(field):
# 	if field is Array:
# 		return Operators.OpenMultiDeep.new(field)
# 	return Operators.OpenDeep.new(field)

static func dict(preds, _any=false, _fail_missing=true):
	return Operators.DictCompare.new(preds, _any, _fail_missing)
	
static func dict_apply(input, args=[]):
	return Operators.DictApplied.new(input, args)	

static func op(item:String, arg):
	match item:
		"<":
			return lt(arg)
		">":
			return gt(arg)
		"<=":
			return lteq(arg)
		">=":
			return gteq(arg)
		"=", "==":
			return eq(arg)
		"!=":
			return neq(arg)			
		"and", "&", "&&":
			return and_(arg)
		"or", "|", "||":
			return or_(arg)
		"not", "!":
			return not_(arg)
		"in":
			return in_(arg)
	return null
