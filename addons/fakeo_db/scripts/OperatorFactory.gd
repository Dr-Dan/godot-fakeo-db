extends Resource

const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")
const Op = Operators.OperatorBase
	
static func run(field, op) -> Op:
	return Operators.RunOp.new(field, op)
	
static func comp(ops:Array, exit_op=null, is_predicate=false) -> Op:
	return Operators.OperatorIterator.new(ops, exit_op, is_predicate)

static func value(val) -> Op:
	return Operators.Value.new(val)

static func identity() -> Op:
	return Operators.Identity.new()

static func even() -> Op:
	return Operators.Even.new()
	
static func odd() -> Op:
	return not_(Operators.Even.new())

static func gt(item=0) -> Op:
	return Operators.GT.new(item)
	
static func lt(item=0) -> Op:
	return Operators.LT.new(item)

static func eq(item=0) -> Op:
	return Operators.Eq.new(item)

static func neq(item) -> Op:
	return not_(Operators.Eq.new(item))

static func gteq(item=0) -> Op:
	return or_([gt(item), eq(item)])
	
static func lteq(item=0) -> Op:
	return or_([lt(item), eq(item)])


static func and_(items: Array) -> Op:
	return Operators.And.new(items)

static func or_(items: Array) -> Op:
	return Operators.Or.new(items)

static func not_(item) -> Op:
	return Operators.Not.new(item)


static func in_(item) -> Op:
	return Operators.In.new(item)

static func is_(cls) -> Op:
	return Operators.Is.new(cls)
	
static func is_var(type:int) -> Op:
	return Operators.IsVariantType.new(type)

static func contains(item) -> Op:
	return Operators.Contains.new(item)

static func has(item) -> Op:
	return Operators.HasField.new(item)

static func func_(obj:Object, func_name:String, args=[]) -> Op:
	var fn = funcref(obj, func_name)
	return Operators.Func.new(fn, args)

static func call_fn(func_name:String, args=[], return_item=false) -> Op:
	return Operators.CallFunc.new(func_name, args, return_item)	
	
static func func_as_args(obj:Object, func_name:String) -> Op:
	var fn = funcref(obj, func_name)
	return Operators.FuncAsArgs.new(fn)

static func expr(expr_str:String, fields=null, target=null) -> Op:
	if fields is Dictionary:
		return Operators.ExprArgsDict.new(expr_str, fields, target)
	elif fields is Array:
		return Operators.ExprArgsDeep.new(expr_str, fields, target)
	elif fields is String:
		return Operators.ExprArgsDeep.new(expr_str, [fields], target)
	return Operators.Expr.new(expr_str, target)

static func open_v(fields:Array) -> Op:
	return Operators.OpenValue.new(fields)

static func open(field) -> Op:
	if field is Array:
		return Operators.OpenMultiDeep.new(field)
	elif field is Dictionary:
		return Operators.OpenMultiDeepDict.new(field)
	assert(field is String)
	return Operators.OpenDeep.new(field)
	
static func open_idx(field, defval=null) -> Op:
	if field is Array:
		return Operators.OpenIndexMultiDeep.new(field, defval)
	elif field is String:
		return Operators.OpenIndexDeep.new(field, defval)
	return Operators.OpenIndex.new(field, defval)

# preds: a dictionary in the form {field0=some_op_or_value_to_compare, ...}
# _any: return true if any fields are valid
# _fail_missing: fail if any field not found
static func dict_cmpr(preds:Dictionary, _any=false, _fail_missing=true) -> Op:
	return Operators.DictCompareOpen.new(preds, _any, _fail_missing)

# input: a dictionary as above but only accepting operators as values
# other: the fields to select alongside those stated in input
# open_if_found: if true will pass the value of a field (if it exists)in the object to the op
#   -otherwise the whole object is passed. By default the whole is passed.
static func dict_apply(input:Dictionary, other=[], open_if_found=false) -> Op:
	return Operators.DictApplied.new(input, other, open_if_found)

static func if_(pred, then, else_) -> Op:
	return Operators.RunIf.new(pred, then, else_)
		
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
