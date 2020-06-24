extends "Enumerables.gd".StepEnumerable
# extends Resource
const Enumerables = preload("res://addons/fakeo_db/scripts/Enumerables.gd")
const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")

var start
# var next

func _init(data=[]).([]):
	if data is Enumerables.StepEnumerable:
		start = data
	else:
		start = Enumerables.StepEnumerable.new(data)
	source = start
		
func set_root(data):
	assert(start.set_source(data) != null)
	return self

func branch():
	var d = get_script().new(source)
	return d
	
func dup(source_=null):
	if source_ == null: source_ = source
	var d = get_script().new(start)
	d.source = source_
	return d
# ===============================================================================
# Instant evaluation

	# get first in enumerable that satisfies conditions
func first(cmps):
	return where(cmps).take(1).at(0)

func last(cmps):
	var result = where(cmps).to_array()
	if result.empty():
		return null
	return result[result.size()-1]
	
# returns true if any match conditions
func exists(cmps):
	var d = first(cmps)
	return d != null

func for_each(fn:FuncRef, args:Array=[]):
	return select(fn, args).to_array()

func for_each_op(op):
	return select_op(op).source.to_array()

func for_each_expr(op, arg=null, target=null):
	if arg is Dictionary:
		return for_each_op(Operators.ExprArgsDict.new(op, arg, target))
	elif arg is Array:
		return for_each_op(Operators.ExprArgsDeep.new(op, arg, target))
		
	return for_each_op(Operators.Expr.new(op, target))
# ===============================================================================
# Deferred

func where(preds):
	return dup(get_where_type(source, preds))

func where_op(op):
	return dup(Enumerables.WhereOp.new(source, op))

func where_fn(fn:FuncRef, args:Array=[]):
	return where_op(Operators.Func.new(fn, args))
	
func where_expr(expr: String, expr_vars=[]):
	return where_op(Operators.ExprArgs.new(expr, expr_vars))
		
func take(amt):
	return dup(Enumerables.Take.new(source, amt))

func skip(amt):
	return dup(Enumerables.Skip.new(source, amt))

func select(op, args=[]):
	return dup(get_select_type(source, op, args))

func select_op(op):
	return dup(Enumerables.SelectOp.new(source, op))

func select_expr(expr, expr_vars=[]):
	return select_op(Operators.ExprArgs.new(expr, expr_vars))

func select_fn(fn:FuncRef, args:Array=[]):
	return select_op(Operators.Func.new(fn, args))

func project(fields:Array):
	return dup(Enumerables.Project.new(source, fields))

func project_dp(fields:Array):
	return dup(Enumerables.ProjectDeep.new(source, fields))

func as_args(fn:FuncRef):
	return dup(select_op(Operators.FuncAsArgs.new(fn)))


# =======================================================
# TODO: call when query changed
#func _on_item_added(e):
#	pass

# =======================================================
# UTILS
static func get_select_type(source, op, args=[]):
	if op is Operators.OperatorBase:
		return Enumerables.SelectOp.new(source, op)
	elif op is String:
		return Enumerables.SelectOp.new(source, Operators.ExprArgs.new(op, args))
	elif op is FuncRef:
		return Enumerables.SelectOp.new(source, Operators.Func.new(op, args))
	return null

static func get_where_type(source, preds):
	if preds is Operators.OperatorBase:
		return Enumerables.WhereOp.new(source, preds)
	elif preds is FuncRef:
		return Enumerables.WhereOp.new(source, Operators.Func.new(preds))
	elif preds is String:
		return Enumerables.WhereOp.new(source, Operators.Expr.new(preds))
	elif preds is Dictionary:
		return  Enumerables.WhereOp.new(source, Operators.DictCompare.new(preds))
	return null
	
