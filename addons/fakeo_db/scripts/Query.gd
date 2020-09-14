extends Resource
class_name Query

const Proc = preload("res://addons/fakeo_db/scripts/Processors.gd")
const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")
const OpBase = Operators.OperatorBase

var items = []

func _init(items_:Array=[]) -> void:
	items = [] + items_

# =======================================================

func proc(item)\
	-> Query:
	return get_script().new(items + [item])

func proc_array(item:Array)\
	-> Query:
	return get_script().new(items + item)

# -------------------------------------------------------

func map(input)\
	-> Query:
	return proc(Proc.MapOp.new(input))

func filter(input)\
	-> Query:
	return proc(Proc.FilterOp.new(input))

func take(amt: int)\
	-> Query:
	return proc(Proc.Take.new(amt))

func take_while(op: OpBase)\
	-> Query:
	return proc(Proc.TakeWhile.new(op))

func skip(amt: int)\
	-> Query:
	return proc(Proc.Skip.new(amt))

# func reduce(op)\
# 	-> Query:
# 	return proc(Proc.IterateOp.new(op))

func as_args(fn:FuncRef)\
	-> Query:
	return map(Operators.FuncAsArgs.new(fn))
