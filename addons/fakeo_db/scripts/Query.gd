extends Resource
class_name Query

const Proc = preload("res://addons/fakeo_db/scripts/Processors.gd")
const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")
const Iterable = preload("res://addons/fakeo_db/scripts/Iterable.gd")

const OpBase = Operators.OperatorBase

var items = []

func _init(items_:Array=[]) -> void:
	items = [] + items_

func to_iter(coll=[]) -> Iterable:
	return Iterable.new(items, coll)

# func from_iter(iter: Iterable)\
# 	-> Query:
# 	return get_script().new(iter.query)
	
func apply(coll: Array):
	return to_iter(coll).run()

# =======================================================
	
func proc(item)\
	-> Query:
	return get_script().new(items + [item])
	
func proc_array(item:Array)\
	-> Query:
	return get_script().new(items + item)

# -------------------------------------------------------

func filter(input, args:Array=[])\
	-> Query:
	return proc(Proc.FilterOpAuto.new(input, args))

func take(amt: int)\
	-> Query:
	return proc(Proc.Take.new(amt))

func take_while(op: OpBase)\
	-> Query:
	return proc(Proc.TakeWhile.new(op))

func skip(amt: int)\
	-> Query:
	return proc(Proc.Skip.new(amt))

func map(input, args:Array=[])\
	-> Query:
	return proc(Proc.MapOpAuto.new(input, args))

func as_args(fn:FuncRef)\
	-> Query:
	return map(Operators.FuncAsArgs.new(fn))

# =======================================================
## TODO: call when query changed
##func _on_item_added(e):
##	pass
#
